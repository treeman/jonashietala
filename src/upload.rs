use axum::http::HeaderMap;
use axum::http::HeaderValue;
use eyre::eyre;
use eyre::Result;
use futures::future;
use lazy_static::lazy_static;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;
use regex::Regex;
use reqwest::header::HeaderName;
use reqwest::header::CACHE_CONTROL;
use s3::bucket::Bucket;
use s3::serde_types::Object;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::os::unix::fs::MetadataExt;
use tracing::{debug, error, info};
use walkdir::WalkDir;

use crate::paths::AbsPath;
use crate::paths::FilePath;

pub struct SyncOpts<'a> {
    pub dir: &'a AbsPath,
    pub bucket: Bucket,
    pub delete: bool,
    pub print_urls: bool,
}

pub async fn sync(opts: SyncOpts<'_>) -> Result<()> {
    let plan = calculate_sync(opts.dir, &opts.bucket).await?;

    let mut futures = Vec::new();
    for x in plan.upload.into_values() {
        if opts.print_urls {
            info!("Creating {}/{}", &opts.bucket.url(), x.file_path.rel_path);
        }
        futures.push(sync_ref(&opts.bucket, SyncRef::Upload(x)));
    }
    if opts.delete {
        for x in plan.delete.into_iter() {
            futures.push(sync_ref(&opts.bucket, SyncRef::Delete(x)));
        }
    }

    let res = future::join_all(futures).await;
    for response in res {
        if let Err(err) = response {
            error!("Failed to sync: `{}", err);
        }
    }

    Ok(())
}

async fn calculate_sync(dir: &AbsPath, bucket: &Bucket) -> Result<SyncPlan> {
    // FIXME this should be a standard function
    let local: Vec<FilePath> = WalkDir::new(dir.as_std_path())
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| match e.metadata() {
            Ok(e) => !e.is_dir(),
            _ => false,
        })
        // .filter_map(|e| Utf8PathBuf::from_path_buf(e.into_path()).ok())
        .filter_map(|e| FilePath::from_std_path(dir, e.into_path()).ok())
        .collect();

    let remote: HashMap<String, Object> = bucket
        .list("".to_string(), None)
        .await?
        .into_par_iter()
        .flat_map(|res| {
            res.contents
                .into_par_iter()
                .map(|obj| (obj.key.clone(), obj))
        })
        .collect();

    SyncPlan::decide(local, remote)
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct SyncPlan {
    upload: HashMap<String, UploadData>,
    delete: HashSet<String>,
}

impl SyncPlan {
    fn decide(local_paths: Vec<FilePath>, remote_objs: HashMap<String, Object>) -> Result<Self> {
        let rel_local_paths: HashSet<String> = local_paths
            .iter()
            .map(|file| file.rel_path.to_string())
            .collect();

        let upload: HashMap<_, _> = local_paths
            .into_par_iter()
            .filter_map(|local_path| {
                let key = &local_path.rel_path.0;
                match UploadPlan::decide(local_path.abs_path(), remote_objs.get(key.as_str()))
                    .unwrap()
                {
                    UploadPlan::WithContent(content) => Some((
                        key.to_string(),
                        UploadData {
                            file_path: local_path,
                            content: Some(content),
                        },
                    )),
                    UploadPlan::Yes => Some((
                        key.to_string(),
                        UploadData {
                            file_path: local_path,
                            content: None,
                        },
                    )),
                    UploadPlan::No => None,
                }
            })
            .collect();

        let delete: HashSet<String> = remote_objs
            .into_iter()
            .filter_map(|(remote_path, _)| {
                if rel_local_paths.contains(&remote_path) {
                    None
                } else {
                    Some(remote_path)
                }
            })
            .collect();

        Ok(Self { upload, delete })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct UploadData {
    file_path: FilePath,
    content: Option<Vec<u8>>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum UploadPlan {
    No,
    Yes,
    WithContent(Vec<u8>),
}

impl UploadPlan {
    /// Decide if we should upload a file.
    ///
    /// Upload a file if (in this order):
    /// - There is no remote file
    /// - Theis size is different
    /// - There is no MD5 digest one the remote
    /// - Their MD5 digest differs
    ///
    /// Ignore last modified since we're always regenerating files locally.
    fn decide(path: AbsPath, s3_obj: Option<&Object>) -> Result<Self> {
        let s3_obj = if let Some(x) = s3_obj {
            x
        } else {
            return Ok(Self::Yes);
        };

        let meta = fs::metadata(&path)?;
        if meta.size() != s3_obj.size {
            return Ok(Self::Yes);
        }

        // MD5 tag comes with extra "" quotes.
        lazy_static! {
            static ref MD5: Regex = Regex::new(r#""([a-f0-9]+)""#).unwrap();
        }

        let s3_digest = if let Some(ref tag) = s3_obj.e_tag {
            let captures = MD5
                .captures(tag)
                .ok_or_else(|| eyre!("Expected MD5 sum, but got `{}`", tag))?;
            captures[1].to_string()
        } else {
            return Ok(Self::Yes);
        };
        let content = fs::read(path)?;
        let digest = format!("{:x}", md5::compute(&content));

        if s3_digest.as_str() == digest {
            debug!("Skip {}", s3_obj.key);
            Ok(Self::No)
        } else {
            Ok(Self::WithContent(content))
        }
    }
}

enum SyncRef {
    Upload(UploadData),
    Delete(String),
}

async fn sync_ref(bucket: &Bucket, x: SyncRef) -> Result<()> {
    match x {
        SyncRef::Upload(x) => upload(bucket, x).await,
        SyncRef::Delete(x) => delete(bucket, x).await,
    }
}

async fn upload(bucket: &Bucket, data: UploadData) -> Result<()> {
    let key = &data.file_path.rel_path.0;
    let ty = new_mime_guess::from_path(key).first_or_text_plain();
    let bucket = bucket_with_headers(bucket, key.as_str());

    let content = if let Some(x) = data.content {
        x
    } else {
        let path = data.file_path.abs_path();
        fs::read(&path)?
    };

    bucket
        // Need to specify content type here, otherwise it's overwritten to octet stream.
        .put_object_with_content_type(key, &content, &ty.to_string())
        .await?;
    info!("Uploaded: {key}");
    Ok(())
}

async fn delete(bucket: &Bucket, key: String) -> Result<()> {
    let bucket = bucket_with_headers(bucket, &key);
    bucket.delete_object(&key).await?;
    info!("Deleted: {key}");
    Ok(())
}

fn bucket_with_headers(bucket: &Bucket, path: &str) -> Bucket {
    let mut headers = HeaderMap::new();
    headers.insert(
        HeaderName::from_static("x-amz-acl"),
        HeaderValue::from_static("public-read"),
    );

    if path.starts_with("images/") || path.starts_with("fonts/") || path.starts_with("files/") {
        headers.insert(CACHE_CONTROL, HeaderValue::from_static("max-age=86400"));
    } else {
        headers.insert(
            CACHE_CONTROL,
            HeaderValue::from_static("max-age=60, must-revalidate"),
        );
    }

    bucket.clone().with_extra_headers(headers)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::paths::RelPath;
    use camino::Utf8PathBuf;
    use std::str::FromStr;

    #[test]
    fn test_should_upload() {
        // Upload if it doesn't exist
        assert_eq!(
            UploadPlan::decide("some_file".into(), None).unwrap(),
            UploadPlan::Yes
        );

        // Size is different
        assert_eq!(
            UploadPlan::decide(
                "test-site/about.markdown".into(),
                Some(&Object {
                    last_modified: "x".to_string(),
                    e_tag: None,
                    storage_class: None,
                    key: "test-site/about.markdown".to_string(),
                    owner: None,
                    size: 1337,
                })
            )
            .unwrap(),
            UploadPlan::Yes
        );

        // Missing MD5 hash
        assert_eq!(
            UploadPlan::decide(
                "test-site/about.markdown".into(),
                Some(&Object {
                    last_modified: "x".to_string(),
                    e_tag: None,
                    storage_class: None,
                    key: "test-site/about.markdown".to_string(),
                    owner: None,
                    size: 38,
                })
            )
            .unwrap(),
            UploadPlan::Yes
        );

        // MD5 hash is different
        assert!(matches!(
            UploadPlan::decide(
                "test-site/about.markdown".into(),
                Some(&Object {
                    last_modified: "x".to_string(),
                    e_tag: Some("\"c429f375574cafb28edd6a4164f00e81\"".to_string()),
                    storage_class: None,
                    key: "test-site/about.markdown".to_string(),
                    owner: None,
                    size: 38,
                })
            )
            .unwrap(),
            UploadPlan::WithContent(_)
        ));

        // MD5 (and others) match
        assert_eq!(
            UploadPlan::decide(
                "test-site/about.markdown".into(),
                Some(&Object {
                    last_modified: "x".to_string(),
                    e_tag: Some("\"7f7369324a747030946a495111490185\"".to_string()),
                    storage_class: None,
                    key: "test-site/about.markdown".to_string(),
                    owner: None,
                    size: 38,
                })
            )
            .unwrap(),
            UploadPlan::No
        );
    }

    #[test]
    fn test_sync_plan() {
        let current_dir = AbsPath::current_dir().unwrap();
        let local = [
            "test-site/about.markdown",
            "test-site/projects.markdown",
            "test-site/static/404.markdown",
        ]
        .into_iter()
        .map(|x| FilePath {
            base: current_dir.0.clone(),
            rel_path: RelPath(Utf8PathBuf::from_str(x).unwrap()),
        })
        .collect();

        let remote = [
            // This should match the local file.
            (
                "test-site/about.markdown".to_string(),
                Object {
                    last_modified: "x".to_string(),
                    e_tag: Some("\"7f7369324a747030946a495111490185\"".to_string()),
                    storage_class: None,
                    key: "test-site/about.markdown".to_string(),
                    owner: None,
                    size: 38,
                },
            ),
            // This should not match the local file.
            (
                "test-site/projects.markdown".to_string(),
                Object {
                    last_modified: "x".to_string(),
                    e_tag: Some("\"7f7369324a747030946a495111490185\"".to_string()),
                    storage_class: None,
                    key: "test-site/projects.markdown".to_string(),
                    owner: None,
                    size: 38,
                },
            ),
            // This exists remotely but not locally.
            (
                "some-remote-thing".to_string(),
                Object {
                    last_modified: "x".to_string(),
                    e_tag: None,
                    storage_class: None,
                    key: "some-remote-thing".to_string(),
                    owner: None,
                    size: 1337,
                },
            ),
            // 404 does not exist remotely.
        ]
        .into_iter()
        .collect();

        let base = Utf8PathBuf::from_str(current_dir.0.as_str()).unwrap();

        let got = SyncPlan::decide(local, remote).unwrap();
        let expected = SyncPlan {
            upload: [
                UploadData {
                    file_path: FilePath {
                        base: base.clone(),
                        rel_path: RelPath("test-site/projects.markdown".into()),
                    },
                    content: None,
                },
                UploadData {
                    file_path: FilePath {
                        base,
                        rel_path: RelPath("test-site/static/404.markdown".into()),
                    },
                    content: None,
                },
            ]
            .into_iter()
            .map(|data| (data.file_path.rel_path.to_string(), data))
            .collect(),
            delete: ["some-remote-thing"].into_iter().map(From::from).collect(),
        };

        assert_eq!(got, expected);
    }

    #[test]
    fn test_css_ty() {
        let ty = new_mime_guess::from_path("main.css").first_or_text_plain();
        assert_eq!(ty, "text/css");
    }
}
