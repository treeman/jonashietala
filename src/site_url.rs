#![allow(dead_code)]

use crate::site::BASE_SITE_URL;
use camino::{Utf8Path, Utf8PathBuf};
use eyre::{eyre, Result};
use std::borrow::Cow;
use url::Url;

#[derive(Debug, Clone, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub enum HrefUrl {
    Fragment(String),
    Internal(SiteUrl),
    External(Url),
}

impl HrefUrl {
    pub fn parse(s: &str) -> Result<Self> {
        if s.starts_with('#') {
            Ok(Self::Fragment(s.to_string()))
        } else if s.starts_with('/') {
            Ok(Self::Internal(SiteUrl::parse(s).map_err(|err| {
                eyre!("Failed to parse site url {s}, {err}")
            })?))
        } else {
            Ok(Self::External(Url::parse(s).map_err(|err| {
                eyre!("Failed to parse regular url {s}, {err}")
            })?))
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub enum ImgUrl {
    Internal(SiteUrl),
    External(Url),
}

impl ImgUrl {
    pub fn parse(s: &str) -> Result<Self> {
        if s.starts_with('/') {
            Ok(Self::Internal(SiteUrl::parse(s).map_err(|err| {
                eyre!("Failed to parse site url {s}, {err}")
            })?))
        } else {
            Ok(Self::External(Url::parse(s).map_err(|err| {
                eyre!("Failed to parse regular url {s}, {err}")
            })?))
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct SiteUrl {
    pub url: Url,
}

impl SiteUrl {
    pub fn parse(s: &str) -> Result<Self> {
        let url = BASE_SITE_URL.join(s)?;
        Ok(SiteUrl { url })
    }

    pub fn path(&self) -> &str {
        self.url.path()
    }

    pub fn href(&self) -> Cow<str> {
        let stripped = self
            .url
            .as_str()
            .strip_prefix(BASE_SITE_URL.as_str())
            .unwrap()
            .trim_end_matches('/');

        if stripped.starts_with('#') {
            Cow::Borrowed(stripped)
        } else {
            Cow::Owned(format!("/{stripped}"))
        }
    }

    pub fn fragment(&self) -> Option<&str> {
        self.url.fragment()
    }

    pub fn output_file(&self, output_dir: &Utf8Path) -> Utf8PathBuf {
        let mut path = output_dir.to_owned();
        path.push(Utf8Path::new(self.url.path()).strip_prefix("/").unwrap());

        // Don't add index to urls like "feed.xml"
        if path.extension().is_none() {
            path.push("index.html");
        }

        path
    }

    pub fn is_img(&self) -> bool {
        self.href().starts_with("/images")
    }
    pub fn is_feed(&self) -> bool {
        self.href() == "/feed.xml"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_site_url() {
        let url = SiteUrl::parse("/blog/2022/01/31/test_post#top").unwrap();
        assert_eq!(url.path(), "/blog/2022/01/31/test_post");
        assert_eq!(url.href(), "/blog/2022/01/31/test_post#top");

        let url = SiteUrl::parse("#top").unwrap();
        assert_eq!(url.path(), "/");
        assert_eq!(url.href(), "#top");
    }

    #[test]
    fn test_output_file_from_url() -> Result<()> {
        let tests = [
            ("/blog", ".output/blog/index.html"),
            ("/blog/", ".output/blog/index.html"),
            (
                "/blog/2022/01/01/my_post/",
                ".output/blog/2022/01/01/my_post/index.html",
            ),
            ("/feed.xml", ".output/feed.xml"),
            ("/post#tag", ".output/post/index.html"),
            (
                "/blog/2022/01/01/my_post#fragment",
                ".output/blog/2022/01/01/my_post/index.html",
            ),
        ];
        let output_dir = Utf8Path::new(".output");
        for (url, expected) in tests {
            assert_eq!(SiteUrl::parse(url)?.output_file(output_dir), expected);
        }

        Ok(())
    }
}
