use camino::Utf8Path;
use camino::Utf8PathBuf;
use eyre::eyre;
use eyre::Result;
use std::env;
use std::fmt::Display;
use std::fs::Metadata;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;
use tempfile::TempDir;
use walkdir::WalkDir;

pub struct WalkDirRes {
    pub meta: Metadata,
    pub path: FilePath,
}

pub fn walk_dir(dir: AbsPath) -> impl Iterator<Item = WalkDirRes> + 'static {
    WalkDir::new(dir.as_std_path())
        .into_iter()
        .filter_map(|e| e.ok())
        .filter_map(move |e| match e.metadata() {
            Ok(meta) => FilePath::from_std_path(&dir, e.into_path())
                .ok()
                .and_then(|path| Some(WalkDirRes { meta, path })),
            Err(_) => None,
        })
}

pub fn file_iter(dir: AbsPath) -> impl Iterator<Item = WalkDirRes> + 'static {
    walk_dir(dir).filter(|e| !e.meta.is_dir())
}

pub fn list_files(dir: AbsPath) -> Vec<FilePath> {
    file_iter(dir).map(|e| e.path).collect()
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FilePath {
    pub base: Utf8PathBuf,
    pub rel_path: RelPath,
}

impl FilePath {
    pub fn from_std_path(base: impl Into<Utf8PathBuf>, path: PathBuf) -> Result<Self> {
        let path =
            Utf8PathBuf::from_path_buf(path).map_err(|path| eyre!("Non-utf8 path: {:?}", path))?;

        Self::from_path(base, &path)
    }

    pub fn from_path(base: impl Into<Utf8PathBuf>, path: impl AsRef<Utf8Path>) -> Result<Self> {
        let base = base.into();
        let rel = path.as_ref().strip_prefix(&base)?;

        Ok(Self {
            base,
            rel_path: RelPath(rel.into()),
        })
    }

    pub fn abs_path(&self) -> AbsPath {
        AbsPath(self.base.join(&self.rel_path.0))
    }

    pub fn file_name(&self) -> &str {
        self.rel_path.0.file_name().expect(&format!(
            "FilePath without a file_name: `{}`",
            self.rel_path.0
        ))
    }
}

impl Display for FilePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.abs_path().fmt(f)
    }
}

/// Deref etc aren't implemented to avoid mixing with absolute paths.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RelPath(pub Utf8PathBuf);

impl<T> PartialEq<T> for RelPath
where
    T: AsRef<str> + ?Sized,
{
    fn eq(&self, other: &T) -> bool {
        self.0.eq(other.as_ref())
    }
}

impl Display for RelPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl RelPath {
    pub fn starts_with(&self, prefix: &str) -> bool {
        self.0.as_str().starts_with(prefix)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AbsPath(pub Utf8PathBuf);

impl AbsPath {
    pub fn new_tempdir() -> Result<(TempDir, Self)> {
        let dir = tempfile::tempdir()?;
        let path = Self::from_path_buf(dir.path().to_owned());
        Ok((dir, path))
    }

    pub fn current_dir() -> Result<Self> {
        let path = env::current_dir()?;
        Ok(Self::from_path_buf(path))
    }

    pub fn from_path_buf(path: PathBuf) -> Self {
        Self(Utf8PathBuf::from_path_buf(path).expect("Non-utf8 path"))
    }

    pub fn join(&self, path: impl AsRef<Utf8Path>) -> AbsPath {
        self.0.join(path).into()
    }
}

impl From<&AbsPath> for Utf8PathBuf {
    fn from(path: &AbsPath) -> Self {
        path.0.clone()
    }
}

impl From<&str> for AbsPath {
    fn from(path: &str) -> Self {
        Self(path.into())
    }
}

impl From<Utf8PathBuf> for AbsPath {
    fn from(path: Utf8PathBuf) -> Self {
        Self(path)
    }
}

impl Display for AbsPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl AsRef<Utf8Path> for AbsPath {
    fn as_ref(&self) -> &Utf8Path {
        self.0.as_path()
    }
}

impl AsRef<Path> for AbsPath {
    fn as_ref(&self) -> &Path {
        self.0.as_std_path()
    }
}

impl Deref for AbsPath {
    type Target = Utf8PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
