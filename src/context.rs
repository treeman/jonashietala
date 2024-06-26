use crate::site::{SiteContent, SiteOptions};
use crate::{git::LatestCommitInfo, git::LatestCommits, paths::FilePath};

use camino::Utf8Path;
use tera::{Context, Tera};

pub struct LoadContext<'a> {
    pub opts: &'a SiteOptions,
    pub latest_commits: &'a LatestCommits,
}

impl<'a> LoadContext<'a> {
    pub fn get_commit(&self, path: &FilePath) -> Option<&'a LatestCommitInfo> {
        self.latest_commits.get(path)
    }
}

pub struct RenderContext<'a> {
    pub output_dir: &'a Utf8Path,
    pub parent_context: &'a Context,
    pub content: &'a SiteContent,
    pub tera: &'a Tera,
}
