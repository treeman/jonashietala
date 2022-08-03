use eyre::{eyre, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::{borrow::Cow, collections::HashSet};
use tera::Context;
use yaml_front_matter::{Document, YamlFrontMatter};

use crate::{
    item::Item, item::RenderContext, item::TeraItem, markdown::find_markdown_files,
    markdown::markdown_to_html, paths::AbsPath, site_url::SiteUrl,
};

pub fn load_standalones(dir: AbsPath) -> Result<HashSet<StandaloneItem>> {
    find_markdown_files(dir)
        .into_iter()
        .map(|path| StandaloneItem::from_file(path.abs_path()))
        .collect::<Result<HashSet<StandaloneItem>>>()
}

#[derive(Debug)]
pub struct StandaloneItem {
    pub title: String,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub content: String,
}

impl PartialEq for StandaloneItem {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for StandaloneItem {}

impl std::hash::Hash for StandaloneItem {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl StandaloneItem {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let raw_content = fs::read_to_string(&path)?;
        Self::from_string(path, raw_content)
    }

    pub fn from_string(path: AbsPath, raw_content: String) -> Result<Self> {
        let slug = path
            .file_stem()
            .ok_or_else(|| eyre!("Missing file stem: {path}"))?
            .to_string();

        let Document { metadata, content } =
            YamlFrontMatter::parse::<StandaloneMetadata>(&raw_content)
                .map_err(|err| eyre!("Failed to parse metadata for post: {:#?}\n{}", path, err))?;

        let url = SiteUrl::parse(&format!("/{slug}/")).expect("Should be able to create a url");

        Ok(Self {
            title: metadata.title,
            path,
            url,
            content,
        })
    }
}

impl TeraItem for StandaloneItem {
    fn context(&self, _ctx: &RenderContext) -> Context {
        Context::from_serialize(StandaloneContext {
            title: html_escape::encode_text(&self.title),
            content: &markdown_to_html(&self.content),
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "static.html"
    }

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Debug, Clone, Serialize)]
struct StandaloneContext<'a> {
    title: Cow<'a, str>,
    content: &'a str,
}

#[derive(Deserialize, Debug)]
struct StandaloneMetadata {
    title: String,
}
