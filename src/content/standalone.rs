use eyre::{eyre, Result};
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, collections::HashSet};
use tera::Context;

use crate::markup::{find_markup_files, Html, RawMarkupFile};
use crate::{item::Item, item::RenderContext, item::TeraItem, paths::AbsPath, site_url::SiteUrl};

pub fn load_standalones(dir: AbsPath) -> Result<HashSet<StandaloneItem>> {
    find_markup_files(&[dir])
        .into_iter()
        .map(|path| StandaloneItem::from_file(path.abs_path()))
        .collect::<Result<HashSet<StandaloneItem>>>()
}

#[derive(Debug)]
pub struct StandaloneItem {
    pub title: String,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub content: Html,
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
        let markup = RawMarkupFile::from_file(path)?;
        Self::from_markup(markup)
    }

    pub fn from_markup(markup: RawMarkupFile<StandaloneMetadata>) -> Result<Self> {
        let markup = markup.parse()?;
        let slug = markup
            .path
            .file_stem()
            .ok_or_else(|| eyre!("Missing file stem: {}", markup.path))?
            .to_string();

        let url = SiteUrl::parse(&format!("/{slug}/")).expect("Should be able to create a url");

        Ok(Self {
            title: markup.markup_meta.title,
            path: markup.path,
            url,
            content: markup.html,
        })
    }
}

impl TeraItem for StandaloneItem {
    fn context(&self, _ctx: &RenderContext) -> Context {
        Context::from_serialize(StandaloneContext {
            title: html_escape::encode_text(&self.title),
            content: &self.content.0,
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
pub struct StandaloneMetadata {
    title: String,
}
