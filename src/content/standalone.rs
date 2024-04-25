use eyre::{eyre, Result};
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, collections::HashSet};
use tera::Context;

use crate::markup::{find_markup_files, Html, MarkupLookup, ParseContext, RawMarkupFile};
use crate::{item::Item, item::RenderContext, item::TeraItem, paths::AbsPath, site_url::SiteUrl};

pub fn load_standalones(dir: AbsPath, create_lookup: bool) -> Result<HashSet<StandaloneItem>> {
    find_markup_files(&[dir])
        .into_iter()
        .map(|path| StandaloneItem::from_file(path.abs_path(), create_lookup))
        .collect::<Result<HashSet<StandaloneItem>>>()
}

#[derive(Debug)]
pub struct StandaloneItem {
    pub title: String,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub content: Html,
    pub markup_lookup: Option<MarkupLookup>,
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
    pub fn from_file(path: AbsPath, create_lookup: bool) -> Result<Self> {
        let markup = RawMarkupFile::from_file(path)?;
        Self::from_markup(markup, create_lookup)
    }

    pub fn from_markup(
        markup: RawMarkupFile<StandaloneMetadata>,
        create_lookup: bool,
    ) -> Result<Self> {
        let meta_line_count = markup.meta_line_count;
        let markup = markup.parse(ParseContext::new(create_lookup, meta_line_count))?;
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
            markup_lookup: markup.markup_lookup,
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

    fn tera_url(&self) -> &SiteUrl {
        &self.url
    }

    fn tera_source_file(&self) -> Option<&AbsPath> {
        Some(&self.path)
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
