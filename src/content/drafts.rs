use chrono::NaiveDateTime;
use eyre::{eyre, Result};
use itemref_derive::ItemRef;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fs;
use tera::Context;
use yaml_front_matter::{Document, YamlFrontMatter};

use super::tags::{Tag, TagPostContext, TagsMeta};
use crate::item::Item;
use crate::item::RenderContext;
use crate::markdown::find_markdown_files;
use crate::markdown::markdown_to_html;
use crate::paths::AbsPath;
use crate::{item::TeraItem, site_url::SiteUrl, util};

pub fn load_drafts(dir: AbsPath) -> Result<BTreeMap<DraftRef, DraftItem>> {
    let drafts = find_markdown_files(dir)
        .into_iter()
        .map(|path| DraftItem::from_file(path.abs_path()).map(|draft| (draft.draft_ref(), draft)))
        .collect::<Result<BTreeMap<DraftRef, DraftItem>>>()?;
    Ok(drafts)
}

#[derive(ItemRef, Debug, Clone)]
#[item(DraftItem)]
pub struct DraftRef {
    #[order]
    pub id: String,
}

#[derive(Debug)]
pub struct DraftItem {
    pub title: String,
    pub tags: Vec<Tag>,
    pub modified: NaiveDateTime,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub content: String,
}

impl DraftItem {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let modified = util::last_modified(&path)?;
        let raw_content = fs::read_to_string(&*path)?;
        Self::from_string(path, raw_content, modified)
    }

    pub fn from_string(
        path: AbsPath,
        raw_content: String,
        modified: NaiveDateTime,
    ) -> Result<Self> {
        let slug = path
            .file_stem()
            .ok_or_else(|| eyre!("Missing file stem: {path}"))?
            .to_string();

        let Document { metadata, content } = YamlFrontMatter::parse::<DraftMetadata>(&raw_content)
            .map_err(|err| eyre!("Failed to parse metadata for draft: {:#?}\n{}", path, err))?;

        let url =
            SiteUrl::parse(&format!("/drafts/{slug}/")).expect("Should be able to create a url");

        Ok(Self {
            title: metadata.title,
            tags: metadata.tags.into(),
            modified,
            path,
            url,
            content,
        })
    }

    fn draft_context<'a>(&'a self, _ctx: &RenderContext) -> DraftContext<'a> {
        DraftContext {
            title: html_escape::encode_text(&self.title),
            url: self.url.href(),
            content: markdown_to_html(&self.content),
            tags: self.tags.iter().map(TagPostContext::from).collect(),
        }
    }

    fn draft_ref_context(&self) -> DraftRefContext {
        DraftRefContext {
            title: html_escape::encode_text(&self.title),
            url: self.url.href(),
            tags: self.tags.iter().map(TagPostContext::from).collect(),
        }
    }

    pub fn draft_ref(&self) -> DraftRef {
        DraftRef {
            id: self.id().to_string(),
        }
    }
}

impl TeraItem for DraftItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        Context::from_serialize(self.draft_context(ctx)).unwrap()
    }

    fn template(&self) -> &str {
        "post.html"
    }

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Debug, Serialize)]
struct DraftContext<'a> {
    title: Cow<'a, str>,
    url: Cow<'a, str>,
    content: String,
    tags: Vec<TagPostContext<'a>>,
}

#[derive(Deserialize, Debug)]
struct DraftMetadata {
    title: String,
    tags: TagsMeta,
}

#[derive(Debug)]
pub struct DraftArchiveItem<'a> {
    pub title: String,
    pub url: SiteUrl,
    pub drafts: Vec<&'a DraftItem>,
}

impl TeraItem for DraftArchiveItem<'_> {
    fn context(&self, _ctx: &RenderContext) -> Context {
        Context::from_serialize(ArchiveContext {
            title: &self.title,
            posts: self
                .drafts
                .iter()
                .map(|draft| draft.draft_ref_context())
                .collect(),
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "archive.html"
    }

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Debug, Clone, Serialize)]
struct ArchiveContext<'a> {
    title: &'a str,
    posts: Vec<DraftRefContext<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct DraftRefContext<'a> {
    title: Cow<'a, str>,
    url: Cow<'a, str>,
    tags: Vec<TagPostContext<'a>>,
}
