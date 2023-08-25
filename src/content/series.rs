use crate::item::Item;
use crate::markdown::{markdown_to_html, markdown_to_html_strip_one_paragraph};
use crate::paths::AbsPath;
use crate::{
    content::PostItem,
    item::{RenderContext, TeraItem},
    markdown::find_markdown_files,
    site_url::SiteUrl,
};
use chrono::NaiveDate;
use eyre::{eyre, Result};
use itemref_derive::ItemRef;
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use tera::Context;
use tracing::warn;
use yaml_front_matter::{Document, YamlFrontMatter};

use super::posts::{PostRef, PostRefContext};

pub fn load_series(
    dir: AbsPath,
    posts: &mut BTreeMap<PostRef, PostItem>,
) -> Result<BTreeMap<SeriesRef, SeriesItem>> {
    let mut posts_in_series: HashMap<String, BTreeSet<Reverse<PostRef>>> = HashMap::new();
    for post in posts.values() {
        if let Some(id) = &post.series_id {
            let series_posts = posts_in_series
                .entry(id.to_string())
                .or_insert(BTreeSet::new());
            series_posts.insert(Reverse(post.post_ref()));
        }
    }

    let mut series = find_markdown_files(dir)
        .par_iter_mut()
        .map(|path| SeriesItem::from_file(path.abs_path()).map(|serie| (serie.id.clone(), serie)))
        .collect::<Result<HashMap<_, _>>>()?;

    for (id, series_posts) in posts_in_series.into_iter() {
        let serie = series
            .get_mut(id.as_str())
            .ok_or_else(|| eyre!("Missing series `{id}`"))?;

        if series_posts.is_empty() {
            warn!("Series definition without post: `{}`", serie.id);
            continue;
        }

        serie.posts = series_posts;

        for post in serie.posts.iter() {
            let post = posts.get_mut(&post.0).expect("Should have post");
            post.series = Some(serie.series_ref());
        }
    }

    Ok(series
        .into_values()
        .map(|serie| (serie.series_ref(), serie))
        .collect())
}

#[derive(ItemRef, Debug, Clone)]
#[item(SeriesItem)]
pub struct SeriesRef {
    pub id: String,
    #[order]
    pub last_created: NaiveDate,
}

#[derive(Debug)]
pub struct SeriesItem {
    pub id: String,
    pub title: String,
    pub completed: bool,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub description: String,
    pub post_note: Option<String>,
    pub posts: BTreeSet<Reverse<PostRef>>,
}

impl SeriesItem {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let raw_content = fs::read_to_string(&path)?;
        Self::from_string(path, raw_content)
    }

    pub fn from_string(path: AbsPath, raw_content: String) -> Result<Self> {
        let SeriesDirMetadata { id } = SeriesDirMetadata::from_path(&path)?;

        let Document { metadata, content } = YamlFrontMatter::parse::<SeriesMetadata>(&raw_content)
            .map_err(|err| eyre!("Failed to parse metadata for serie: {}\n{}", path, err))?;

        let url =
            SiteUrl::parse(&format!("/series/{id}/")).expect("Should be able to create a url");

        let transformed_description = markdown_to_html(&content);
        let transformmed_post_note = metadata
            .post_note
            .as_ref()
            .map(|x| markdown_to_html_strip_one_paragraph(x).into_owned());

        Ok(Self {
            id,
            title: metadata.title,
            completed: metadata.completed,
            path,
            url,
            description: transformed_description,
            post_note: transformmed_post_note,
            posts: BTreeSet::new(),
        })
    }

    pub fn series_ref(&self) -> SeriesRef {
        SeriesRef {
            id: self.id.to_owned(),
            last_created: self
                .posts
                .iter()
                .next()
                .expect("Should have posts")
                .0
                .created,
        }
    }
}

impl TeraItem for SeriesItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        Context::from_serialize(SeriesContext::from_series(self, ctx)).unwrap()
    }

    fn template(&self) -> &str {
        "series.html"
    }

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct SeriesContext<'a> {
    title: Cow<'a, str>,
    url: Cow<'a, str>,
    description: &'a str,
    completed: bool,
    posts: Vec<PostRefContext<'a>>,
    post_note: Option<&'a str>,
}

impl<'a> SeriesContext<'a> {
    pub fn from_ref(series_ref: &SeriesRef, ctx: &'a RenderContext) -> Self {
        let series = ctx
            .content
            .get_series(series_ref)
            .expect("Should have series");

        Self::from_series(series, ctx)
    }

    pub fn from_series(series: &'a SeriesItem, ctx: &'a RenderContext) -> Self {
        Self {
            title: html_escape::encode_text(&series.title),
            url: series.url.href(),
            description: &series.description,
            completed: series.completed,
            posts: series
                .posts
                .iter()
                .map(|x| PostRefContext::from_ref(&x.0, ctx))
                .collect(),
            post_note: series.post_note.as_deref(),
        }
    }
}

#[derive(Deserialize, Debug)]
struct SeriesMetadata {
    title: String,
    completed: bool,
    post_note: Option<String>,
}

pub struct SeriesDirMetadata {
    pub id: String,
}

impl SeriesDirMetadata {
    pub fn from_path(path: &AbsPath) -> Result<Self> {
        match path.file_stem() {
            Some(file) => Ok(Self {
                id: file.to_string(),
            }),
            None => Err(eyre!("Bad series: {}", path)),
        }
    }
}
