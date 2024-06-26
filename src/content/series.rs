use crate::context::LoadContext;
use crate::item::Item;
use crate::markup::{find_markup_files, Html, Markup, MarkupLookup, ParseContext, RawMarkupFile};
use crate::paths::AbsPath;
use crate::{content::PostItem, context::RenderContext, item::TeraItem, site_url::SiteUrl};
use chrono::{NaiveDate, Utc};
use eyre::{eyre, Result};
use itemref_derive::ItemRef;
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use tera::Context;
use tracing::warn;

use super::posts::{PostRef, PostRefContext};

pub fn load_series(
    dir: AbsPath,
    context: &LoadContext,
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

    let mut series = find_markup_files(&context.opts.input_dir, &[dir])
        .par_iter_mut()
        .map(|path| {
            SeriesItem::from_file(path.abs_path(), context.opts.generate_markup_lookup)
                .map(|serie| (serie.id.clone(), serie))
        })
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
    pub img: SiteUrl,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub description: Html,
    pub markup_lookup: Option<MarkupLookup>,
    pub post_note: Option<Html>,
    pub posts: BTreeSet<Reverse<PostRef>>,
    pub homepage: bool,
}

impl SeriesItem {
    pub fn from_file(path: AbsPath, create_lookup: bool) -> Result<Self> {
        let markup = RawMarkupFile::from_file(path)?;
        Self::from_markup(markup, create_lookup)
    }

    pub fn from_markup(markup: RawMarkupFile<SeriesMetadata>, create_lookup: bool) -> Result<Self> {
        let meta_line_count = markup.meta_line_count;
        let markup = markup.parse(ParseContext::new(create_lookup, meta_line_count))?;
        let SeriesDirMetadata { id } = SeriesDirMetadata::from_path(&markup.path)?;

        let url =
            SiteUrl::parse(&format!("/series/{id}/")).expect("Should be able to create a url");

        let post_note = match markup.markup_meta.post_note {
            Some(note) => Some(
                Markup::new(note, markup.markup.t())
                    .parse(ParseContext::default().with_path(&markup.path))?
                    .html
                    .strip_one_paragraph(),
            ),
            None => None,
        };

        let img =
            SiteUrl::parse(&markup.markup_meta.img).expect("Should be able to create url to image");

        Ok(Self {
            id,
            title: markup.markup_meta.title,
            completed: markup.markup_meta.completed,
            img,
            path: markup.path,
            url,
            description: markup.html,
            markup_lookup: markup.markup_lookup,
            post_note,
            posts: BTreeSet::new(),
            homepage: markup.markup_meta.homepage.unwrap_or(false),
        })
    }

    pub fn series_ref(&self) -> SeriesRef {
        SeriesRef {
            id: self.id.to_owned(),
            last_created: self
                .posts
                .iter()
                .last()
                .map(|post_ref| post_ref.0.order.created.date())
                .unwrap_or_else(|| Utc::now().date_naive()),
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

    fn tera_url(&self) -> &SiteUrl {
        &self.url
    }

    fn tera_source_file(&self) -> Option<&AbsPath> {
        None
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct SeriesContext<'a> {
    title: Cow<'a, str>,
    url: Cow<'a, str>,
    img: Cow<'a, str>,
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
            description: &series.description.0,
            completed: series.completed,
            img: series.img.href(),
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
pub struct SeriesMetadata {
    title: String,
    completed: bool,
    post_note: Option<String>,
    img: String,
    homepage: Option<bool>,
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
