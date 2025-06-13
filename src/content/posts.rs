use camino::Utf8Path;
use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use eyre::{eyre, Result};
use itemref_derive::ItemRef;
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fmt::Debug;
use tera::Context;
use tracing::error;

use crate::content::series::SeriesRef;
use crate::content::tags::{Tag, TagPostContext, TagsMeta};
use crate::context::{LoadContext, RenderContext};
use crate::git::{CommitContext, LatestCommitInfo};
use crate::item::Item;
use crate::markup::{self, Html, Markup, MarkupLookup, ParseContext, RawMarkupFile};
use crate::paths::{AbsPath, FilePath, RelPath};
use crate::{content::SeriesItem, item::TeraItem, site_url::SiteUrl, util};

pub fn load_posts(dirs: &[AbsPath], context: &LoadContext) -> Result<BTreeMap<PostRef, PostItem>> {
    let mut posts = markup::find_markup_files(&context.opts.input_dir, dirs)
        .par_iter()
        .map(|path| PostItem::from_file(path, context).map(|post| (post.post_ref(), post)))
        .collect::<Result<BTreeMap<PostRef, PostItem>>>()?;

    set_post_prev_next(&mut posts);

    Ok(posts)
}

pub fn set_post_prev_next(posts: &mut BTreeMap<PostRef, PostItem>) {
    let mut next: Option<(&PostRef, &mut PostItem)> = None;
    for curr in posts.iter_mut().peekable() {
        curr.1.next = next.as_ref().map(|x| x.0.clone());
        if let Some((_, next_post)) = next {
            next_post.prev = Some(curr.0.clone());
        }
        next = Some(curr);
    }
}

#[derive(ItemRef, Debug, Clone)]
#[item(PostItem)]
pub struct PostRef {
    pub id: String,
    #[order]
    pub order: PostRefOrder,
}

#[derive(Debug, Clone, Eq)]
pub struct PostRefOrder {
    pub id: String,
    pub is_draft: bool,
    pub created: NaiveDateTime,
}

impl PartialOrd for PostRefOrder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PostRefOrder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.id == other.id {
            std::cmp::Ordering::Equal
        } else if self.is_draft && other.is_draft {
            (self.id).cmp(&(other.id))
        } else {
            (self.is_draft, self.created).cmp(&(other.is_draft, other.created))
        }
    }
}

impl PartialEq for PostRefOrder {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Debug)]
pub struct PostItem {
    pub title: String,
    pub tags: Vec<Tag>,
    pub created: NaiveDateTime,
    pub modified: NaiveDateTime,
    pub latest_commit: Option<LatestCommitInfo>,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub prev: Option<PostRef>,
    pub next: Option<PostRef>,
    pub recommended: bool,
    pub favorite: bool,
    pub content: Html,
    pub markup: Markup,
    pub markup_lookup: Option<MarkupLookup>,
    pub embedded_files: HashSet<RelPath>,
    pub series_id: Option<String>,
    pub series: Option<SeriesRef>,
    pub is_draft: bool,
}

impl PostItem {
    pub fn from_file(path: &FilePath, context: &LoadContext) -> Result<Self> {
        let abs_path = path.abs_path();
        let modified = util::last_modified(&abs_path)?;
        let markup = RawMarkupFile::from_file(abs_path)?;
        let latest_commit = context.get_commit(path).cloned();
        Self::from_markup(markup, modified, latest_commit)
    }

    pub fn from_markup(
        markup: RawMarkupFile<PostMetadata>,
        modified: NaiveDateTime,
        latest_commit: Option<LatestCommitInfo>,
    ) -> Result<Self> {
        let partial =
            PartialPostItem::from_markup(markup.path.clone(), &markup.markup_meta, modified)?;

        let meta_line_count = markup.meta_line_count;
        let markup = markup.parse(ParseContext::new_post_context(
            partial.is_draft,
            meta_line_count,
        ))?;

        Ok(Self {
            title: partial.title,
            tags: partial.tags,
            created: partial.created,
            modified: partial.updated,
            latest_commit,
            path: partial.path,
            url: partial.url,
            prev: None,
            next: None,
            content: markup.html,
            markup: markup.markup,
            markup_lookup: markup.markup_lookup,
            embedded_files: markup.embedded_files,
            series_id: partial.series_id,
            series: None,
            recommended: partial.recommended,
            favorite: partial.favorite,
            is_draft: partial.is_draft,
        })
    }

    pub fn post_ref(&self) -> PostRef {
        PostRef {
            id: self.id().to_string(),
            order: PostRefOrder {
                id: self.id().to_string(),
                is_draft: self.is_draft,
                created: self.created,
            },
        }
    }
}

impl TeraItem for PostItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        let series = self.series.as_ref().and_then(|series_ref| {
            if let Some(series) = ctx.content.get_series(series_ref) {
                Some(PostSeriesContext::new(self, series, ctx))
            } else {
                error!("Couldn't find series: {series_ref:#?}");
                None
            }
        });

        Context::from_serialize(PostContext {
            title: html_escape::encode_text(&self.title),
            url: self.url.href(),
            created: self.created.format("%FT%T%.fZ").to_string(),
            latest_commit: self.latest_commit.as_ref().map(Into::into),
            content: &self.content,
            tags: self.tags.iter().map(TagPostContext::from).collect(),
            meta_keywords: self.tags.iter().map(|tag| tag.name.as_str()).collect(),
            series,
            favorite: self.favorite,
            prev: self.prev.as_ref().map(|x| PostRefContext::from_ref(x, ctx)),
            next: self.next.as_ref().map(|x| PostRefContext::from_ref(x, ctx)),
            is_draft: self.is_draft,
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "post.html"
    }

    fn tera_url(&self) -> &SiteUrl {
        &self.url
    }

    fn tera_source_file(&self) -> Option<&AbsPath> {
        Some(&self.path)
    }
}

/// A post item with frontmatter data but without markup.
#[derive(Debug, Eq)]
pub struct PartialPostItem {
    pub title: String,
    pub tags: Vec<Tag>,
    pub created: NaiveDateTime,
    pub updated: NaiveDateTime,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub recommended: bool,
    pub favorite: bool,
    pub series_id: Option<String>,
    pub is_draft: bool,
}

impl PartialPostItem {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let modified = util::last_modified(&path)?;
        let markup = RawMarkupFile::from_file(path)?;
        Self::from_markup(markup.path, &markup.markup_meta, modified)
    }

    pub fn from_markup(
        path: AbsPath,
        meta: &PostMetadata,
        modified: NaiveDateTime,
    ) -> Result<Self> {
        let post_dir = PostDirMetadata::from_path(&path, &modified)?;

        let time = match &meta.time {
            Some(time_str) => parse_time(time_str)?,
            None => NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
        };

        let created = NaiveDateTime::new(post_dir.date, time);
        let url = post_dir.to_url()?;

        Ok(Self {
            title: meta.title.clone(),
            tags: meta.tags.clone().into(),
            created,
            updated: modified,
            path,
            url,
            series_id: meta.series.clone(),
            recommended: meta.recommended.unwrap_or(false),
            favorite: meta.favorite.unwrap_or(false),
            is_draft: post_dir.is_draft,
        })
    }
}

impl TryFrom<&FilePath> for PartialPostItem {
    type Error = eyre::Error;

    fn try_from(path: &FilePath) -> std::result::Result<Self, Self::Error> {
        PartialPostItem::from_file(path.abs_path())
    }
}

impl PartialEq for PartialPostItem {
    fn eq(&self, other: &Self) -> bool {
        self.path.eq(&other.path)
    }
}

impl PartialOrd for PartialPostItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PartialPostItem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.created.cmp(&other.created)
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct CountedWordsPostItem {
    pub title: String,
    pub tags: Vec<Tag>,
    pub created: NaiveDate,
    pub url: SiteUrl,
    pub word_count: usize,
    pub series_id: Option<String>,
}

impl CountedWordsPostItem {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let post_dir = PostDirMetadata::parse_post(&path)?;
        let markup: RawMarkupFile<PostMetadata> = RawMarkupFile::from_file(path)?;

        let words: Vec<_> = markup.markup.content().split_whitespace().collect();

        let url = post_dir.to_url()?;

        Ok(Self {
            title: markup.markup_meta.title.clone(),
            tags: markup.markup_meta.tags.clone().into(),
            created: post_dir.date,
            url,
            series_id: markup.markup_meta.series.clone(),
            word_count: words.len(),
        })
    }
}

#[derive(Debug, Clone, Serialize)]
struct PostContext<'a> {
    title: Cow<'a, str>,
    url: Cow<'a, str>,
    created: String,
    latest_commit: Option<CommitContext>,
    content: &'a str,
    favorite: bool,
    tags: Vec<TagPostContext<'a>>,
    meta_keywords: Vec<&'a str>,
    series: Option<PostSeriesContext<'a>>,
    prev: Option<PostRefContext<'a>>,
    next: Option<PostRefContext<'a>>,
    is_draft: bool,
}

#[derive(Debug, Clone, Serialize)]
struct PostSeriesContext<'a> {
    title: Cow<'a, str>,
    url: Cow<'a, str>,
    completed: bool,
    part_number: usize,
    post_note: Option<&'a str>,
    next_url: Option<Cow<'a, str>>,
    is_draft: bool,
    posts: Vec<PostRefContext<'a>>,
}

impl<'a> PostSeriesContext<'a> {
    fn new(post: &PostItem, series: &'a SeriesItem, ctx: &'a RenderContext) -> Self {
        let posts: Vec<_> = series.posts.iter().collect();

        // FIXME this sometimes crashes.
        // If we add a draft?
        let post_index = posts.iter().position(|curr| &curr.0 == post).unwrap();
        let next_url = posts
            .get(post_index + 1)
            .as_ref()
            .and_then(|next| ctx.content.get_post(&next.0).as_ref().map(|x| x.url.href()));

        Self {
            title: html_escape::encode_text(&series.title),
            url: series.url.href(),
            completed: series.completed,
            part_number: post_index + 1,
            next_url,
            post_note: series.post_note.as_deref(),
            is_draft: post.is_draft,
            posts: series
                .posts
                .iter()
                .map(|x| PostRefContext::from_ref(&x.0, ctx))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct PostRefContext<'a> {
    title: &'a str,
    url: Cow<'a, str>,
    created: String,
    tags: Vec<TagPostContext<'a>>,
    favorite: bool,
    is_draft: bool,
}

impl<'a> PostRefContext<'a> {
    pub fn from_post(post: &'a PostItem) -> Self {
        PostRefContext {
            title: &post.title,
            url: post.url.href(),
            created: post.created.format("%FT%T%.fZ").to_string(),
            tags: post.tags.iter().map(TagPostContext::from).collect(),
            favorite: post.favorite,
            is_draft: post.is_draft,
        }
    }

    pub fn from_ref(post_ref: &PostRef, ctx: &'a RenderContext) -> Self {
        let post = ctx.content.get_post(post_ref).expect("Should have post");
        Self::from_post(post)
    }
}

#[derive(Deserialize, Debug)]
pub struct PostMetadata {
    pub title: String,
    pub tags: TagsMeta,
    pub time: Option<String>,
    pub series: Option<String>,
    pub recommended: Option<bool>,
    pub favorite: Option<bool>,
}

#[derive(Debug)]
pub struct PostDirMetadata {
    pub date: NaiveDate,
    pub slug: String,
    pub is_draft: bool,
}

impl PostDirMetadata {
    pub fn from_path(path: &Utf8Path, modified: &NaiveDateTime) -> Result<Self> {
        Self::parse_post(path).or_else(|_| Self::parse_draft(path, modified))
    }

    pub fn parse_post(path: &Utf8Path) -> Result<Self> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^(\d{4})-(\d{2})-(\d{2})-(\S+)$").unwrap();
        }
        let captures = RE
            .captures(path.file_stem().expect("should have a file name"))
            .ok_or_else(|| eyre!("Failed to parse post path: {}", path))?;

        Ok(Self {
            date: NaiveDate::from_ymd_opt(
                captures[1].parse().unwrap(),
                captures[2].parse().unwrap(),
                captures[3].parse().unwrap(),
            )
            .ok_or_else(|| eyre!("Post has invalid ymd: {}", path))?,
            slug: captures[4].to_string(),
            is_draft: false,
        })
    }

    pub fn parse_draft(path: &Utf8Path, modified: &NaiveDateTime) -> Result<Self> {
        let slug = path
            .file_stem()
            .ok_or_else(|| eyre!("Missing file stem: {path}"))?
            .to_string();

        Ok(Self {
            date: modified.date(),
            slug,
            is_draft: true,
        })
    }

    pub fn to_url(&self) -> Result<SiteUrl> {
        if self.is_draft {
            SiteUrl::parse(&format!("/drafts/{}", self.slug))
        } else {
            SiteUrl::parse(&format!(
                "/blog/{}/{}/",
                self.date.format("%Y/%m/%d"),
                self.slug
            ))
        }
    }
}

fn parse_time(s: &str) -> Result<NaiveTime> {
    if s.len() == 8 {
        NaiveTime::parse_from_str(s, "%H:%M:%S")
            .map_err(|err| eyre!("Failed to parse time: `{}`: {}", s, err))
    } else if s.len() == 5 {
        NaiveTime::parse_from_str(s, "%H:%M")
            .map_err(|err| eyre!("Failed to parse time: `{}`: {}", s, err))
    } else {
        Err(eyre!("Wrong time length {}", s.len()))
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use super::*;
    use crate::tests::*;
    use crate::{context::RenderContext, site::SiteContext};
    use git2::Oid;
    use scraper::{node::Element, Html, Selector};

    #[test]
    fn test_post_from_string() -> Result<()> {
        let path = "posts/2022-01-31-test_post.dj";
        let content = fs::read_to_string(PathBuf::from("test-site").join(path))?;
        // let (path, content) = tests::raw_post1();
        let modified = NaiveDateTime::new(
            NaiveDate::from_ymd_opt(2022, 4, 30).unwrap(),
            NaiveTime::from_hms_opt(1, 2, 3).unwrap(),
        );

        let post = PostItem::from_markup(
            RawMarkupFile::from_content(content, path.into())?,
            modified,
            Some(LatestCommitInfo {
                dt: modified,
                id: Oid::from_str("f66a95823286a8d05fc4878fb40f7391545cdb91")?,
                is_revision: true,
            }),
        )?;

        assert_eq!(post.title, "Post & Title");
        assert_eq!(
            post.tags,
            vec![
                Tag {
                    id: "<Tag> 2".to_string(),
                    name: "&lt;Tag&gt; 2".to_string(),
                    url: SiteUrl::parse("/blog/tags/tag_2").unwrap(),
                },
                Tag {
                    id: "Tag1".to_string(),
                    name: "Tag1".to_string(),
                    url: SiteUrl::parse("/blog/tags/tag1").unwrap(),
                },
            ]
        );
        assert_eq!(
            post.created,
            NaiveDateTime::new(
                NaiveDate::from_ymd_opt(2022, 1, 31).unwrap(),
                NaiveTime::from_hms_opt(7, 7, 0).unwrap()
            )
        );
        assert!(post.markup.content().contains("# Header 1"));
        assert_eq!(post.url.path(), "/blog/2022/01/31/test_post/");
        assert_eq!(post.url.href(), "/blog/2022/01/31/test_post");
        assert_eq!(
            post.output_file(".output".into()),
            ".output/blog/2022/01/31/test_post/index.html"
        );
        assert!(post.latest_commit.is_some());

        Ok(())
    }

    fn select_inner_html(document: &Html, selectors: &str) -> Option<String> {
        Some(
            document
                .select(&Selector::parse(selectors).unwrap())
                .next()?
                .inner_html(),
        )
    }

    fn select_element<'a>(document: &'a Html, selectors: &str) -> Option<&'a Element> {
        Some(
            document
                .select(&Selector::parse(selectors).unwrap())
                .next()?
                .value(),
        )
    }

    #[test]
    fn test_postref() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let post = test_site.find_post("2022-01-31-test_post.dj").unwrap();

        let id = "/blog/2022/01/31/test_post".to_string();
        assert_eq!(
            post.post_ref(),
            PostRef {
                id: id.clone(),
                order: PostRefOrder {
                    id,
                    is_draft: false,
                    created: NaiveDate::from_ymd_opt(2022, 1, 31)
                        .unwrap()
                        .and_hms_opt(7, 7, 0)
                        .unwrap(),
                }
            }
        );
        Ok(())
    }

    #[test]
    fn test_render_post() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let post = test_site.find_post("2022-01-31-test_post.dj").unwrap();

        let rendered = post.render_to_string(&RenderContext {
            parent_context: &Context::from_serialize(SiteContext::new(false, false)).unwrap(),
            output_dir: ".test_output".into(), // Not used
            tera: tests::templates(),
            content: &test_site.site.content,
        })?;
        let document = Html::parse_document(&rendered);

        // Element selection returns decoded < and >, so we make extra sure it's escaped properly
        // in the raw document.
        assert!(!rendered.contains("<Tag>"));
        assert_eq!(
            select_element(&document, r#"meta[name="keywords"]"#)
                .unwrap()
                .attr("content"),
            Some("<Tag> 2, Tag1")
        );
        assert_eq!(
            select_inner_html(&document, "title").unwrap(),
            "Jonas Hietala: Post &amp; Title"
        );
        assert!(rendered.contains(
            r#"<a href="/blog/2022/01/31/test_post" class="title">Post &amp; Title</a>"#
        ));
        assert!(rendered.contains(r#"<span class="favorite"><a href="/favorite">★</a></span>"#));
        assert!(rendered.contains(r#"<time datetime="2022-01-31T07:07:00Z""#));
        assert!(
            rendered.contains(r##"<h2><a href="#Header-1" class="heading-ref">Header 1</a></h2>"##)
        );
        assert!(rendered.contains(r#"<a href="https://www.youtube.com/watch?v=eoKDyhxCVm0""#));
        assert!(rendered.contains("☃︎"));
        assert!(rendered.contains("Dashes—and–some…"));
        assert!(rendered.contains("“Auto quote” ‘A’"));
        // Yeah maybe it wold be easier to check these another way
        assert!(rendered.contains(r#"href="/blog/tags/tag1""#));
        assert!(rendered.contains(r#"href="/blog/tags/tag_2""#));
        assert!(rendered.contains(r#"title="Posts tagged `&lt;Tag&gt; 2`""#));

        // We should reference the full commit hash somewhere
        assert!(rendered.contains("f15141519b858f5ce1ce0bd22935d579ceb74061"));

        // Just make sure that code is highlighted
        let rust_code = select_inner_html(&document, r#"pre code.rust"#).unwrap();
        assert!(rust_code.contains("<span class=\"storage type rust\">let</span> x"));
        Ok(())
    }
}
