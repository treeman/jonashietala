use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::fs;

use camino::Utf8Path;
use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use eyre::{eyre, Result};
use itemref_derive::ItemRef;
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use regex::Regex;
use serde::{Deserialize, Serialize};
use tera::Context;
use yaml_front_matter::{Document, YamlFrontMatter};

use crate::content::series::SeriesRef;
use crate::content::tags::{Tag, TagPostContext, TagsMeta};
use crate::item::Item;
use crate::item::RenderContext;
use crate::markdown::find_markdown_files;
use crate::markdown::markdown_to_html;
use crate::paths::AbsPath;
use crate::{content::SeriesItem, item::TeraItem, site_url::SiteUrl, util};

pub fn load_posts(dir: AbsPath) -> Result<BTreeMap<PostRef, PostItem>> {
    let mut posts = find_markdown_files(dir)
        .par_iter()
        .map(|path| PostItem::from_file(path.abs_path()).map(|post| (post.post_ref(), post)))
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
    pub created: NaiveDateTime,
}

#[derive(Debug)]
pub struct PostItem {
    pub title: String,
    pub tags: Vec<Tag>,
    pub created: NaiveDateTime,
    pub updated: NaiveDateTime,
    pub path: AbsPath,
    pub url: SiteUrl,
    pub prev: Option<PostRef>,
    pub next: Option<PostRef>,
    pub recommended: bool,
    pub raw_content: String,
    pub transformed_content: String,
    pub series_id: Option<String>,
    pub series: Option<SeriesRef>,
}

impl PostItem {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let modified = util::last_modified(&path)?;
        let raw_content = fs::read_to_string(&path)?;
        Self::from_string(path, raw_content, modified)
    }

    pub fn from_string(
        path: AbsPath,
        raw_content: String,
        modified: NaiveDateTime,
    ) -> Result<Self> {
        let post_dir = PostDirMetadata::from_path(&path)?;

        let Document { metadata, content } =
            YamlFrontMatter::parse::<PostMetadata>(&raw_content)
                .map_err(|err| eyre!("Failed to parse metadata for post: {}\n{}", path, err))?;

        let time = match &metadata.time {
            Some(time_str) => parse_time(&time_str)?,
            None => NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
        };

        let created = NaiveDateTime::new(post_dir.date, time);

        let url = post_dir.to_url().expect("Should be able to create a url");

        let transformed_content = markdown_to_html(&content);

        Ok(Self {
            title: metadata.title,
            tags: metadata.tags.into(),
            created,
            updated: modified,
            path,
            url,
            prev: None,
            next: None,
            raw_content: content,
            transformed_content,
            series_id: metadata.series,
            series: None,
            recommended: metadata.recommended.unwrap_or(false),
        })
    }

    pub fn post_ref(&self) -> PostRef {
        PostRef {
            id: self.id().to_string(),
            created: self.created.clone(),
        }
    }
}

impl TeraItem for PostItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        let series = self.series.as_ref().map(|series| {
            PostSeriesContext::new(
                self,
                ctx.content
                    .get_series(series)
                    .expect("Could not find series {:?series}"),
                ctx,
            )
        });

        Context::from_serialize(PostContext {
            title: html_escape::encode_text(&self.title),
            url: self.url.href(),
            created: self.created.format("%FT%T%.fZ").to_string(),
            content: &self.transformed_content,
            tags: self.tags.iter().map(TagPostContext::from).collect(),
            meta_keywords: self.tags.iter().map(|tag| tag.name.as_str()).collect(),
            series,
            prev: self.prev.as_ref().map(|x| PostRefContext::from_ref(x, ctx)),
            next: self.next.as_ref().map(|x| PostRefContext::from_ref(x, ctx)),
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "post.html"
    }

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Debug, Clone, Serialize)]
struct PostContext<'a> {
    title: Cow<'a, str>,
    url: Cow<'a, str>,
    created: String,
    content: &'a str,
    tags: Vec<TagPostContext<'a>>,
    meta_keywords: Vec<&'a str>,
    series: Option<PostSeriesContext<'a>>,
    prev: Option<PostRefContext<'a>>,
    next: Option<PostRefContext<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct PostSeriesContext<'a> {
    title: Cow<'a, str>,
    url: Cow<'a, str>,
    completed: bool,
    part_number: usize,
    post_note: Option<&'a str>,
    next_url: Option<Cow<'a, str>>,
}

impl<'a> PostSeriesContext<'a> {
    fn new(post: &PostItem, series: &'a SeriesItem, ctx: &'a RenderContext) -> Self {
        let posts: Vec<_> = series.posts.iter().collect();

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
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct PostRefContext<'a> {
    title: &'a str,
    url: Cow<'a, str>,
    created: String,
    tags: Vec<TagPostContext<'a>>,
}

impl<'a> PostRefContext<'a> {
    pub fn from_post(post: &'a PostItem) -> Self {
        PostRefContext {
            title: &post.title,
            url: post.url.href(),
            created: post.created.format("%FT%T%.fZ").to_string(),
            tags: post.tags.iter().map(TagPostContext::from).collect(),
        }
    }

    pub fn from_ref(post_ref: &PostRef, ctx: &'a RenderContext) -> Self {
        let post = ctx.content.get_post(post_ref).expect("Should have post");
        Self::from_post(post)
    }
}

#[derive(Deserialize, Debug)]
struct PostMetadata {
    title: String,
    tags: TagsMeta,
    time: Option<String>,
    series: Option<String>,
    recommended: Option<bool>,
}

#[derive(Debug)]
pub struct PostDirMetadata {
    pub date: NaiveDate,
    pub slug: String,
}

impl PostDirMetadata {
    pub fn from_path(path: &Utf8Path) -> Result<Self> {
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
        })
    }

    pub fn to_url(&self) -> Result<SiteUrl> {
        SiteUrl::parse(&format!(
            "/blog/{}/{}/",
            self.date.format("%Y/%m/%d"),
            self.slug
        ))
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
    use std::path::PathBuf;

    use super::*;
    use crate::tests::*;
    use crate::{item::RenderContext, site::SiteContext};
    use scraper::{node::Element, Html, Selector};

    #[test]
    fn test_post_from_string() -> Result<()> {
        let path = "posts/2022-01-31-test_post.markdown";
        let content = fs::read_to_string(PathBuf::from("test-site").join(path))?;
        // let (path, content) = tests::raw_post1();
        let post = PostItem::from_string(
            path.into(),
            content,
            NaiveDateTime::new(
                NaiveDate::from_ymd_opt(2022, 4, 30).unwrap(),
                NaiveTime::from_hms_opt(1, 2, 3).unwrap(),
            ),
        )?;

        assert_eq!(post.title, "Post & Title");
        assert_eq!(
            post.tags,
            vec![
                Tag {
                    id: "Tag1".to_string(),
                    name: "Tag1".to_string(),
                    url: SiteUrl::parse("/blog/tags/tag1").unwrap(),
                },
                Tag {
                    id: "<Tag> 2".to_string(),
                    name: "&lt;Tag&gt; 2".to_string(),
                    url: SiteUrl::parse("/blog/tags/tag_2").unwrap(),
                }
            ]
        );
        assert_eq!(
            post.created,
            NaiveDateTime::new(
                NaiveDate::from_ymd_opt(2022, 1, 31).unwrap(),
                NaiveTime::from_hms_opt(7, 7, 0).unwrap()
            )
        );
        assert!(post.raw_content.contains("# Header 1"));
        assert_eq!(post.url.path(), "/blog/2022/01/31/test_post/");
        assert_eq!(post.url.href(), "/blog/2022/01/31/test_post/");
        assert_eq!(
            post.output_file(".output".into()),
            ".output/blog/2022/01/31/test_post/index.html"
        );

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
    fn postref() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let post = test_site
            .find_post("2022-01-31-test_post.markdown")
            .unwrap();

        assert_eq!(
            post.post_ref(),
            PostRef {
                id: "/blog/2022/01/31/test_post/".to_string(),
                created: NaiveDate::from_ymd_opt(2022, 1, 31)
                    .unwrap()
                    .and_hms_opt(7, 7, 0)
                    .unwrap(),
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

        let post = test_site
            .find_post("2022-01-31-test_post.markdown")
            .unwrap();

        let rendered = post.render_to_string(&RenderContext {
            parent_context: &Context::from_serialize(SiteContext::new(false)).unwrap(),
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
            Some("Tag1, <Tag> 2")
        );
        assert_eq!(
            select_inner_html(&document, "title").unwrap(),
            "Jonas Hietala: Post &amp; Title"
        );
        assert!(rendered
            .contains(r#"<h1><a href="/blog/2022/01/31/test_post/">Post &amp; Title</a></h1>"#));
        assert!(rendered.contains(r#"<time datetime="2022-01-31T07:07:00Z""#));
        assert!(rendered.contains(
            r##"<h2 id="header-1"><a class="heading-ref" href="#header-1">Header 1</a></h2>"##
        ));
        assert!(rendered.contains(r#"<iframe src="//www.youtube.com/embed/eoKDyhxCVm0""#));
        assert!(rendered.contains("☃︎"));
        assert!(rendered.contains("—and–some…"));
        assert!(rendered.contains("“Auto quote” ‘A’"));
        // Yeah maybe it wold be easier to check these another way
        assert!(rendered.contains(r#"href="/blog/tags/tag1""#));
        assert!(rendered.contains(r#"href="/blog/tags/tag_2""#));
        assert!(rendered.contains(r#"title="Posts tagged `&lt;Tag&gt; 2`""#));

        // Just make sure that code is highlighted
        let rust_code = select_inner_html(&document, r#"pre code.rust"#).unwrap();
        assert!(rust_code.contains("<span class=\"storage type rust\">let</span> x "));

        // FIXME git commit
        Ok(())
    }
}
