use crate::{
    content::ArchiveItem,
    content::PostRef,
    item::{RenderContext, TeraItem},
    site_url::SiteUrl,
    util,
};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashMap;
use tera::Context;

pub fn tags_archives(tags: &HashMap<Tag, Vec<PostRef>>) -> Vec<ArchiveItem> {
    tags.iter()
        .map(|(tag, posts)| ArchiveItem {
            title: format!("Posts tagged {}", tag.name),
            url: tag.url.clone(),
            posts: posts.to_vec(),
        })
        .collect()
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Hash)]
pub struct Tag {
    pub id: String,
    pub name: String,
    pub url: SiteUrl,
}

impl Ord for Tag {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Tag {
    pub fn new(name: &str) -> Self {
        let name = name.trim();
        Tag {
            id: name.to_string(),
            name: html_escape::encode_text(&name).to_string(),
            url: SiteUrl::parse(&format!("/blog/tags/{}", util::slugify(name))).unwrap(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct TagPostContext<'a> {
    name: &'a str,
    href: Cow<'a, str>,
}

impl<'a> From<&'a Tag> for TagPostContext<'a> {
    fn from(tag: &'a Tag) -> Self {
        Self {
            name: &tag.name,
            href: tag.url.href(),
        }
    }
}

// Historically tags have been specified as
//
//      tags: Tag1, Tag2
//
// Instead as the YAML compliant
//
//      tags: [Tag1, Tag2]
//
// But here we do the manual splitting on ',' if it's of the first version.
#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum TagsMeta {
    Seq(Vec<String>),
    Str(String),
}

impl From<TagsMeta> for Vec<Tag> {
    fn from(tags: TagsMeta) -> Self {
        match tags {
            TagsMeta::Seq(xs) => xs.into_iter().map(|tag| Tag::new(&tag)).sorted().collect(),
            TagsMeta::Str(x) => x.split(',').map(Tag::new).collect(),
        }
    }
}

#[derive(Debug)]
pub struct TagListItem<'a> {
    pub tags: &'a HashMap<Tag, Vec<PostRef>>,
    pub url: SiteUrl,
}

impl<'a> TagListItem<'a> {
    pub fn new(tags: &'a HashMap<Tag, Vec<PostRef>>) -> Self {
        Self {
            tags,
            url: SiteUrl::parse("/blog/tags").unwrap(),
        }
    }
}

impl TeraItem for TagListItem<'_> {
    fn context(&self, _ctx: &RenderContext) -> tera::Context {
        let mut tags: Vec<TagContext<'_>> = self
            .tags
            .iter()
            .map(|(tag, posts)| TagContext {
                name: &tag.name,
                href: tag.url.href(),
                num_posts: posts.len(),
            })
            .collect();
        tags.sort();

        Context::from_serialize(TagListContext {
            title: "Tags",
            tags,
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "tags.html"
    }

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Debug, Clone, Serialize)]
struct TagListContext<'a> {
    title: &'a str,
    tags: Vec<TagContext<'a>>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
struct TagContext<'a> {
    name: &'a str,
    href: Cow<'a, str>,
    num_posts: usize,
}

impl Ord for TagContext<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        (other.num_posts, other.name).cmp(&(self.num_posts, self.name))
    }
}

impl PartialOrd for TagContext<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
