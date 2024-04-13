use chrono::{Datelike, NaiveDate};
use serde::Serialize;
use std::collections::{BTreeMap, HashMap};
use tera::Context;

use super::posts::{PostRef, PostRefContext};
use crate::{
    content::PostItem,
    item::{RenderContext, TeraItem},
    paths::AbsPath,
    site_url::SiteUrl,
};

pub fn post_archives(posts: &BTreeMap<PostRef, PostItem>) -> Vec<ArchiveItem> {
    let posts: Vec<PostRef> = posts
        .iter()
        .filter_map(|(post_ref, post)| {
            if post.is_draft {
                None
            } else {
                Some(post_ref.clone())
            }
        })
        .collect();

    let mut by_year: HashMap<i32, Vec<PostRef>> = HashMap::new();
    let mut by_year_month: HashMap<(i32, u32), Vec<PostRef>> = HashMap::new();

    for post in posts.iter() {
        by_year
            .entry(post.created.year())
            .or_insert_with(Vec::new)
            .push(post.clone());

        by_year_month
            .entry((post.created.year(), post.created.month()))
            .or_insert_with(Vec::new)
            .push(post.clone());
    }

    let mut res = vec![ArchiveItem {
        title: "All posts".to_string(),
        url: SiteUrl::parse("/blog").unwrap(),
        posts,
        tag_filter: None,
    }];
    res.extend(by_year.into_iter().map(|(year, posts)| ArchiveItem {
        title: format!("{}", year),
        url: SiteUrl::parse(&format!("/blog/{}", year)).unwrap(),
        posts,
        tag_filter: None,
    }));
    res.extend(by_year_month.into_iter().map(|((year, month), posts)| {
        let date = NaiveDate::from_ymd_opt(year, month, 1).unwrap();
        ArchiveItem {
            title: date.format("%B %Y").to_string(),
            url: SiteUrl::parse(&date.format("/blog/%Y/%m").to_string()).unwrap(),
            posts,
            tag_filter: None,
        }
    }));
    res
}

#[derive(Debug)]
pub struct ArchiveItem {
    pub title: String,
    pub url: SiteUrl,
    pub posts: Vec<PostRef>,
    pub tag_filter: Option<String>,
}

impl TeraItem for ArchiveItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        Context::from_serialize(ArchiveContext {
            title: &self.title,
            posts: self
                .posts
                .iter()
                .map(|post| PostRefContext::from_ref(post, ctx))
                .collect(),
            tag_filter: self.tag_filter.clone(),
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "archive.html"
    }

    fn tera_url(&self) -> &SiteUrl {
        &self.url
    }

    fn tera_source_file(&self) -> Option<&AbsPath> {
        None
    }
}

#[derive(Debug, Clone, Serialize)]
struct ArchiveContext<'a> {
    title: &'a str,
    posts: Vec<PostRefContext<'a>>,
    tag_filter: Option<String>,
}
