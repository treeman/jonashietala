use std::collections::BTreeMap;

use eyre::Result;
use serde::Serialize;
use tera::Context;

use crate::content::posts::{PostItem, PostRef};
use crate::content::{Project, ProjectContext};
use crate::content::{SeriesContext, SeriesItem, SeriesRef};
use crate::{item::RenderContext, item::TeraItem, site_url::SiteUrl};

use super::posts::PostRefContext;

#[derive(Debug)]
pub struct HomepageItem {
    pub url: SiteUrl,
    pub recent: Vec<PostRef>,
    pub recommended: Vec<PostRef>,
    pub series: Vec<SeriesRef>,
    pub projects: Vec<Project>,
}

impl HomepageItem {
    pub fn new(
        posts: &BTreeMap<PostRef, PostItem>,
        series: &BTreeMap<SeriesRef, SeriesItem>,
        projects: &Vec<Project>,
    ) -> Result<Self> {
        let url = SiteUrl::parse("/").expect("Should be able to create a url");

        Ok(Self {
            url,
            recent: Self::get_recent(posts),
            recommended: Self::get_recommended(posts),
            series: series.keys().map(Clone::clone).collect(),
            projects: projects.iter().map(Clone::clone).collect(),
        })
    }

    pub fn update_posts(&mut self, posts: &BTreeMap<PostRef, PostItem>) {
        self.recent = Self::get_recent(posts);
        self.recommended = Self::get_recommended(posts);
    }

    fn get_recent(posts: &BTreeMap<PostRef, PostItem>) -> Vec<PostRef> {
        posts.keys().take(5).map(Clone::clone).collect()
    }

    fn get_recommended(posts: &BTreeMap<PostRef, PostItem>) -> Vec<PostRef> {
        posts
            .iter()
            .filter(|(_, post)| post.recommended)
            .map(|(post_ref, _)| post_ref.clone())
            .collect()
    }
}

impl TeraItem for HomepageItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        Context::from_serialize(HomepageContext {
            title: "Home",
            recommended: self
                .recommended
                .iter()
                .map(|x| PostRefContext::from_ref(x, ctx))
                .collect(),
            recent: self
                .recent
                .iter()
                .map(|x| PostRefContext::from_ref(x, ctx))
                .collect(),
            series: self
                .series
                .iter()
                .map(|series| SeriesContext::from_ref(series, ctx))
                .collect(),
            projects: self.projects.iter().map(|x| x.context(ctx)).collect(),
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "homepage.html"
    }

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Debug, Clone, Serialize)]
struct HomepageContext<'a> {
    title: &'static str,
    recommended: Vec<PostRefContext<'a>>,
    recent: Vec<PostRefContext<'a>>,
    series: Vec<SeriesContext<'a>>,
    projects: Vec<ProjectContext<'a>>,
}
