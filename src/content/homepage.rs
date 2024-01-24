use std::collections::BTreeMap;

use eyre::Result;
use serde::Serialize;
use tera::Context;

use crate::content::posts::{PostItem, PostRef};
use crate::content::{Game, GameContext};
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
    pub games: Vec<Game>,
}

impl HomepageItem {
    pub fn new(
        posts: &BTreeMap<PostRef, PostItem>,
        series: &BTreeMap<SeriesRef, SeriesItem>,
        projects: &[Project],
        games: &[Game],
    ) -> Result<Self> {
        let url = SiteUrl::parse("/").expect("Should be able to create a url");

        Ok(Self {
            url,
            recent: Self::filter_recent(posts),
            recommended: Self::filter_recommended(posts),
            series: Self::filter_series(series),
            projects: Self::filter_projects(projects),
            games: games.iter().map(Clone::clone).collect(),
        })
    }

    pub fn update_posts(&mut self, posts: &BTreeMap<PostRef, PostItem>) {
        self.recent = Self::filter_recent(posts);
        self.recommended = Self::filter_recommended(posts);
    }

    fn filter_recent(posts: &BTreeMap<PostRef, PostItem>) -> Vec<PostRef> {
        posts
            .iter()
            .filter(|(_, post)| !post.is_draft)
            .take(5)
            .map(|(post_ref, _)| post_ref.clone())
            .collect()
    }

    fn filter_recommended(posts: &BTreeMap<PostRef, PostItem>) -> Vec<PostRef> {
        posts
            .iter()
            .filter(|(_, post)| post.recommended)
            .map(|(post_ref, _)| post_ref.clone())
            .collect()
    }

    fn filter_series(series: &BTreeMap<SeriesRef, SeriesItem>) -> Vec<SeriesRef> {
        series
            .iter()
            .filter(|(_, series)| series.homepage)
            .map(|(series_ref, _)| series_ref.clone())
            .collect()
    }

    fn filter_projects(projects: &[Project]) -> Vec<Project> {
        projects
            .iter()
            .filter(|project| project.homepage)
            .map(Clone::clone)
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
            games: self.games.iter().map(GameContext::from).collect(),
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
    games: Vec<GameContext<'a>>,
}
