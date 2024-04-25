use chrono::NaiveDate;
use eyre::Result;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::cmp::Ordering;
use tera::Context;

use crate::{
    item::RenderContext,
    item::TeraItem,
    markup::find_markup_files,
    markup::{Html, MarkupFile, ParseContext, RawMarkupFile},
    paths::AbsPath,
    site_url::SiteUrl,
};

#[derive(Debug)]
pub struct ProjectsItem {
    prematter: Html,
    pub title: String,
    pub url: SiteUrl,
    pub projects: Vec<Project>,
    pub games: Vec<Game>,
}

impl ProjectsItem {
    pub fn new(dir: &AbsPath) -> Result<Self> {
        let markup: MarkupFile<ProjectsMetadata> =
            RawMarkupFile::from_file(dir.join("projects.markdown"))?
                .parse(ParseContext::default())?;

        let url = SiteUrl::parse("/projects").expect("Should be able to create a url");
        let title = markup.markup_meta.title.clone();

        let project_files = find_markup_files(&[dir.join("projects")]);

        let mut projects = project_files
            .iter()
            .filter(|path| !path.rel_path.starts_with("games/"))
            .map(|path| Project::from_file(path.abs_path()))
            .collect::<Result<Vec<Project>>>()?;
        projects.sort();

        let mut games = project_files
            .iter()
            .filter(|path| path.rel_path.starts_with("games/"))
            .map(|path| Game::from_file(path.abs_path()))
            .collect::<Result<Vec<Game>>>()?;
        games.sort();

        Ok(Self {
            url,
            prematter: markup.html,
            title,
            projects,
            games,
        })
    }
}

impl TeraItem for ProjectsItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        Context::from_serialize(ProjectsContext {
            title: html_escape::encode_text(&self.title),
            prematter: &self.prematter.0,
            projects: self
                .projects
                .iter()
                .map(|project| project.context(ctx))
                .collect(),
            games: self.games.iter().map(GameContext::from).collect(),
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "projects.html"
    }

    fn tera_url(&self) -> &SiteUrl {
        &self.url
    }

    fn tera_source_file(&self) -> Option<&AbsPath> {
        None
    }
}

#[derive(Deserialize, Debug)]
pub struct ProjectsMetadata {
    title: String,
}

#[derive(Debug, Serialize)]
struct ProjectsContext<'a> {
    title: Cow<'a, str>,
    prematter: &'a str,
    projects: Vec<ProjectContext<'a>>,
    games: Vec<GameContext<'a>>,
}

#[derive(Debug, Clone)]
pub struct Project {
    title: String,
    link: Option<String>,
    year: u32,
    path: AbsPath,
    descr: Html,
    // pub markup_lookup: Option<MarkupLookup>,
    pub homepage: bool,
}

impl Project {
    fn from_file(path: AbsPath) -> Result<Self> {
        let markup = RawMarkupFile::from_file(path)?;
        Self::from_markup(markup)
    }

    pub fn from_markup(markup: RawMarkupFile<ProjectMetadata>) -> Result<Self> {
        let markup = markup.parse(ParseContext::default())?;

        Ok(Self {
            title: markup.markup_meta.title,
            link: markup.markup_meta.link,
            year: markup.markup_meta.year,
            path: markup.path,
            descr: markup.html,
            homepage: markup.markup_meta.homepage.unwrap_or(false),
        })
    }

    pub fn context<'a, 'cache>(&'a self, _ctx: &'cache RenderContext) -> ProjectContext<'a> {
        ProjectContext {
            title: html_escape::encode_text(&self.title),
            link: self.link.as_deref(),
            year: self.year,
            descr: &self.descr.0,
        }
    }
}

impl PartialOrd for Project {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Project {
    fn cmp(&self, other: &Self) -> Ordering {
        self.path.cmp(&other.path)
    }
}

impl Eq for Project {}

impl PartialEq for Project {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

#[derive(Debug, Serialize, Clone)]
pub struct ProjectContext<'a> {
    title: Cow<'a, str>,
    link: Option<&'a str>,
    year: u32,
    descr: &'a str,
}

#[derive(Deserialize, Debug)]
pub struct ProjectMetadata {
    title: String,
    link: Option<String>,
    year: u32,
    homepage: Option<bool>,
}

#[derive(Debug, Eq, Clone)]
pub struct Game {
    title: String,
    event: String,
    event_link: Option<String>,
    url: SiteUrl,
    path: AbsPath,
    img: SiteUrl,
    published: NaiveDate,
    // pub markup_lookup: Option<MarkupLookup>,
}

impl Game {
    fn from_file(path: AbsPath) -> Result<Self> {
        let markup = RawMarkupFile::from_file(path)?;
        Self::from_markup(markup)
    }

    pub fn from_markup(markup: RawMarkupFile<GameMetadata>) -> Result<Self> {
        let markup = markup.parse(ParseContext::default())?;

        let published = NaiveDate::parse_from_str(&markup.markup_meta.published, "%Y-%m-%d")?;
        let url = SiteUrl::parse(&markup.markup_meta.url)?;
        let img = SiteUrl::parse(&markup.markup_meta.img)?;

        Ok(Self {
            title: markup.markup_meta.title,
            event: markup.markup_meta.event,
            event_link: markup.markup_meta.event_link,
            url,
            img,
            path: markup.path,
            published,
        })
    }
}

impl PartialOrd for Game {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Game {
    fn cmp(&self, other: &Self) -> Ordering {
        other.published.cmp(&self.published)
    }
}

impl PartialEq for Game {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct GameContext<'a> {
    title: Cow<'a, str>,
    event: &'a str,
    event_link: Option<&'a str>,
    url: Cow<'a, str>,
    img: Cow<'a, str>,
    published: String,
}

impl<'a> From<&'a Game> for GameContext<'a> {
    fn from(x: &'a Game) -> Self {
        Self {
            title: html_escape::encode_text(&x.title),
            event: &x.event,
            event_link: x.event_link.as_deref(),
            url: x.url.href(),
            img: x.img.href(),
            published: x.published.format("%F").to_string(),
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct GameMetadata {
    title: String,
    event: String,
    event_link: Option<String>,
    url: String,
    img: String,
    published: String,
}
