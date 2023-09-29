use std::fs;

use chrono::NaiveDate;
use eyre::{eyre, Result};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::cmp::Ordering;
use tera::Context;
use yaml_front_matter::{Document, YamlFrontMatter};

use crate::{
    item::RenderContext, item::TeraItem, markdown::find_markdown_files, markdown::markdown_to_html,
    paths::AbsPath, site_url::SiteUrl,
};

#[derive(Debug)]
pub struct ProjectsItem {
    prematter: String,
    title: String,
    url: SiteUrl,
    pub projects: Vec<Project>,
    pub games: Vec<Game>,
}

impl ProjectsItem {
    pub fn new(dir: &AbsPath) -> Result<Self> {
        let raw_content = fs::read_to_string(dir.join("projects.markdown"))?;
        let Document { metadata, content } =
            YamlFrontMatter::parse::<ProjectsMetadata>(&raw_content)
                .map_err(|err| eyre!("Failed to parse metadata for projects\n{}", err))?;
        let url = SiteUrl::parse("/projects").expect("Should be able to create a url");

        let project_files = find_markdown_files(dir.join("projects"));

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
            prematter: content,
            title: metadata.title,
            projects,
            games,
        })
    }
}

impl TeraItem for ProjectsItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        Context::from_serialize(ProjectsContext {
            title: html_escape::encode_text(&self.title),

            prematter: markdown_to_html(&self.prematter),
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

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Deserialize, Debug)]
struct ProjectsMetadata {
    title: String,
}

#[derive(Debug, Serialize)]
struct ProjectsContext<'a> {
    title: Cow<'a, str>,
    prematter: String,
    projects: Vec<ProjectContext<'a>>,
    games: Vec<GameContext<'a>>,
}

#[derive(Debug, Eq, Clone)]
pub struct Project {
    title: String,
    link: Option<String>,
    year: u32,
    path: AbsPath,
    descr: String,
}

impl Project {
    fn from_file(path: AbsPath) -> Result<Self> {
        let raw_content = fs::read_to_string(&path)?;
        Self::from_string(path, raw_content)
    }

    pub fn from_string(path: AbsPath, raw_content: String) -> Result<Self> {
        let Document { metadata, content } =
            YamlFrontMatter::parse::<ProjectMetadata>(&raw_content)
                .map_err(|err| eyre!("Failed to parse metadata for project: {}\n{}", path, err))?;

        Ok(Self {
            title: metadata.title,
            link: metadata.link,
            year: metadata.year,
            path,
            descr: content,
        })
    }

    pub fn context<'a, 'cache>(&'a self, _ctx: &'cache RenderContext) -> ProjectContext<'a> {
        ProjectContext {
            title: html_escape::encode_text(&self.title),
            link: self.link.as_deref(),
            year: self.year,
            descr: markdown_to_html(&self.descr),
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
    descr: String,
}

#[derive(Deserialize, Debug)]
struct ProjectMetadata {
    title: String,
    link: Option<String>,
    year: u32,
}

#[derive(Debug, Eq)]
pub struct Game {
    title: String,
    event: String,
    event_link: Option<String>,
    url: SiteUrl,
    path: AbsPath,
    img: SiteUrl,
    published: NaiveDate,
}

impl Game {
    fn from_file(path: AbsPath) -> Result<Self> {
        let raw_content = fs::read_to_string(&path)?;
        Self::from_string(path, raw_content)
    }

    pub fn from_string(path: AbsPath, raw_content: String) -> Result<Self> {
        let Document {
            metadata,
            content: _,
        } = YamlFrontMatter::parse::<GameMetadata>(&raw_content)
            .map_err(|err| eyre!("Failed to parse metadata for game: {}\n{}", path, err))?;

        let published = NaiveDate::parse_from_str(&metadata.published, "%Y-%m-%d")?;
        let url = SiteUrl::parse(&metadata.url)?;
        let img = SiteUrl::parse(&metadata.img)?;

        Ok(Self {
            title: metadata.title,
            event: metadata.event,
            event_link: metadata.event_link,
            url,
            img,
            path,
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
struct GameContext<'a> {
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
            // FIXME
            // published: x.published.format("%FT%T%.fZ").to_string(),
            published: x.published.format("%F").to_string(),
        }
    }
}

#[derive(Deserialize, Debug)]
struct GameMetadata {
    title: String,
    event: String,
    event_link: Option<String>,
    url: String,
    img: String,
    published: String,
}
