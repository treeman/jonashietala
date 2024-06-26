use chrono::NaiveDate;
use eyre::Result;
use itemref_derive::ItemRef;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::cmp::{Ordering, Reverse};
use std::collections::BTreeMap;
use tera::Context;

use crate::context::{LoadContext, RenderContext};
use crate::item::TeraItem;
use crate::markup::find_markup_files;
use crate::markup::{Html, MarkupFile, MarkupLookup, ParseContext, RawMarkupFile};
use crate::paths::{AbsPath, FilePath};
use crate::site_url::SiteUrl;

#[derive(Debug)]
pub struct ProjectsItem {
    prematter: Html,
    markup_lookup: Option<MarkupLookup>,
    path: AbsPath,
    pub title: String,
    pub url: SiteUrl,
    pub projects: BTreeMap<ProjectRef, Project>,
    pub games: BTreeMap<GameRef, Game>,
}

impl ProjectsItem {
    pub fn new(dir: &AbsPath, context: &LoadContext) -> Result<Self> {
        let raw_markup = RawMarkupFile::from_file(dir.join("projects.dj"))?;

        let meta_line_count = raw_markup.meta_line_count;
        let markup: MarkupFile<ProjectsMetadata> = raw_markup.parse(ParseContext::new(
            context.opts.generate_markup_lookup,
            meta_line_count,
        ))?;

        let title = markup.markup_meta.title.clone();

        let project_files = find_markup_files(&context.opts.input_dir, &[dir.join("projects")]);

        let is_game = |path: &FilePath| path.rel_path.starts_with("projects/games");

        let projects = project_files
            .iter()
            .filter(|path| !is_game(path))
            .map(|path| {
                Project::from_file(path.abs_path(), context.opts.generate_markup_lookup)
                    .map(|p| (p.project_ref(), p))
            })
            .collect::<Result<BTreeMap<ProjectRef, Project>>>()?;

        let games = project_files
            .iter()
            .filter(|path| is_game(path))
            .map(|path| {
                Game::from_file(path.abs_path(), context.opts.generate_markup_lookup)
                    .map(|g| (g.game_ref(), g))
            })
            .collect::<Result<BTreeMap<GameRef, Game>>>()?;

        Ok(Self {
            url: Self::url(),
            prematter: markup.html,
            markup_lookup: markup.markup_lookup,
            path: markup.path,
            title,
            projects,
            games,
        })
    }

    pub fn url() -> SiteUrl {
        SiteUrl::parse("/projects").expect("Should be able to create a url")
    }

    pub fn find_lookup_by_path<'a>(&'a self, path: &AbsPath) -> Option<&'a MarkupLookup> {
        if self.path == *path {
            return self.markup_lookup.as_ref();
        }
        for project in self.projects.values() {
            if project.path == *path {
                return project.markup_lookup.as_ref();
            }
        }
        for game in self.games.values() {
            if game.path == *path {
                return game.markup_lookup.as_ref();
            }
        }
        None
    }
}

impl TeraItem for ProjectsItem {
    fn context(&self, _ctx: &RenderContext) -> Context {
        Context::from_serialize(ProjectsContext {
            title: html_escape::encode_text(&self.title),
            prematter: &self.prematter.0,
            projects: self.projects.values().map(ProjectContext::from).collect(),
            games: self.games.values().map(GameContext::from).collect(),
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

#[derive(ItemRef, Debug, Clone)]
#[item(Project)]
pub struct ProjectRef {
    pub id: String,
    #[order]
    pub path: Reverse<AbsPath>,
}

#[derive(Debug)]
pub struct Project {
    title: String,
    link: Option<String>,
    year: u32,
    path: AbsPath,
    descr: Html,
    markup_lookup: Option<MarkupLookup>,
    pub homepage: bool,
}

impl Project {
    fn from_file(path: AbsPath, create_lookup: bool) -> Result<Self> {
        let markup = RawMarkupFile::from_file(path)?;
        Self::from_markup(markup, create_lookup)
    }

    pub fn from_markup(
        markup: RawMarkupFile<ProjectMetadata>,
        create_lookup: bool,
    ) -> Result<Self> {
        let meta_line_count = markup.meta_line_count;
        let markup = markup.parse(ParseContext::new(create_lookup, meta_line_count))?;

        Ok(Self {
            title: markup.markup_meta.title,
            link: markup.markup_meta.link,
            year: markup.markup_meta.year,
            path: markup.path,
            descr: markup.html,
            homepage: markup.markup_meta.homepage.unwrap_or(false),
            markup_lookup: markup.markup_lookup,
        })
    }

    pub fn id(&self) -> Cow<str> {
        return Cow::Borrowed(self.title.as_str());
    }

    pub fn project_ref(&self) -> ProjectRef {
        ProjectRef {
            id: self.id().to_string(),
            path: Reverse(self.path.clone()),
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

impl<'a> ProjectContext<'a> {
    pub fn from_ref(project_ref: &ProjectRef, ctx: &'a RenderContext) -> Self {
        ctx.content
            .projects
            .projects
            .get(project_ref)
            .expect("Should have project")
            .into()
    }
}

impl<'a> From<&'a Project> for ProjectContext<'a> {
    fn from(project: &'a Project) -> Self {
        Self {
            title: html_escape::encode_text(&project.title),
            link: project.link.as_deref(),
            year: project.year,
            descr: &project.descr.0,
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct ProjectMetadata {
    title: String,
    link: Option<String>,
    year: u32,
    homepage: Option<bool>,
}

#[derive(ItemRef, Debug, Clone)]
#[item(Game)]
pub struct GameRef {
    pub id: String,
    #[order]
    pub published: NaiveDate,
}

#[derive(Debug)]
pub struct Game {
    title: String,
    event: String,
    event_link: Option<String>,
    url: SiteUrl,
    path: AbsPath,
    img: SiteUrl,
    published: NaiveDate,
    markup_lookup: Option<MarkupLookup>,
}

impl Game {
    fn from_file(path: AbsPath, create_lookup: bool) -> Result<Self> {
        let markup = RawMarkupFile::from_file(path)?;
        Self::from_markup(markup, create_lookup)
    }

    pub fn from_markup(markup: RawMarkupFile<GameMetadata>, create_lookup: bool) -> Result<Self> {
        let meta_line_count = markup.meta_line_count;
        let markup = markup.parse(ParseContext::new(create_lookup, meta_line_count))?;

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
            markup_lookup: markup.markup_lookup,
        })
    }

    pub fn id(&self) -> Cow<str> {
        return Cow::Borrowed(self.title.as_str());
    }

    pub fn game_ref(&self) -> GameRef {
        GameRef {
            id: self.id().to_string(),
            published: self.published.clone(),
        }
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

impl Eq for Game {}

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

impl<'a> GameContext<'a> {
    pub fn from_ref(game_ref: &GameRef, ctx: &'a RenderContext) -> Self {
        ctx.content
            .projects
            .games
            .get(game_ref)
            .expect("Should have game")
            .into()
    }
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
