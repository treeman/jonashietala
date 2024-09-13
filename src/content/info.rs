use crate::content::{
    Game, PartialPostItem, PartialProject, PartialProjectsItem, PartialStandaloneItem, PostItem,
    PostRef, SeriesItem, StandaloneItem, Tag,
};
use crate::markup::markup_lookup::{Heading, LinkDef};
use crate::paths::FilePath;
use crate::site::Site;
use eyre::eyre;
use serde::Serialize;

use super::series::PartialSeriesItem;

#[derive(Debug, Serialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum ContentInfo {
    Post(PostInfo),
    Standalone(StandaloneInfo),
    Game(GameInfo),
    Projects(ProjectsInfo),
    Project(ProjectInfo),
    Constant(ConstantInfo),
    Series(SeriesInfo),
    Tag(TagInfo),
    Img(ImgInfo),
    Heading(HeadingInfo),
    LinkDef(LinkDefInfo),
    BrokenLink(BrokenLinkInfo),
    DivClass(DivClassInfo),
    Symbol(SymbolInfo),
}

impl TryFrom<&FilePath> for ContentInfo {
    type Error = eyre::Error;

    fn try_from(path: &FilePath) -> Result<Self, Self::Error> {
        if path.rel_path.starts_with("posts/") || path.rel_path.starts_with("drafts/") {
            Ok(Self::Post(PartialPostItem::try_from(path)?.into()))
        } else if path.rel_path.starts_with("series/") {
            Ok(Self::Series(PartialSeriesItem::from_file(path)?.into()))
        } else if path.rel_path.starts_with("standalone/") {
            Ok(Self::Standalone(
                PartialStandaloneItem::from_file(path)?.into(),
            ))
        } else if path.rel_path == "projects.dj" {
            Ok(Self::Projects(PartialProjectsItem::from_file(path)?.into()))
        } else if path.rel_path.starts_with("projects/games/") {
            Ok(Self::Game(Game::from_file(path.abs_path())?.into()))
        } else if path.rel_path.starts_with("projects/") {
            Ok(Self::Project(
                PartialProject::from_file(path.abs_path())?.into(),
            ))
        } else {
            Err(eyre!("Couldn't convert {path} to ContentInfo"))
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct DivClassInfo {
    pub name: &'static str,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct SymbolInfo {
    pub sym: &'static str,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct TagInfo {
    pub id: String,
    pub name: String,
    pub url: String,
    pub posts: Vec<PostInfo>,
}

impl TagInfo {
    pub fn from_tag(tag: &Tag, posts: &[PostRef], site: &Site) -> Self {
        Self {
            id: tag.id.clone(),
            name: tag.name.to_string(),
            url: tag.url.href().to_string(),
            posts: posts
                .iter()
                .map(|post_ref| {
                    site.content
                        .posts
                        .get(post_ref)
                        .expect("Tag references non-existent post")
                        .into()
                })
                .collect(),
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct PostInfo {
    pub title: String,
    pub path: String,
    pub created: String,
    pub url: String,
    pub tags: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub series: Option<String>,
    pub is_draft: bool,
}

impl From<&PostItem> for PostInfo {
    fn from(post: &PostItem) -> Self {
        PostInfo {
            title: post.title.to_string(),
            path: post.path.to_string(),
            url: post.url.href().to_string(),
            created: post.created.format("%F").to_string(),
            tags: post.tags.iter().map(|tag| tag.name.to_string()).collect(),
            series: post.series.as_ref().map(|x| x.id.clone()),
            is_draft: post.is_draft,
        }
    }
}

impl From<PartialPostItem> for PostInfo {
    fn from(post: PartialPostItem) -> Self {
        PostInfo {
            title: post.title,
            path: post.path.to_string(),
            url: post.url.href().to_string(),
            created: post.created.format("%F").to_string(),
            tags: post.tags.iter().map(|tag| tag.name.to_string()).collect(),
            series: post.series_id,
            is_draft: post.is_draft,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct StandaloneInfo {
    pub title: String,
    pub url: String,
    pub path: String,
    pub is_draft: bool,
}

impl From<&StandaloneItem> for StandaloneInfo {
    fn from(item: &StandaloneItem) -> Self {
        StandaloneInfo {
            title: item.title.to_string(),
            url: item.url.href().to_string(),
            path: item.path.to_string(),
            is_draft: item.is_draft,
        }
    }
}

impl From<PartialStandaloneItem> for StandaloneInfo {
    fn from(item: PartialStandaloneItem) -> Self {
        StandaloneInfo {
            title: item.title.to_string(),
            url: item.url.href().to_string(),
            path: item.path.to_string(),
            is_draft: item.is_draft,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct ConstantInfo {
    pub title: String,
    pub url: String,
}

#[derive(Debug, Serialize)]
pub struct UrlInfo {
    pub title: String,
    pub url: String,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct SeriesInfo {
    pub id: String,
    pub title: String,
    pub completed: bool,
    pub url: String,
    pub path: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub posts: Vec<PostInfo>,
    pub homepage: bool,
}

impl SeriesInfo {
    pub fn from(item: &SeriesItem, site: &Site) -> Self {
        Self {
            id: item.id.clone(),
            title: item.title.to_string(),
            url: item.url.href().to_string(),
            path: item.path.to_string(),
            completed: item.completed,
            homepage: item.homepage,
            posts: item
                .posts
                .iter()
                .map(|post_ref| {
                    site.content
                        .posts
                        .get(&post_ref.0)
                        .expect("Series references non-existent post")
                        .into()
                })
                .collect(),
        }
    }
}

impl From<PartialSeriesItem> for SeriesInfo {
    fn from(item: PartialSeriesItem) -> Self {
        Self {
            id: item.id.clone(),
            title: item.title.to_string(),
            url: item.url.href().to_string(),
            path: item.path.to_string(),
            completed: item.completed,
            homepage: item.homepage,
            posts: Vec::new(), // FIXME we don't get the post list here
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum HeadingContext {
    OtherFile {
        path: String,
        url: String,
        start_row: usize,
        end_row: usize,
    },
    SameFile {
        start_row: usize,
        end_row: usize,
    },
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct HeadingInfo {
    pub id: String,
    pub content: String,
    pub level: u16,
    pub context: HeadingContext,
}

impl HeadingInfo {
    pub fn from_heading(heading: &Heading, context: HeadingContext) -> Self {
        Self {
            id: heading.id.clone(),
            content: heading.content.clone(),
            level: heading.level,
            context,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct LinkDefInfo {
    pub label: String,
    pub url: String,
    pub start_row: usize,
    pub end_row: usize,
}

impl LinkDefInfo {
    pub fn from_link_def(def: &LinkDef, start_row: usize, end_row: usize) -> Self {
        Self {
            label: def.label.clone(),
            url: def.url.clone(),
            start_row,
            end_row,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct BrokenLinkInfo {
    pub tag: String,
    pub row: usize,
}

impl BrokenLinkInfo {
    pub fn from_link(tag: &str, row: usize) -> Self {
        Self {
            tag: tag.to_string(),
            row,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct ImgInfo {
    pub url: String,
    pub modified: u64,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct GameInfo {
    title: String,
    event: String,
    event_link: Option<String>,
    url: String,
    path: String,
    published: String,
}

impl From<Game> for GameInfo {
    fn from(game: Game) -> Self {
        Self {
            title: game.title,
            event: game.event,
            event_link: game.event_link,
            url: game.url.href().to_string(),
            path: game.path.to_string(),
            published: game.published.format("%F").to_string(),
        }
    }
}

#[derive(Debug, Serialize, Eq, PartialEq)]
pub struct ProjectsInfo {
    pub title: String,
    pub path: String,
}

impl From<PartialProjectsItem> for ProjectsInfo {
    fn from(projects: PartialProjectsItem) -> Self {
        Self {
            title: projects.title,
            path: projects.path.to_string(),
        }
    }
}

#[derive(Debug, Serialize, Eq, PartialEq)]
pub struct ProjectInfo {
    pub title: String,
    pub link: Option<String>,
    pub year: u32,
    pub path: String,
    pub homepage: bool,
}

impl From<PartialProject> for ProjectInfo {
    fn from(project: PartialProject) -> Self {
        Self {
            title: project.title,
            link: project.link,
            year: project.year,
            path: project.path.to_string(),
            homepage: project.homepage,
        }
    }
}
