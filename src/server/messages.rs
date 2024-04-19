use super::complete::CompletionItemKind;
use crate::content::SeriesItem;
use crate::content::StandaloneItem;
use crate::content::Tag;
use crate::content::{PostItem, PostRef};
use crate::markup::markup_lookup::{BrokenLink, Heading, LinkDef};
use crate::Site;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum WebEvent {
    Refresh,
    PositionPage {
        path: String,
        linenum: usize,
        linecount: usize,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "id")]
pub enum NeovimEvent {
    CursorMoved {
        linenum: usize,
        linecount: usize,
        column: usize,
        path: String,
    },
    ListTags {
        message_id: u64,
    },
    // ListSeries {
    //     message_id: u64,
    // },
    // ListUrls {
    //     message_id: u64,
    // },
    // ListLinkDefs {
    //     message_id: u64,
    //     path: String,
    // },
    // ListBrokenLinks {
    //     message_id: u64,
    //     path: String,
    // },
    // ListHeadings {
    //     message_id: u64,
    //     path: String,
    // },
    Complete {
        message_id: u64,
        path: String,
        cursor_before_line: String,
        col: usize,
        row: usize,
    },
    GotoDef {
        message_id: u64,
        linenum: usize,
        column: usize,
        path: String,
    },
    RefreshDiagnostics {
        path: String,
    },
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
pub struct ImgInfo {
    pub url: String,
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
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct StandaloneInfo {
    pub title: String,
    pub url: String,
    pub path: String,
}

impl From<&StandaloneItem> for StandaloneInfo {
    fn from(item: &StandaloneItem) -> Self {
        StandaloneInfo {
            title: item.title.to_string(),
            url: item.url.href().to_string(),
            path: item.path.to_string(),
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
    pub url: String,
    pub path: String,
    pub posts: Vec<PostInfo>,
}

impl SeriesInfo {
    pub fn from(item: &SeriesItem, site: &Site) -> Self {
        Self {
            id: item.id.clone(),
            title: item.title.to_string(),
            url: item.url.href().to_string(),
            path: item.path.to_string(),
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
    pub fn from_link(link: &BrokenLink, row: usize) -> Self {
        Self {
            tag: link.tag.clone(),
            row,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum ExtraCompletionInfo {
    Post(PostInfo),
    Standalone(StandaloneInfo),
    Constant(ConstantInfo),
    Series(SeriesInfo),
    Tag(TagInfo),
    Img(ImgInfo),
    Heading(HeadingInfo),
    LinkDef(LinkDefInfo),
    BrokenLink(BrokenLinkInfo),
}

#[derive(Debug, Serialize, PartialEq, Eq, Default)]
#[serde(rename_all = "camelCase")]
pub struct CompletionItem {
    // These are used by cmp.
    pub label: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub insert_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filter_text: Option<String>,
    pub kind: CompletionItemKind,

    // Metadata used to construct documentation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub info: Option<ExtraCompletionInfo>,
}

#[derive(Debug, Serialize)]
pub struct Diagnostic {
    pub linenum: usize,
    pub end_linenum: usize,
    pub column: usize,
    pub end_column: usize,
    pub message: String,
}

#[derive(Debug, Serialize)]
#[serde(tag = "id")]
pub enum NeovimResponse {
    ListTags {
        message_id: u64,
        tags: Vec<TagInfo>,
    },
    // ListSeries {
    //     message_id: u64,
    //     series: Vec<SeriesInfo>,
    // },
    // ListUrls {
    //     message_id: u64,
    //     urls: Vec<UrlInfo>,
    // },
    // ListLinkDefs {
    //     message_id: u64,
    //     defs: Vec<LinkDefInfo>,
    // },
    // ListBrokenLinks {
    //     message_id: u64,
    //     links: Vec<BrokenLinkInfo>,
    // },
    // ListHeadings {
    //     message_id: u64,
    //     headings: Vec<HeadingInfo>,
    // },
    Complete {
        message_id: u64,
        completion_items: Vec<CompletionItem>,
    },
    GotoDef {
        message_id: u64,
        linenum: Option<usize>,
        column: Option<usize>,
        path: Option<String>,
    },
    Diagnostics {
        diagnostics: HashMap<String, Vec<Diagnostic>>,
    },
}
