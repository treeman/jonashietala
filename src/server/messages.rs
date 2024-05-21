use super::diagnostics::Diagnostic;
use crate::content::PartialPostItem;
use crate::content::SeriesItem;
use crate::content::StandaloneItem;
use crate::content::Tag;
use crate::content::{PostItem, PostRef};
use crate::markup::markup_lookup::{Heading, LinkDef};
use crate::Site;
use serde::{Deserialize, Serialize};
use serde_repr::Serialize_repr;
use std::collections::HashMap;

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum WebEvent {
    Refresh,
    PositionPage {
        url: String,
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
    pub modified: u64,
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

impl From<PartialPostItem> for PostInfo {
    fn from(post: PartialPostItem) -> Self {
        PostInfo {
            title: post.title,
            path: post.path.to_string(),
            url: post.url.href().to_string(),
            created: post.created.format("%F").to_string(),
            tags: post.tags.iter().map(|tag| tag.name.to_string()).collect(),
            series: post.series_id,
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
    pub fn from_link(tag: &str, row: usize) -> Self {
        Self {
            tag: tag.to_string(),
            row,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct DivClassInfo {
    pub name: &'static str,
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
    DivClass(DivClassInfo),
}

#[allow(dead_code)]
#[derive(Debug, Serialize_repr, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CompletionItemKind {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25,
}

impl Default for CompletionItemKind {
    fn default() -> Self {
        Self::Text
    }
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

    // Blog specific metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(flatten)]
    pub info: Option<ExtraCompletionInfo>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "id")]
pub enum NeovimResponse {
    ListTags {
        message_id: u64,
        tags: Vec<TagInfo>,
    },
    Complete {
        message_id: u64,
        completion_items: Vec<CompletionItem>,
    },
    GotoDef {
        message_id: u64,
        #[serde(skip_serializing_if = "Option::is_none")]
        linenum: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        column: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        path: Option<String>,
    },
    Diagnostics {
        diagnostics: HashMap<String, Vec<Diagnostic>>,
    },
}
