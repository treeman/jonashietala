use crate::content::PostItem;
use crate::markup::markup_lookup::{BrokenLink, Heading, LinkDef};
use crate::paths::AbsPath;
use crate::site_url::SiteUrl;
use serde::{Deserialize, Serialize};
use serde_repr::*;
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

#[derive(Debug, Serialize)]
pub struct TagInfo {
    pub id: String,
    pub name: String,
    pub url: String,
    pub posts: Vec<PostInfo>,
}

#[derive(Debug, Serialize)]
pub struct PostInfo {
    pub title: String,
    pub url: String,
    pub tags: Vec<String>,
    pub series: Option<String>,
}

impl From<&PostItem> for PostInfo {
    fn from(post: &PostItem) -> Self {
        PostInfo {
            title: post.title.to_string(),
            url: post.url.href().to_string(),
            tags: post.tags.iter().map(|tag| tag.name.to_string()).collect(),
            series: post.series.as_ref().map(|x| x.id.clone()),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct UrlInfo {
    pub title: String,
    pub url: String,
}

#[derive(Debug, Serialize)]
pub struct SeriesInfo {
    pub id: String,
    pub title: String,
    pub url: String,
    pub posts: Vec<PostInfo>,
}

#[derive(Debug, Serialize)]
pub struct LinkDefInfo {
    pub label: String,
    pub url: String,
}

impl From<&LinkDef> for LinkDefInfo {
    fn from(def: &LinkDef) -> Self {
        LinkDefInfo {
            label: def.label.clone(),
            url: def.url.clone(),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct BrokenLinkInfo {
    pub tag: String,
}

impl From<&BrokenLink> for BrokenLinkInfo {
    fn from(link: &BrokenLink) -> Self {
        Self {
            tag: link.tag.clone(),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct HeadingInfo {
    pub id: String,
    pub content: String,
}

impl From<&Heading> for HeadingInfo {
    fn from(heading: &Heading) -> Self {
        HeadingInfo {
            id: heading.id.clone(),
            content: heading.content.clone(),
        }
    }
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

pub struct CompletionItemBuilder {
    pub label: String,
    pub kind: CompletionItemKind,
    pub label_tag: Option<String>,
    pub filter_text: Option<String>,
    pub insert_text: Option<String>,
    pub path: Option<AbsPath>,
}

impl CompletionItemBuilder {
    pub fn from_url(label: &str, url: &SiteUrl, kind: CompletionItemKind) -> Self {
        let href = url.href();
        Self {
            label: label.to_string(),
            insert_text: Some(href.to_string()),
            kind,
            ..Default::default()
        }
        .add_filter(href.as_ref())
        .add_filter(label)
    }

    pub fn with_label_tag(mut self, tag: &str) -> Self {
        self.label_tag = Some(tag.into());
        self
    }

    pub fn with_path(mut self, path: &AbsPath) -> Self {
        self.path = Some(path.to_owned());
        self
    }

    pub fn add_filter(mut self, txt: &str) -> Self {
        if let Some(mut s) = self.filter_text {
            s.push('|');
            s.push_str(txt);
            self.filter_text = Some(s);
        } else {
            self.filter_text = Some(txt.to_string());
        }
        self
    }
}

impl Default for CompletionItemBuilder {
    fn default() -> Self {
        CompletionItemBuilder {
            label: "".into(),
            kind: CompletionItemKind::Text,
            label_tag: None,
            filter_text: None,
            insert_text: None,
            path: None,
        }
    }
}

impl Into<CompletionItem> for CompletionItemBuilder {
    fn into(self) -> CompletionItem {
        let label = if let Some(tag) = self.label_tag {
            format!("{}: {}", tag, self.label)
        } else {
            self.label
        };

        CompletionItem {
            label,
            insert_text: self.insert_text,
            filter_text: self.filter_text,
            kind: self.kind,
            path: self.path.map(|path| path.to_string()),
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct CompletionItem {
    pub label: String,
    pub insert_text: Option<String>,
    pub filter_text: Option<String>,
    pub kind: CompletionItemKind,
    pub path: Option<String>,
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
