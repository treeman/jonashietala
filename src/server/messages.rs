use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum WebEvent {
    RefreshAll,
    RefreshPage {
        path: String,
    },
    PositionPage {
        path: String,
        linenum: u32,
        linecount: u32,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "id")]
pub enum NeovimEvent {
    CursorMoved {
        linenum: u32,
        linecount: u32,
        column: u32,
        path: String,
    },
    ListTags {
        message_id: u64,
    },
    ListUrls {
        message_id: u64,
    },
    ListPosts {
        message_id: u64,
    },
}

#[derive(Debug, Serialize)]
pub struct PostInfo {
    pub title: String,
    pub url: String,
    pub tags: Vec<String>,
    pub series: Option<String>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "id")]
pub enum NeovimResponse {
    ListPosts {
        message_id: u64,
        posts: Vec<PostInfo>,
    },
}
