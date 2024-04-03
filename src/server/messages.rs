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
    ListTags,
    ListUrls,
    ListPosts,
}

#[derive(Debug, Serialize)]
#[serde(tag = "id")]
pub enum NeovimResponse {}
