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
    ListSeries {
        message_id: u64,
    },
    ListUrls {
        message_id: u64,
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

#[derive(Debug, Serialize)]
pub struct SeriesInfo {
    pub id: String,
    pub title: String,
    pub url: String,
    pub posts: Vec<PostInfo>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "id")]
pub enum NeovimResponse {
    ListTags {
        message_id: u64,
        tags: Vec<TagInfo>,
    },
    ListSeries {
        message_id: u64,
        series: Vec<SeriesInfo>,
    },
    ListUrls {
        message_id: u64,
        urls: Vec<String>,
    },
}
