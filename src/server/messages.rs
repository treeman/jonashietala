use super::complete::{CompletionItem, TagInfo};
use super::diagnostics::Diagnostic;
use crate::markup::markup_lookup::Element;
use serde::{Deserialize, Serialize};
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
        #[allow(dead_code)]
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
    CursorInfo {
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
    CursorInfo {
        message_id: u64,
        #[serde(skip_serializing_if = "Option::is_none")]
        element: Option<Element>,
    },
    Diagnostics {
        diagnostics: HashMap<String, Vec<Diagnostic>>,
    },
}
