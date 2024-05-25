use super::complete;
use super::complete::TagInfo;
use super::goto_def::{self, GotoDefRes};
use super::messages::{NeovimEvent, NeovimResponse, WebEvent};
use crate::server::{diagnostics, info};
use crate::site::Site;
use std::sync::{Arc, Mutex};
use tracing::{debug, warn};

#[derive(Debug)]
pub enum Response {
    Web(WebEvent),
    Reply(NeovimResponse),
}

pub fn handle_msg<'a>(msg: NeovimEvent, site: Arc<Mutex<Site>>) -> Option<Response> {
    debug!("Msg received: {:?}", msg);
    let site = site.lock().expect("site lock failed");

    match msg {
        NeovimEvent::CursorMoved {
            linenum,
            linecount,
            path,
            ..
        } => {
            if let Some(url) = site.path_to_url(path.as_str()) {
                Some(Response::Web(WebEvent::PositionPage {
                    linenum,
                    linecount,
                    url: url.href().to_string(),
                }))
            } else {
                warn!("Unknown path for cursor moved: {}", path);
                None
            }
        }

        NeovimEvent::ListTags { message_id } => {
            let tags = site
                .lookup
                .tags
                .iter()
                .map(|(tag, post_refs)| TagInfo::from_tag(tag, post_refs, &site))
                .collect();

            Some(Response::Reply(NeovimResponse::ListTags {
                message_id,
                tags,
            }))
        }
        NeovimEvent::Complete {
            message_id,
            path,
            cursor_before_line,
            col,
            row,
        } => Some(Response::Reply(NeovimResponse::Complete {
            message_id,
            completion_items: complete::complete(&cursor_before_line, col, row, &path, &site),
        })),
        NeovimEvent::GotoDef {
            message_id,
            linenum,
            column,
            path,
        } => match goto_def::goto_def(linenum, column, &path, &site) {
            Some(GotoDefRes::SameFile { row, col }) => {
                Some(Response::Reply(NeovimResponse::GotoDef {
                    message_id,
                    linenum: Some(row),
                    column: Some(col),
                    path: None,
                }))
            }
            Some(GotoDefRes::OtherFile { path }) => {
                Some(Response::Reply(NeovimResponse::GotoDef {
                    message_id,
                    linenum: None,
                    column: None,
                    path: Some(path.to_string()),
                }))
            }
            None => Some(Response::Reply(NeovimResponse::GotoDef {
                message_id,
                linenum: None,
                column: None,
                path: None,
            })),
        },
        NeovimEvent::CursorInfo {
            message_id,
            linenum,
            column,
            path,
        } => Some(Response::Reply(NeovimResponse::CursorInfo {
            message_id,
            element: info::item_info(linenum, column, &path, &site),
        })),
        NeovimEvent::RefreshDiagnostics { path } => {
            diagnostics::generate_file_diagnostics(&path.as_str().into(), &site).and_then(
                |path_diagnostics| {
                    Some(Response::Reply(NeovimResponse::Diagnostics {
                        diagnostics: [(path, path_diagnostics)].into(),
                    }))
                },
            )
        }
    }
}
