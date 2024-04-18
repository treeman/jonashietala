use super::complete;
use super::goto_def::{self, GotoDefRes};
use super::messages::{NeovimEvent, NeovimResponse, TagInfo, WebEvent};
use crate::site::Site;
use camino::Utf8PathBuf;
use std::sync::{Arc, Mutex};
use tracing::debug;

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
            let path = Utf8PathBuf::from(path);
            Some(Response::Web(WebEvent::PositionPage {
                linenum,
                linecount,
                path: path.to_string(),
            }))
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
        NeovimEvent::RefreshDiagnostics { path } => {
            let diagnostics = site.collect_paths_diagnostics([&path.as_str().into()].into_iter());
            Some(Response::Reply(NeovimResponse::Diagnostics { diagnostics }))
        }
    }
}
