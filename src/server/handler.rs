use super::messages::{NeovimEvent, NeovimResponse, WebEvent};
use crate::site::Site;
use camino::Utf8PathBuf;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum Response {
    Web(WebEvent),
    Reply(NeovimResponse),
}

pub fn handle_msg(msg: NeovimEvent, _site: &Arc<Mutex<Site>>) -> Option<Response> {
    match dbg!(msg) {
        NeovimEvent::CursorMoved {
            linenum,
            linecount,
            path,
            ..
        } => {
            // let site = site.lock().expect("To JsEvent failed");
            let path = Utf8PathBuf::from(path);
            // This works:
            // dbg!(&site.content.find_post(&path.file_name().unwrap()));
            // dbg!(&site
            //     .content
            //     .find_post(&site.file_path(path.clone().into())?.rel_path.0.as_str()));
            Some(Response::Web(WebEvent::PositionPage {
                linenum,
                linecount,
                path: path.to_string(),
            }))
        }
        _ => None,
    }
}
