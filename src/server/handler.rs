use super::messages::{NeovimEvent, NeovimResponse, PostInfo, WebEvent};
use crate::site::Site;
use camino::Utf8PathBuf;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum Response {
    Web(WebEvent),
    Reply(NeovimResponse),
}

pub fn handle_msg<'a>(msg: NeovimEvent, site: Arc<Mutex<Site>>) -> Option<Response> {
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

        NeovimEvent::ListPosts { message_id } => {
            let site = site.lock().expect("To JsEvent failed");
            Some(Response::Reply(NeovimResponse::ListPosts {
                message_id,
                // posts: vec![],
                posts: site
                    .content
                    .posts
                    .iter()
                    .take(2) // FIXME temp
                    .map(|(_, item)| PostInfo {
                        title: item.title.to_string(),
                        url: item.url.href().to_string(),
                        tags: item.tags.iter().map(|tag| tag.name.to_string()).collect(),
                        series: item.series.as_ref().map(|x| x.id.clone()),
                    })
                    .collect(),
            }))
        }
        _ => None,
    }
}
