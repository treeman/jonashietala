use super::messages::{NeovimEvent, NeovimResponse, PostInfo, TagInfo, WebEvent};
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

        NeovimEvent::ListTags { message_id } => {
            let site = site.lock().expect("To JsEvent failed");

            let tags = site
                .lookup
                .tags
                .iter()
                .map(|(tag, post_refs)| TagInfo {
                    id: tag.id.clone(),
                    name: tag.name.clone(),
                    url: tag.url.href().to_string(),
                    posts: post_refs
                        .iter()
                        .map(|post_ref| {
                            let post = site
                                .content
                                .posts
                                .get(post_ref)
                                .expect("Tag references non-existent post");
                            PostInfo {
                                title: post.title.to_string(),
                                url: post.url.href().to_string(),
                                tags: post.tags.iter().map(|tag| tag.name.to_string()).collect(),
                                series: post.series.as_ref().map(|x| x.id.clone()),
                            }
                        })
                        .collect(),
                })
                .collect();

            Some(Response::Reply(NeovimResponse::ListTags {
                message_id,
                tags,
            }))
        }
        _ => None,
    }
}
