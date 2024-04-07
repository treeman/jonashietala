use super::messages::{NeovimEvent, NeovimResponse, SeriesInfo, TagInfo, UrlInfo, WebEvent};
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

    match msg {
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
            let site = site.lock().expect("site lock failed");

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
                            site.content
                                .posts
                                .get(post_ref)
                                .expect("Tag references non-existent post")
                                .into()
                        })
                        .collect(),
                })
                .collect();

            Some(Response::Reply(NeovimResponse::ListTags {
                message_id,
                tags,
            }))
        }
        NeovimEvent::ListUrls { message_id } => {
            let site = site.lock().expect("site lock failed");

            let mut urls = Vec::new();

            for post in site.content.posts.values() {
                urls.push(UrlInfo {
                    url: post.url.href().to_string(),
                    title: post.title.clone(),
                });
            }
            // TODO
            // Site content:
            // Series, standalones, tags, projects
            // Files:
            // fonts, images, static

            Some(Response::Reply(NeovimResponse::ListUrls {
                message_id,
                urls,
            }))
        }
        NeovimEvent::ListSeries { message_id } => {
            let site = site.lock().expect("site lock failed");

            let series = site
                .content
                .series
                .values()
                .map(|series| SeriesInfo {
                    id: series.id.clone(),
                    title: series.title.clone(),
                    url: series.url.href().to_string(),
                    posts: series
                        .posts
                        .iter()
                        .map(|post_ref| {
                            site.content
                                .get_post(&post_ref.0)
                                .expect("Series references non-existent post")
                                .into()
                        })
                        .collect(),
                })
                .collect();

            Some(Response::Reply(NeovimResponse::ListSeries {
                message_id,
                series,
            }))
        }
    }
}
