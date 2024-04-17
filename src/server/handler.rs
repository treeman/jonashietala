use super::complete;
use super::goto_def::{self, GotoDefRes};
use super::messages::{CompletionItem, NeovimEvent, NeovimResponse, TagInfo, WebEvent};
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
        // NeovimEvent::ListUrls { message_id } => {
        //     let mut urls = Vec::new();

        //     for post in site.content.posts.values() {
        //         urls.push(UrlInfo {
        //             url: post.url.href().to_string(),
        //             title: post.title.clone(),
        //         });
        //     }

        //     for series in site.content.series.values() {
        //         urls.push(UrlInfo {
        //             url: series.url.href().to_string(),
        //             title: series.title.clone(),
        //         });
        //     }

        //     for standalone in site.content.standalones.iter() {
        //         urls.push(UrlInfo {
        //             url: standalone.url.href().to_string(),
        //             title: standalone.title.clone(),
        //         });
        //     }

        //     for tag in site.lookup.tags.keys() {
        //         urls.push(UrlInfo {
        //             url: tag.url.href().to_string(),
        //             title: tag.name.clone(),
        //         });
        //     }

        //     urls.push(UrlInfo {
        //         url: site.content.projects.url.href().to_string(),
        //         title: site.content.projects.title.clone(),
        //     });

        //     Some(Response::Reply(NeovimResponse::ListUrls {
        //         message_id,
        //         urls,
        //     }))
        // }
        // NeovimEvent::ListSeries { message_id } => {
        //     let series = site
        //         .content
        //         .series
        //         .values()
        //         .map(|series| SeriesInfo {
        //             id: series.id.clone(),
        //             title: series.title.clone(),
        //             url: series.url.href().to_string(),
        //             posts: series
        //                 .posts
        //                 .iter()
        //                 .map(|post_ref| {
        //                     site.content
        //                         .get_post(&post_ref.0)
        //                         .expect("Series references non-existent post")
        //                         .into()
        //                 })
        //                 .collect(),
        //         })
        //         .collect();

        //     Some(Response::Reply(NeovimResponse::ListSeries {
        //         message_id,
        //         series,
        //     }))
        // }
        // NeovimEvent::ListLinkDefs { message_id, path } => {
        //     // FIXME should reply with an error if path not found
        //     let lookup = site.content.find_post_lookup(&path)?;
        //     let defs = lookup.link_defs.values().map(Into::into).collect();

        //     Some(Response::Reply(NeovimResponse::ListLinkDefs {
        //         message_id,
        //         defs,
        //     }))
        // }
        // NeovimEvent::ListBrokenLinks { message_id, path } => {
        //     // FIXME should reply with an error if path not found
        //     let lookup = site.content.find_post_lookup(&path)?;
        //     let links = lookup.broken_links.iter().map(Into::into).collect();

        //     Some(Response::Reply(NeovimResponse::ListBrokenLinks {
        //         message_id,
        //         links,
        //     }))
        // }
        // NeovimEvent::ListHeadings { message_id, path } => {
        //     // FIXME should reply with an error if path not found
        //     let lookup = site.content.find_post_lookup(&path)?;
        //     let headings = lookup.headings.values().map(Into::into).collect();

        //     Some(Response::Reply(NeovimResponse::ListHeadings {
        //         message_id,
        //         headings,
        //     }))
        // }
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
