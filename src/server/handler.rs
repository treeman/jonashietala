use super::messages::{NeovimEvent, NeovimResponse, SeriesInfo, TagInfo, UrlInfo, WebEvent};
use crate::markup::markup_lookup::{ElementInfo, Link, LinkDef, LinkRef};
use crate::paths::AbsPath;
use crate::{markup::MarkupLookup, site::Site};
use camino::Utf8PathBuf;
use lazy_static::lazy_static;
use regex::Regex;
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
            let mut urls = Vec::new();

            for post in site.content.posts.values() {
                urls.push(UrlInfo {
                    url: post.url.href().to_string(),
                    title: post.title.clone(),
                });

                // if let Some(ref lookup) = post.markup_lookup {
                //     for heading in lookup.headings.values() {
                //         urls.push(UrlInfo {
                //             url: format!("{}#{}", post.url.href(), heading.id),
                //             title: post.title.clone(),
                //         });
                //     }
                // }
            }
            // TODO
            // Site content:
            // Series, standalones, tags, projects

            Some(Response::Reply(NeovimResponse::ListUrls {
                message_id,
                urls,
            }))
        }
        NeovimEvent::ListSeries { message_id } => {
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
        NeovimEvent::ListLinkDefs { message_id, path } => {
            // FIXME should reply with an error if path not found
            let lookup = get_post_lookup(&site, &path)?;
            let defs = lookup.link_defs.values().map(Into::into).collect();

            Some(Response::Reply(NeovimResponse::ListLinkDefs {
                message_id,
                defs,
            }))
        }
        NeovimEvent::ListHeadings { message_id, path } => {
            // FIXME should reply with an error if path not found
            let lookup = get_post_lookup(&site, &path)?;
            let headings = lookup.headings.values().map(Into::into).collect();

            Some(Response::Reply(NeovimResponse::ListHeadings {
                message_id,
                headings,
            }))
        }
        NeovimEvent::GotoDef {
            message_id,
            linenum,
            column,
            path,
        } => match goto_def(linenum, column, &path, &site) {
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
    }
}

fn get_post_lookup<'a>(site: &'a Site, path: &str) -> Option<&'a MarkupLookup> {
    let path = Utf8PathBuf::from(path);

    site.content
        .find_post_by_file_name(path.file_name()?)
        .and_then(|post| post.markup_lookup.as_ref())
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum GotoDefRes {
    OtherFile { path: AbsPath },
    SameFile { row: usize, col: usize },
}

fn goto_def(linenum: usize, column: usize, path: &str, site: &Site) -> Option<GotoDefRes> {
    let lookup = get_post_lookup(&site, &path)?;

    match lookup.element_at(linenum, column)? {
        ElementInfo::Link(Link {
            link_ref: LinkRef::Inline(url),
            ..
        }) => goto_url(url, lookup, site),
        ElementInfo::Link(Link {
            link_ref: LinkRef::Reference { label, url },
            ..
        }) => {
            if let Some(def) = lookup.link_defs.get(label) {
                let (row, col) = lookup.char_pos_to_row_col(def.range.start);
                Some(GotoDefRes::SameFile { row, col })
            } else {
                // May happen for short heading links
                goto_url(url, lookup, site)
            }
        }
        ElementInfo::Link(Link {
            link_ref: LinkRef::AutoLink(url),
            ..
        }) => goto_url(url, lookup, site),
        ElementInfo::Link(_) => None,
        ElementInfo::LinkDef(LinkDef { url, .. }) => goto_url(url, lookup, site),
        ElementInfo::Heading(_) => None,
    }
}

lazy_static! {
    static ref HASH_REF: Regex = Regex::new("^#(.+)$").unwrap();
}

fn goto_url(url: &str, lookup: &MarkupLookup, site: &Site) -> Option<GotoDefRes> {
    let hash_ref = HASH_REF.captures(url.trim());
    if let Some(hash_match) = hash_ref {
        if let Some(heading) = lookup.headings.get(&hash_match[1]) {
            let (row, col) = lookup.char_pos_to_row_col(heading.range.start);
            return Some(GotoDefRes::SameFile { row, col });
        }
    }

    if let Some(post) = site.content.find_post_by_url(url) {
        return Some(GotoDefRes::OtherFile {
            path: post.path.clone().into(),
        });
    }

    for heading in lookup.headings.values() {
        if heading.content == url {
            let (row, col) = lookup.char_pos_to_row_col(heading.range.start);
            return Some(GotoDefRes::SameFile { row, col });
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    use eyre::Result;

    #[test]
    fn test_goto_link_def() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let feb_post = "posts/2022-02-01-feb_post.dj";

        let feb_post2 = "posts/2022-02-02-feb_post2.dj";
        let abs_feb_post2 = test_site.input_path(feb_post2);

        // Goto link def from full reference link
        assert_eq!(
            goto_def(9, 17, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 13, col: 1 })
        );

        // Goto link def from collapsed reference
        assert_eq!(
            goto_def(11, 3, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 13, col: 1 })
        );

        // Goto link def from collapsed reference
        assert_eq!(
            goto_def(11, 3, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 13, col: 1 })
        );

        // Goto other post from link def
        assert_eq!(
            goto_def(13, 9, feb_post, &test_site.site),
            Some(GotoDefRes::OtherFile {
                path: abs_feb_post2.clone()
            })
        );

        // Goto other post from inline link
        assert_eq!(
            goto_def(15, 6, feb_post, &test_site.site),
            Some(GotoDefRes::OtherFile {
                path: abs_feb_post2.clone()
            })
        );

        // Goto explicit heading from inline link
        assert_eq!(
            goto_def(21, 17, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 17, col: 1 })
        );

        // Goto heading from link def
        assert_eq!(
            goto_def(23, 8, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 17, col: 1 })
        );

        // Goto short heading
        assert_eq!(
            goto_def(19, 4, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 17, col: 1 })
        );

        // Above markup, inside frontmatter
        assert_eq!(goto_def(1, 1, feb_post, &test_site.site), None);

        // Past eof
        assert_eq!(goto_def(99999999, 1, feb_post, &test_site.site), None);

        // Zero column on a non-zero line
        assert_eq!(goto_def(11, 0, feb_post, &test_site.site), None);

        // Past column
        assert_eq!(goto_def(11, 9999999, feb_post, &test_site.site), None);

        Ok(())
    }
}
