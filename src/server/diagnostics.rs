use crate::item::Item;
use crate::markup::markup_lookup::{ElementInfo, ImgRef, LinkRef, NeovimRange};
use crate::paths::AbsPath;
use crate::site_url::SiteUrl;
use crate::Site;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Serialize)]
pub struct Diagnostic {
    pub lnum: usize,
    pub end_lnum: usize,
    pub col: usize,
    pub end_col: usize,
    pub message: String,
}

pub fn generate_diagnostics(items: &[&dyn Item], site: &Site) -> HashMap<String, Vec<Diagnostic>> {
    items
        .into_iter()
        .filter_map(|item| item.source_file())
        .filter_map(|path| {
            generate_file_diagnostics(path, site).and_then(|d| Some((path.to_string(), d)))
        })
        .collect()
}

pub fn generate_file_diagnostics(path: &AbsPath, site: &Site) -> Option<Vec<Diagnostic>> {
    let lookup = site.content.find_post_lookup_by_file_name(path.as_str())?;

    let mut res = Vec::new();

    for (_, e) in lookup.char_pos_to_element.iter() {
        match e {
            ElementInfo::Link(link) => match &link.link_ref {
                LinkRef::Inline(url) | LinkRef::AutoLink(url) => {
                    check_url(&link.range, url, site, &mut res);
                }
                LinkRef::Reference { .. } => {}
                LinkRef::Email(_) => {}
                LinkRef::Unresolved(tag) => {
                    push_diagnostic(
                        &link.range,
                        format!("Link to non-existent link definition: `{}`", tag),
                        &mut res,
                    );
                }
            },
            ElementInfo::Img(img) => match &img.link_ref {
                ImgRef::Inline(url) => {
                    check_url(&img.range, url, site, &mut res);
                }
                ImgRef::Reference { .. } => {}
                ImgRef::Unresolved(tag) => {
                    push_diagnostic(
                        &img.range,
                        format!("Link to non-existent link definition: `{}`", tag),
                        &mut res,
                    );
                }
            },
            _ => {}
        }
    }

    for defs in lookup.link_defs.values() {
        // Duplicate link definitions if there are multiple with the same label.
        let duplicate = defs.len() > 1;

        for def in defs.iter() {
            check_url(&def.range, &def.url, site, &mut res);

            if duplicate {
                push_diagnostic(
                    &def.range,
                    format!("Duplicate link definition: `{}`", def.label),
                    &mut res,
                );
            }
        }
    }

    for headers in lookup.headings.values() {
        // Duplicate headings if there are multiple with the same id.
        // Normally the Djot parser should manage this, but we may override it to cause collisions.
        if headers.len() > 1 {
            for heading in headers.iter() {
                push_diagnostic(
                    &heading.range,
                    format!("Duplicate heading id: `{}`", heading.id),
                    &mut res,
                );
            }
        }
    }

    Some(res)
}

fn check_url(range: &NeovimRange, url: &str, site: &Site, res: &mut Vec<Diagnostic>) {
    if !url.starts_with('/') {
        return;
    }

    match SiteUrl::parse(url) {
        Ok(site_url) => {
            let path = site_url.output_file(&site.opts.output_dir);
            if !path.exists() {
                push_diagnostic(range, format!("Link to non-existent url: `{}`", url), res);
            }
        }
        Err(err) => {
            push_diagnostic(
                range,
                format!("Unable to parse url `{}`: {}", url, err),
                res,
            );
        }
    }
}

fn push_diagnostic(range: &NeovimRange, message: String, res: &mut Vec<Diagnostic>) {
    // let start = lookup.char_pos_to_row_col(range.start);
    // let end = lookup.char_pos_to_row_col(range.end);
    res.push(Diagnostic {
        lnum: range.start.row,
        col: range.start.col,
        end_lnum: range.end.row,
        end_col: range.end.col,
        message,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    use eyre::Result;

    #[test]
    fn test_diagnostics() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let post_path = test_site.input_path("posts/2022-01-31-test_post.dj");

        let diagnostics = generate_file_diagnostics(&post_path, &test_site.site)
            .expect("Should finD diagnostics for test file");

        let mut messages: Vec<_> = diagnostics.iter().map(|d| d.message.as_str()).collect();
        messages.sort();

        assert_eq!(
            messages,
            [
                "Duplicate heading id: `duplicate-heading`",
                "Duplicate heading id: `duplicate-heading`",
                "Duplicate link definition: `tag1`",
                "Duplicate link definition: `tag1`",
                "Link to non-existent link definition: `broken_tag`",
                "Link to non-existent url: `/blog/xxx`",
                "Link to non-existent url: `/xxx.png`",
                "Link to non-existent url: `/xxx`",
            ]
        );

        Ok(())
    }
}
