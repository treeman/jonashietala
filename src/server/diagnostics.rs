use crate::item::Item;
use crate::markup::markup_lookup::{LinkRef, MarkupLookup};
use crate::paths::AbsPath;
use crate::site_url::SiteUrl;
use crate::Site;
use serde::Serialize;
use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug, Serialize)]
pub struct Diagnostic {
    pub linenum: usize,
    pub end_linenum: usize,
    pub column: usize,
    pub end_column: usize,
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

    for link in lookup.links.iter() {
        match &link.link_ref {
            LinkRef::Inline(url) | LinkRef::AutoLink(url) => {
                check_url(&link.range, url, lookup, site, &mut res);
            }
            LinkRef::Reference { .. } => {}
            LinkRef::Email(_) => {}
            LinkRef::Unresolved(tag) => {
                push_diagnostic(
                    &link.range,
                    format!("Link to non-existent link definition: `{}`", tag),
                    lookup,
                    &mut res,
                );
            }
        }
    }

    for def in lookup.link_defs.values() {
        check_url(&def.range, &def.url, lookup, site, &mut res);
    }

    // Duplicate link definitions.

    Some(res)
}

fn check_url(
    range: &Range<usize>,
    url: &str,
    lookup: &MarkupLookup,
    site: &Site,
    res: &mut Vec<Diagnostic>,
) {
    if !url.starts_with('/') {
        return;
    }

    match SiteUrl::parse(url) {
        Ok(site_url) => {
            let path = site_url.output_file(&site.opts.output_dir);
            if !path.exists() {
                push_diagnostic(
                    range,
                    format!("Link to non-existent url: `{}`", url),
                    lookup,
                    res,
                );
            }
        }
        Err(err) => {
            push_diagnostic(
                range,
                format!("Unable to parse url `{}`: {}", url, err),
                lookup,
                res,
            );
        }
    }
}

fn push_diagnostic(
    range: &Range<usize>,
    message: String,
    lookup: &MarkupLookup,
    res: &mut Vec<Diagnostic>,
) {
    let start = lookup.char_pos_to_row_col(range.start);
    let end = lookup.char_pos_to_row_col(range.end);
    res.push(Diagnostic {
        linenum: start.0,
        column: start.1,
        end_linenum: end.0,
        end_column: end.1,
        message,
    });
}
