use crate::item::Item;
use crate::markup::markup_lookup::{Element, ImgRef, LinkRef, PosRange, TodoTag};
use crate::paths::AbsPath;
use crate::site_url::SiteUrl;
use crate::Site;
use serde::Serialize;
use serde_repr::*;
use std::collections::{HashMap, HashSet};

#[allow(dead_code)]
#[derive(Debug, Serialize_repr, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum DiagnosticSeverity {
    ERROR = 1,
    WARN = 2,
    INFO = 3,
    HINT = 4,
}

#[derive(Debug, Serialize)]
pub struct Diagnostic {
    pub lnum: usize,
    pub end_lnum: usize,
    pub col: usize,
    pub end_col: usize,
    pub message: String,
    pub severity: DiagnosticSeverity,
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
    let lookup = site.find_lookup_by_path(&path)?;

    let mut res = Vec::new();

    let mut referenced_link_defs = HashSet::new();

    for (_, e) in lookup.char_pos_to_element.iter() {
        match &e.element {
            Element::Link(link) => match &link.link_ref {
                LinkRef::Inline { url } | LinkRef::AutoLink { url } => {
                    check_url(&e.range, url, site, &mut res);
                }
                LinkRef::Reference { label, .. } => {
                    referenced_link_defs.insert(label.as_str());
                }
                LinkRef::Email { .. } => {}
                LinkRef::Unresolved { tag } => {
                    push_diagnostic(
                        &e.range,
                        format!("Link to non-existent link definition: `{}`", tag),
                        DiagnosticSeverity::ERROR,
                        &mut res,
                    );
                }
            },
            Element::Img(img) => match &img.link_ref {
                ImgRef::Inline { url } => {
                    check_url(&e.range, url, site, &mut res);
                }
                ImgRef::Reference { label, .. } => {
                    referenced_link_defs.insert(label.as_str());
                }
                ImgRef::Unresolved { tag } => {
                    push_diagnostic(
                        &e.range,
                        format!("Link to non-existent link definition: `{}`", tag),
                        DiagnosticSeverity::ERROR,
                        &mut res,
                    );
                }
            },
            Element::Todo(TodoTag::Todo) => {
                push_diagnostic(
                    &e.range,
                    format!("TODO"),
                    DiagnosticSeverity::WARN,
                    &mut res,
                );
            }
            Element::Todo(TodoTag::Note) => {
                push_diagnostic(
                    &e.range,
                    format!("NOTE"),
                    DiagnosticSeverity::INFO,
                    &mut res,
                );
            }
            Element::Todo(TodoTag::Fixme) => {
                push_diagnostic(
                    &e.range,
                    format!("FIXME"),
                    DiagnosticSeverity::WARN,
                    &mut res,
                );
            }
            _ => {}
        }
    }

    for (label, defs) in lookup.link_defs.iter() {
        // Duplicate link definitions if there are multiple with the same label.
        let duplicate = defs.len() > 1;

        for def in defs.iter() {
            check_url(&def.range, &def.link_def.url, site, &mut res);

            if duplicate {
                push_diagnostic(
                    &def.range,
                    format!("Duplicate link definition: `{}`", def.link_def.label),
                    DiagnosticSeverity::WARN,
                    &mut res,
                );
            }

            if !referenced_link_defs.contains(label.as_str()) {
                push_diagnostic(
                    &def.range,
                    format!("Unused link definition: `{}`", def.link_def.label),
                    DiagnosticSeverity::INFO,
                    &mut res,
                );
            }
        }
    }

    for headers in lookup.headings.values() {
        // Duplicate headings if there are multiple with the same id.
        // Normally the Djot parser should manage this, but we may override it to cause collisions.
        if headers.len() > 1 {
            for h in headers.iter() {
                push_diagnostic(
                    &h.range,
                    format!("Duplicate heading id: `{}`", h.heading.id),
                    DiagnosticSeverity::WARN,
                    &mut res,
                );
            }
        }
    }

    Some(res)
}

fn check_url(range: &PosRange, url: &str, site: &Site, res: &mut Vec<Diagnostic>) {
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
                    DiagnosticSeverity::ERROR,
                    res,
                );
            }
        }
        Err(err) => {
            push_diagnostic(
                range,
                format!("Unable to parse url `{}`: {}", url, err),
                DiagnosticSeverity::ERROR,
                res,
            );
        }
    }
}

fn push_diagnostic(
    range: &PosRange,
    message: String,
    severity: DiagnosticSeverity,
    res: &mut Vec<Diagnostic>,
) {
    // let start = lookup.char_pos_to_row_col(range.start);
    // let end = lookup.char_pos_to_row_col(range.end);
    res.push(Diagnostic {
        lnum: range.start.row,
        col: range.start.col,
        end_lnum: range.end.row,
        end_col: range.end.col,
        message,
        severity,
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
            .expect("Should find diagnostics for test file");

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
                "Unused link definition: `bad`",
            ]
        );

        Ok(())
    }
}
