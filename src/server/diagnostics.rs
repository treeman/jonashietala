use crate::item::Item;
use crate::paths::{self, AbsPath};
use crate::Site;
use serde::Serialize;
use std::collections::HashMap;

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

    // Broken links.
    for link in lookup.broken_links.iter() {
        let start = lookup.char_pos_to_row_col(link.range.start);
        let end = lookup.char_pos_to_row_col(link.range.end);
        res.push(Diagnostic {
            linenum: start.0,
            column: start.1,
            end_linenum: end.0,
            end_column: end.1,
            message: format!("Link to non-existent link definition `{}`", link.tag),
        })
    }

    // Links to non-existent images.

    // Links to non-existent urls.

    // Duplicate link definitions.

    Some(res)
}
