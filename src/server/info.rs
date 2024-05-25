use crate::{markup::markup_lookup::Element, site::Site};

pub fn item_info(linenum: usize, column: usize, path: &str, site: &Site) -> Option<Element> {
    let lookup = site.find_lookup_by_path(&path.into())?;
    Some(lookup.element_at(linenum, column)?.element.clone())
}
