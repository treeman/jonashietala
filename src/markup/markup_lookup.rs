use btree_range_map::RangeMap;
use std::{collections::HashMap, ops::Range};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Heading {
    pub id: String,
    pub content: String,
    pub range: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LinkDef {
    pub label: String,
    pub url: String,
    pub range: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LinkRef {
    Inline(String),
    Reference { label: String, url: String },
    Email(String),
    AutoLink(String),
    Unresolved(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Link {
    pub link_ref: LinkRef,
    pub range: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ElementInfo {
    Link(Link),
    LinkDef(LinkDef),
    Heading(Heading),
}

type LinkLabel = String;
type HeadingId = String;

#[derive(Debug)]
pub struct MarkupLookup {
    // Element lookup by character position.
    pub pos_to_element: RangeMap<usize, ElementInfo>,

    // Element lookup by id or type.
    pub broken_links: Vec<Link>,
    pub link_defs: HashMap<LinkLabel, LinkDef>,
    pub headings: HashMap<HeadingId, Heading>,

    // Position translations.
    line_size: Vec<usize>,
    prev_line_size_sum: Vec<usize>,
    // Line calculation offset, to handle frontmatter that isn't
    // included in markup registration.
    line_calc_offset: usize,
}

impl MarkupLookup {
    pub fn new(source: &str, line_calc_offset: usize) -> Self {
        let mut line_size = Vec::new();
        let mut line_size_sum = Vec::new();

        let mut sum = 0;
        for line in source.lines() {
            // Include the newline.
            // I don't know how to handle carriage returns nor do I care.
            // Neovim counts columns using bytes, not character!
            let count = line.bytes().count() + 1;
            line_size.push(count);
            line_size_sum.push(sum);
            sum += count;
        }

        Self {
            pos_to_element: RangeMap::new(),
            broken_links: Vec::new(),
            link_defs: HashMap::new(),
            headings: HashMap::new(),
            line_size,
            prev_line_size_sum: line_size_sum,
            line_calc_offset,
        }
    }

    pub fn element_at(&self, row: usize, col: usize) -> Option<&ElementInfo> {
        self.pos_to_element.get(self.row_col_to_char_pos(row, col)?)
    }

    pub fn char_pos_to_row_col(&self, pos: usize) -> (usize, usize) {
        match self.prev_line_size_sum.binary_search(&pos) {
            // Found an exact match
            Ok(row) => {
                if self.line_size[row] == 1 {
                    // Special case for lines with only a newline,
                    // Neovim treats those as column 0 when in normal mode.
                    (row + 1 + self.line_calc_offset, 0)
                } else {
                    (row + 1 + self.line_calc_offset, 1)
                }
            }
            // An error means we could insert it here sorted,
            // but we'll use it to calculate the remaining chars.
            Err(row) => (
                row + self.line_calc_offset,
                pos - self.prev_line_size_sum[row - 1] + 1,
            ),
        }
    }

    pub fn row_col_to_char_pos(&self, row: usize, col: usize) -> Option<usize> {
        if row == 0 || self.line_calc_offset > row - 1 {
            return None;
        }
        let row_check = row - 1 - self.line_calc_offset;
        if row_check >= self.line_size.len() {
            return None;
        }

        if self.line_size[row_check] == 1 {
            // Special case for blankline.
            Some(self.prev_line_size_sum[row_check] + col)
        } else {
            Some(self.prev_line_size_sum[row_check] + col - 1)
        }
    }

    pub fn at_pos(&self, pos: usize) -> Option<&ElementInfo> {
        self.pos_to_element.get(pos)
    }

    pub fn insert_heading(&mut self, heading: Heading) {
        self.headings.insert(heading.id.clone(), heading.clone());
        self.pos_to_element
            .insert(heading.range.clone(), ElementInfo::Heading(heading));
    }

    pub fn insert_link(&mut self, link: Link) {
        if let LinkRef::Unresolved(_) = link.link_ref {
            self.broken_links.push(link.clone());
        }

        self.pos_to_element
            .insert(link.range.clone(), ElementInfo::Link(link));
    }

    pub fn insert_link_def(&mut self, link_def: LinkDef) {
        self.link_defs
            .insert(link_def.label.clone(), link_def.clone());
        self.pos_to_element
            .insert(link_def.range.clone(), ElementInfo::LinkDef(link_def));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_position_transforms() {
        // 1. 6 chars, 7th newline
        // 2. 7..8 blankline
        // 3. 5 chars, 13..14 softbreak
        // 4. 14..15 "0", 16th end paragraph
        //
        let lookup = MarkupLookup::new(
            "012345

01234
0
",
            0,
        );

        // vim.api.nvim_win_get_cursor(0)

        // In Neovim, a character starts at pos 1 on a column.
        // A newline on a blankline gets pos 0.
        // Inserting after a line gets the newline position too.
        // Rows start at 1 as well.
        assert_eq!(lookup.char_pos_to_row_col(0), (1, 1));
        assert_eq!(lookup.char_pos_to_row_col(1), (1, 2));
        assert_eq!(lookup.char_pos_to_row_col(2), (1, 3));
        assert_eq!(lookup.char_pos_to_row_col(3), (1, 4));
        assert_eq!(lookup.char_pos_to_row_col(4), (1, 5));
        assert_eq!(lookup.char_pos_to_row_col(5), (1, 6));
        assert_eq!(lookup.char_pos_to_row_col(6), (1, 7));

        assert_eq!(lookup.char_pos_to_row_col(7), (2, 0));

        assert_eq!(lookup.char_pos_to_row_col(8), (3, 1));
        assert_eq!(lookup.char_pos_to_row_col(9), (3, 2));
        assert_eq!(lookup.char_pos_to_row_col(10), (3, 3));
        assert_eq!(lookup.char_pos_to_row_col(11), (3, 4));
        assert_eq!(lookup.char_pos_to_row_col(12), (3, 5));
        assert_eq!(lookup.char_pos_to_row_col(13), (3, 6));

        assert_eq!(lookup.char_pos_to_row_col(14), (4, 1));
        assert_eq!(lookup.char_pos_to_row_col(15), (4, 2));

        assert_eq!(lookup.row_col_to_char_pos(1, 1), Some(0));
        assert_eq!(lookup.row_col_to_char_pos(1, 2), Some(1));
        assert_eq!(lookup.row_col_to_char_pos(1, 3), Some(2));
        assert_eq!(lookup.row_col_to_char_pos(1, 4), Some(3));
        assert_eq!(lookup.row_col_to_char_pos(1, 5), Some(4));
        assert_eq!(lookup.row_col_to_char_pos(1, 6), Some(5));
        assert_eq!(lookup.row_col_to_char_pos(1, 7), Some(6));

        assert_eq!(lookup.row_col_to_char_pos(2, 0), Some(7));

        assert_eq!(lookup.row_col_to_char_pos(3, 1), Some(8));
        assert_eq!(lookup.row_col_to_char_pos(3, 2), Some(9));
        assert_eq!(lookup.row_col_to_char_pos(3, 3), Some(10));
        assert_eq!(lookup.row_col_to_char_pos(3, 4), Some(11));
        assert_eq!(lookup.row_col_to_char_pos(3, 5), Some(12));
        assert_eq!(lookup.row_col_to_char_pos(3, 6), Some(13));

        assert_eq!(lookup.row_col_to_char_pos(4, 1), Some(14));
        assert_eq!(lookup.row_col_to_char_pos(4, 2), Some(15));
    }

    #[test]
    fn test_lookup_position_offset() {
        let lookup = MarkupLookup::new("012345", 4);

        assert_eq!(lookup.char_pos_to_row_col(0), (5, 1));
        assert_eq!(lookup.char_pos_to_row_col(1), (5, 2));
        assert_eq!(lookup.char_pos_to_row_col(2), (5, 3));
        assert_eq!(lookup.char_pos_to_row_col(3), (5, 4));
        assert_eq!(lookup.char_pos_to_row_col(4), (5, 5));
        assert_eq!(lookup.char_pos_to_row_col(5), (5, 6));
        assert_eq!(lookup.char_pos_to_row_col(6), (5, 7));

        assert_eq!(lookup.row_col_to_char_pos(0, 1), None);
        assert_eq!(lookup.row_col_to_char_pos(1, 1), None);
        assert_eq!(lookup.row_col_to_char_pos(2, 1), None);
        assert_eq!(lookup.row_col_to_char_pos(3, 1), None);
        assert_eq!(lookup.row_col_to_char_pos(4, 1), None);

        assert_eq!(lookup.row_col_to_char_pos(5, 1), Some(0));
        assert_eq!(lookup.row_col_to_char_pos(5, 2), Some(1));
        assert_eq!(lookup.row_col_to_char_pos(5, 3), Some(2));
        assert_eq!(lookup.row_col_to_char_pos(5, 4), Some(3));
        assert_eq!(lookup.row_col_to_char_pos(5, 5), Some(4));
        assert_eq!(lookup.row_col_to_char_pos(5, 6), Some(5));
        assert_eq!(lookup.row_col_to_char_pos(5, 7), Some(6));
    }

    #[test]
    fn test_lookup_lines_counts_bytes() {
        let lookup = MarkupLookup::new("’", 5);
        assert_eq!(lookup.line_size, vec![4]); // 3 for ’ + newline
    }
}
