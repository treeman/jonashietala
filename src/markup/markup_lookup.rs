use btree_range_map::RangeMap;
use lazy_static::lazy_static;
use regex::Regex;
use serde::Serialize;
use std::{collections::HashMap, ops::Range};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Pos {
    pub row: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(row: usize, col: usize) -> Pos {
        Self { row, col }
    }
}

impl PartialEq<(usize, usize)> for Pos {
    fn eq(&self, other: &(usize, usize)) -> bool {
        self.row == other.0 && self.col == other.1
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PosRange {
    pub start: Pos,
    pub end: Pos,
}

impl PosRange {
    pub fn new(start: (usize, usize), end: (usize, usize)) -> Self {
        Self {
            start: Pos::new(start.0, start.1),
            end: Pos::new(end.0, end.1),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Heading {
    pub id: String,
    pub level: u16,
    pub content: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct LinkDef {
    pub label: String,
    pub url: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(tag = "type")]
pub enum LinkRef {
    Inline { url: String },
    Reference { label: String, url: String },
    Email { url: String },
    AutoLink { url: String },
    Unresolved { tag: String },
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Link {
    pub link_ref: LinkRef,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(tag = "type")]
pub enum ImgRef {
    Inline { url: String },
    Reference { label: String, url: String },
    Unresolved { tag: String },
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Img {
    pub link_ref: ImgRef,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(tag = "type")]
pub enum TodoTag {
    Todo,
    Fixme,
    Note,
}

lazy_static! {
    static ref SPLIT: Regex = Regex::new(r"^(\w+)\s+").unwrap();
}

impl TodoTag {
    pub fn from_beginning_of_line(s: &str) -> Option<(Self, usize)> {
        if let Some(caps) = SPLIT.captures(s) {
            match &caps[1] {
                "TODO" | "WIP" => Some((Self::Todo, (caps[1]).len())),
                "NOTE" | "INFO" | "XXX" => Some((Self::Note, (caps[1]).len())),
                "FIXME" => Some((Self::Fixme, (caps[1]).len())),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn class(&self) -> &'static str {
        match self {
            Self::Todo => "todo",
            Self::Note => "note",
            Self::Fixme => "fixme",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(tag = "type")]
pub enum Element {
    Link(Link),
    LinkDef(LinkDef),
    Heading(Heading),
    Img(Img),
    Todo(TodoTag),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RawElementLookup {
    pub element: Element,
    pub char_range: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LinkDefLookup {
    pub link_def: LinkDef,
    pub range: PosRange,
    pub char_range: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HeadingLookup {
    pub heading: Heading,
    pub range: PosRange,
    pub char_range: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ElementLookup {
    pub element: Element,
    pub range: PosRange,
    pub char_range: Range<usize>,
}

type LinkLabel = String;
type HeadingId = String;

#[derive(Debug)]
pub struct MarkupLookup {
    // Element lookup by character position.
    pub char_pos_to_element: RangeMap<usize, ElementLookup>,

    // Element lookup by id or type.
    pub link_defs: HashMap<LinkLabel, Vec<LinkDefLookup>>,
    pub headings: HashMap<HeadingId, Vec<HeadingLookup>>,

    // Position translations.
    prev_line_size_sum: Vec<usize>,
    // Line calculation offset, to handle frontmatter that isn't
    // included in markup registration.
    line_calc_offset: usize,
}

impl MarkupLookup {
    pub fn new(source: &str, line_calc_offset: usize) -> Self {
        let mut line_size_sum = Vec::new();

        let mut sum = 0;
        for line in source.lines() {
            // Include the newline.
            // I don't know how to handle carriage returns nor do I care.
            // Neovim counts columns using bytes, not character!
            let count = line.len() + 1;
            line_size_sum.push(sum);
            sum += count;
        }
        line_size_sum.push(sum);

        Self {
            char_pos_to_element: RangeMap::new(),
            link_defs: HashMap::new(),
            headings: HashMap::new(),
            prev_line_size_sum: line_size_sum,
            line_calc_offset,
        }
    }

    pub fn in_frontmatter(&self, row: usize) -> bool {
        row < self.line_calc_offset
    }

    pub fn element_at(&self, row: usize, col: usize) -> Option<&ElementLookup> {
        self.char_pos_to_element
            .get(self.row_col_to_char_pos(row, col)?)
    }

    pub fn pos_range(&self, range: &Range<usize>) -> PosRange {
        PosRange {
            start: self.char_pos_to_row_col(range.start),
            end: self.char_pos_to_row_col(range.end),
        }
    }

    fn char_pos_to_row_col(&self, pos: usize) -> Pos {
        match self.prev_line_size_sum.binary_search(&pos) {
            // Found an exact match
            Ok(row) => Pos::new(row + self.line_calc_offset, 0),
            // An error means we could insert it here sorted,
            // but we'll use it to calculate the remaining chars.
            Err(row) => Pos::new(
                row + self.line_calc_offset - 1,
                pos - self.prev_line_size_sum[row - 1],
            ),
        }
    }

    fn row_col_to_char_pos(&self, row: usize, col: usize) -> Option<usize> {
        if self.line_calc_offset > row {
            return None;
        }
        let row_check = row - self.line_calc_offset;
        if row_check + 1 >= self.prev_line_size_sum.len() {
            return None;
        }

        Some(self.prev_line_size_sum[row_check] + col)
    }

    pub fn at_pos(&self, pos: usize) -> Option<&ElementLookup> {
        self.char_pos_to_element.get(pos)
    }

    pub fn insert_element(&mut self, element: Element, char_range: Range<usize>) {
        let range = self.pos_range(&char_range);
        match element {
            Element::Heading(ref heading) => {
                let heading_lookup = HeadingLookup {
                    heading: heading.clone(),
                    range,
                    char_range: char_range.clone(),
                };
                self.headings
                    .entry(heading.id.clone())
                    .and_modify(|hs| hs.push(heading_lookup.clone()))
                    .or_insert_with(|| vec![heading_lookup]);
            }
            Element::LinkDef(ref link_def) => {
                let link_def_lookup = LinkDefLookup {
                    link_def: link_def.clone(),
                    range,
                    char_range: char_range.clone(),
                };
                self.link_defs
                    .entry(link_def.label.clone())
                    .and_modify(|defs| defs.push(link_def_lookup.clone()))
                    .or_insert_with(|| vec![link_def_lookup]);
            }
            _ => {}
        }

        self.char_pos_to_element.insert(
            char_range.clone(),
            ElementLookup {
                element,
                range,
                char_range,
            },
        );
    }

    // FIXME remove all below?
    pub fn insert_heading(&mut self, heading: Heading, char_range: Range<usize>) {
        self.insert_element(Element::Heading(heading), char_range);
    }

    pub fn insert_img(&mut self, img: Img, char_range: Range<usize>) {
        self.insert_element(Element::Img(img), char_range);
    }

    pub fn insert_link(&mut self, link: Link, char_range: Range<usize>) {
        self.insert_element(Element::Link(link), char_range);
    }

    pub fn insert_link_def(&mut self, link_def: LinkDef, char_range: Range<usize>) {
        self.insert_element(Element::LinkDef(link_def), char_range);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_position_transforms() {
        let lookup = MarkupLookup::new(
            "012345

01234
0",
            0,
        );

        assert_eq!(lookup.char_pos_to_row_col(0), (0, 0));
        assert_eq!(lookup.char_pos_to_row_col(1), (0, 1));
        assert_eq!(lookup.char_pos_to_row_col(2), (0, 2));
        assert_eq!(lookup.char_pos_to_row_col(3), (0, 3));
        assert_eq!(lookup.char_pos_to_row_col(4), (0, 4));
        assert_eq!(lookup.char_pos_to_row_col(5), (0, 5));
        assert_eq!(lookup.char_pos_to_row_col(6), (0, 6));

        assert_eq!(lookup.char_pos_to_row_col(7), (1, 0));

        assert_eq!(lookup.char_pos_to_row_col(8), (2, 0));
        assert_eq!(lookup.char_pos_to_row_col(9), (2, 1));
        assert_eq!(lookup.char_pos_to_row_col(10), (2, 2));
        assert_eq!(lookup.char_pos_to_row_col(11), (2, 3));
        assert_eq!(lookup.char_pos_to_row_col(12), (2, 4));
        assert_eq!(lookup.char_pos_to_row_col(13), (2, 5));

        assert_eq!(lookup.char_pos_to_row_col(14), (3, 0));
        assert_eq!(lookup.char_pos_to_row_col(15), (3, 1));

        assert_eq!(lookup.row_col_to_char_pos(0, 0), Some(0));
        assert_eq!(lookup.row_col_to_char_pos(0, 1), Some(1));
        assert_eq!(lookup.row_col_to_char_pos(0, 2), Some(2));
        assert_eq!(lookup.row_col_to_char_pos(0, 3), Some(3));
        assert_eq!(lookup.row_col_to_char_pos(0, 4), Some(4));
        assert_eq!(lookup.row_col_to_char_pos(0, 5), Some(5));
        assert_eq!(lookup.row_col_to_char_pos(0, 6), Some(6));

        assert_eq!(lookup.row_col_to_char_pos(1, 0), Some(7));

        assert_eq!(lookup.row_col_to_char_pos(2, 0), Some(8));
        assert_eq!(lookup.row_col_to_char_pos(2, 1), Some(9));
        assert_eq!(lookup.row_col_to_char_pos(2, 2), Some(10));
        assert_eq!(lookup.row_col_to_char_pos(2, 3), Some(11));
        assert_eq!(lookup.row_col_to_char_pos(2, 4), Some(12));
        assert_eq!(lookup.row_col_to_char_pos(2, 5), Some(13));

        assert_eq!(lookup.row_col_to_char_pos(3, 0), Some(14));
        assert_eq!(lookup.row_col_to_char_pos(3, 1), Some(15));

        assert_eq!(lookup.row_col_to_char_pos(4, 0), None);
    }

    #[test]
    fn test_lookup_position_offset() {
        let lookup = MarkupLookup::new("012345", 4);

        assert_eq!(lookup.char_pos_to_row_col(0), (4, 0));
        assert_eq!(lookup.char_pos_to_row_col(1), (4, 1));
        assert_eq!(lookup.char_pos_to_row_col(2), (4, 2));
        assert_eq!(lookup.char_pos_to_row_col(3), (4, 3));
        assert_eq!(lookup.char_pos_to_row_col(4), (4, 4));
        assert_eq!(lookup.char_pos_to_row_col(5), (4, 5));
        assert_eq!(lookup.char_pos_to_row_col(6), (4, 6));

        assert_eq!(lookup.row_col_to_char_pos(0, 0), None);
        assert_eq!(lookup.row_col_to_char_pos(1, 0), None);
        assert_eq!(lookup.row_col_to_char_pos(2, 0), None);
        assert_eq!(lookup.row_col_to_char_pos(3, 0), None);

        assert_eq!(lookup.row_col_to_char_pos(4, 0), Some(0));
        assert_eq!(lookup.row_col_to_char_pos(4, 1), Some(1));
        assert_eq!(lookup.row_col_to_char_pos(4, 2), Some(2));
        assert_eq!(lookup.row_col_to_char_pos(4, 3), Some(3));
        assert_eq!(lookup.row_col_to_char_pos(4, 4), Some(4));
        assert_eq!(lookup.row_col_to_char_pos(4, 5), Some(5));
        assert_eq!(lookup.row_col_to_char_pos(4, 6), Some(6));
    }

    #[test]
    fn test_lookup_lines_counts_bytes() {
        let lookup = MarkupLookup::new("’", 5);
        assert_eq!(lookup.prev_line_size_sum, vec![0, 4]); // 3 for ’ + newline
    }

    #[test]
    fn test_neovim_range() {
        let lookup = MarkupLookup::new(
            "first line
second line
third line",
            0,
        );

        assert_eq!(lookup.pos_range(&(0..1)), PosRange::new((0, 0), (0, 1)));
        assert_eq!(lookup.pos_range(&(0..5)), PosRange::new((0, 0), (0, 5)));
        assert_eq!(lookup.pos_range(&(7..25)), PosRange::new((0, 7), (2, 2)));
    }
}
