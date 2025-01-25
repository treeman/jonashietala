pub mod syntect_highlighter;
pub mod treesitter_highlighter;

use btree_range_map::RangeSet;
use syntect_highlighter::SyntectHighlighter;
use treesitter_highlighter::TreesitterHighlighter;

use eyre::{eyre, Result};
use lazy_static::lazy_static;
use regex::Regex;
use std::borrow::Cow;
use tracing::warn;

pub fn has_highlighter(lang_id: &str) -> bool {
    HighlighterType::find(lang_id).is_some()
}

pub enum Code<'a> {
    Inline {
        code: &'a str,
        lang: Option<&'a str>,
    },
    Block {
        code: &'a str,
        lang: Option<&'a str>,
        path: Option<&'a str>,
        linenum_start: Option<u32>,
        highlight_lines: Option<RangeSet<u32>>,
    },
}

impl Code<'_> {
    pub fn push(&self, s: &mut String) {
        match self {
            Self::Inline { code, lang } => {
                let transformed = TransformedCode::new(*lang, code);
                transformed.push_code_tag_start(s);
                transformed.push_code_raw(s);
                s.push_str("</code>");
            }
            Self::Block {
                code,
                lang,
                path,
                linenum_start,
                highlight_lines,
            } => {
                let transformed = TransformedCode::new(*lang, code);
                transformed.push_wrapper_tag_start(s);
                push_descr(*lang, *path, s);
                s.push_str("<pre>");
                transformed.push_code_tag_start(s);
                transformed.push_code_lines(*linenum_start, highlight_lines.as_ref(), s);
                s.push_str("</code>");
                s.push_str("</pre>");
                s.push_str("</div>");
            }
        }
    }
}

enum TransformedCode<'a> {
    Highlighted {
        code: Cow<'a, str>,
        lang_id: String,
        wide: bool,
    },
    NoHighlight {
        code: Cow<'a, str>,
        wide: bool,
    },
}

impl<'a> TransformedCode<'a> {
    fn new(lang: Option<&str>, code: &'a str) -> TransformedCode<'a> {
        let wide = largest_line_count(code) > 66;

        match lang {
            Some(lang) => match Highlighter::create(lang) {
                Some(highlighter) => match highlighter.highlight(code) {
                    Ok(highlighted) => Self::Highlighted {
                        lang_id: highlighter.lang_id,
                        code: Cow::Owned(highlighted),
                        wide,
                    },
                    Err(err) => {
                        panic!("Syntax highlight error: {}", err);
                    }
                },
                None => {
                    warn!("No highlighter found for: {}", lang);
                    Self::NoHighlight {
                        code: Cow::Borrowed(code),
                        wide,
                    }
                }
            },
            None => Self::NoHighlight {
                code: Cow::Borrowed(code),
                wide,
            },
        }
    }

    fn push_code_tag_start(&self, s: &mut String) {
        match self {
            Self::Highlighted { lang_id, .. } => {
                let html_id = html_escape::encode_safe(lang_id);
                s.push_str(&format!(r#"<code class="highlight {html_id}">"#));
            }
            Self::NoHighlight { .. } => s.push_str("<code>"),
        }
    }

    fn push_wrapper_tag_start(&self, s: &mut String) {
        let mut classes = vec!["code-wrapper"];
        if self.wide() {
            classes.push("wide");
        }
        s.push_str(&format!(r#"<div class="{}">"#, classes.join(" ")));
    }

    fn wide(&self) -> bool {
        match self {
            Self::Highlighted { wide, .. } | Self::NoHighlight { wide, .. } => *wide,
        }
    }

    fn code(&self) -> &str {
        match self {
            Self::Highlighted { code, .. } | Self::NoHighlight { code, .. } => code.as_ref(),
        }
    }

    fn push_code_raw(&self, s: &mut String) {
        let code = self.code();
        s.push_str(code);
    }

    fn push_code_lines(
        &self,
        linenum_start: Option<u32>,
        highlight_lines: Option<&RangeSet<u32>>,
        s: &mut String,
    ) {
        let code = self.code();

        let mut linenum_width = 1;
        if let Some(offset) = linenum_start {
            for (num, _) in code.lines().enumerate() {
                let num = num as u32 + offset;
                let w = num.to_string().len();
                if w > linenum_width {
                    linenum_width = w;
                }
            }
        }

        for (num, line) in code.lines().enumerate() {
            let linenum = (num + 1) as u32;
            let mut classes = vec!["line"];
            if highlight_lines
                .as_ref()
                .map(|hl| hl.contains(linenum))
                .unwrap_or(false)
            {
                classes.push("hl");
            };
            let data_linenum = match linenum_start {
                Some(start) => format!(
                    r#"data-linenum="{:>width$}""#,
                    linenum + start - 1,
                    width = linenum_width
                ),
                None => "".to_string(),
            };
            s.push_str(&format!(
                r#"<div class="{}"{}>"#,
                classes.join(" "),
                data_linenum
            ));
            s.push_str(line);
            s.push('\n');
            s.push_str("</div>");
        }
    }
}

fn push_descr(lang_id: Option<&str>, path: Option<&str>, s: &mut String) {
    let descr = path.or_else(|| lang_id.map(lang_display_name));
    if let Some(descr) = descr {
        s.push_str(&format!(
            r#"<div class="descr" data-descr="{}"></div>"#,
            html_escape::encode_safe(descr)
        ));
    }
}

fn largest_line_count(s: &str) -> usize {
    s.lines().map(|x| x.chars().count()).max().unwrap_or(0)
}

enum HighlighterType<'a> {
    Syntect(SyntectHighlighter<'a>),
    Treesitter(TreesitterHighlighter<'a>),
}

impl HighlighterType<'_> {
    fn find(lang_id: &str) -> Option<Self> {
        if let Some(x) = TreesitterHighlighter::find(lang_id) {
            return Some(Self::Treesitter(x));
        }
        if let Some(x) = SyntectHighlighter::find(lang_id) {
            return Some(Self::Syntect(x));
        }
        None
    }
}

struct Highlighter<'a> {
    lang_id: String,
    highlighter: HighlighterType<'a>,
}

impl Highlighter<'_> {
    fn create(lang_id: &str) -> Option<Self> {
        HighlighterType::find(lang_id).map(|highlighter| Self {
            lang_id: lang_id.to_string(),
            highlighter,
        })
    }

    fn highlight(&self, code: &str) -> Result<String> {
        match &self.highlighter {
            HighlighterType::Syntect(x) => x.highlight(code),
            HighlighterType::Treesitter(x) => x.highlight(code),
        }
    }
}

fn lang_display_name(lang: &str) -> &str {
    match lang {
        "cpp" => "c++",
        "md" => "markdown",
        "vim" => "vimscript",
        "fish-shell" => "fish",
        x => x,
    }
}

lazy_static! {
    pub static ref BLOCK_CODE_SPEC: Regex = Regex::new(r"^[\w_-]+$").unwrap();
    pub static ref INLINE_CODE_SPEC: Regex = Regex::new(r"^([\w_-]+)(.*?)$").unwrap();
    pub static ref RANGE: Regex = Regex::new(r"^(\d+)..(\d+)$").unwrap();
}

pub fn inline_code_spec(s: &str) -> Option<(String, String)> {
    if s.is_empty() {
        return None;
    }
    let captures = INLINE_CODE_SPEC.captures(s)?;

    Some((captures[1].to_string(), captures[2].to_string()))
}

pub fn parse_code_spec(s: &str) -> Option<String> {
    if s.is_empty() {
        return None;
    }
    if BLOCK_CODE_SPEC.is_match(s) {
        Some(s.to_string())
    } else {
        panic!("Could not match code spec: `{}`", s)
    }
}

pub fn parse_line_highlight_spec(spec: &str) -> Result<RangeSet<u32>> {
    let mut res = RangeSet::new();

    for part in spec.split(',') {
        let part = part.trim();
        if let Some(caps) = RANGE.captures(part) {
            let from = caps[1].parse::<u32>()?;
            let to = caps[2].parse::<u32>()?;
            res.insert(from..=to);
        } else {
            let line = part.parse::<u32>()?;
            res.insert(line);
        }
    }

    if res.is_empty() {
        Err(eyre!("Empty highlight spec: `{spec}`"))
    } else {
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    //     #[test]
    //     fn test_extra_code_class() {
    //         assert_eq!(largest_line_count("one\ntwo\nthree"), 5);
    //         assert_eq!(extra_code_class("one\ntwo\nthree"), None);
    //         assert_eq!(
    //             extra_code_class(
    //                 r"
    // One line that is exactly 67... Which line, it's this one...........
    // This is just a small line
    // "
    //             ),
    //             Some("wide")
    //         );
    //     }

    #[test]
    fn test_parse_line_highlight_spec() -> Result<()> {
        let set = parse_line_highlight_spec("0,2,4..5")?;

        assert!(set.contains(0));
        assert!(!set.contains(1));
        assert!(set.contains(2));
        assert!(!set.contains(3));
        assert!(set.contains(4));
        assert!(set.contains(5));
        assert!(!set.contains(6));

        Ok(())
    }
}
