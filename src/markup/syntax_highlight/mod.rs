pub mod syntect_highlighter;
pub mod treesitter_highlighter;

use syntect_highlighter::SyntectHighlighter;
use treesitter_highlighter::TreesitterHighlighter;

use eyre::Result;
use lazy_static::lazy_static;
use regex::Regex;
use tracing::warn;

pub fn push_code_block<S: AsRef<str>>(s: &mut String, lang: Option<S>, code: &str) {
    match lang {
        Some(lang) => match Highlighter::create(lang.as_ref()) {
            Some(highlighter) => match highlighter.highlight(code) {
                Ok(highlighted) => {
                    push_code_block_highlight(s, &highlighter.lang_id, code, &highlighted);
                }
                Err(err) => {
                    panic!("Syntax highlight error: {}", err);
                }
            },
            None => {
                warn!("No highlighter found for: {}", lang.as_ref());
                push_code_block_no_highlight(s, code);
            }
        },
        None => {
            push_code_block_no_highlight(s, code);
        }
    }
}

pub fn push_code_inline(s: &mut String, lang: &str, code: &str) {
    match Highlighter::create(lang) {
        Some(highlighter) => match highlighter.highlight(code) {
            Ok(highlighted) => {
                push_code_highlight(s, &highlighter.lang_id, &highlighted);
            }
            Err(err) => {
                panic!("Syntax highlight error: {}", err);
            }
        },
        None => {
            warn!("No highlighter found for: {}", lang);
            push_code_no_highlight(s, code);
        }
    }
}

fn push_code_block_highlight(
    s: &mut String,
    lang_id: &str,
    original_code: &str,
    highlighted_code: &str,
) {
    let html_id = lang_display_name(lang_id);

    // Wrap things in an extra div to allow the language display div to
    // be visible outside the <pre> tag.
    // Set the language as a class and insert text using ::before to
    // not interfere with readers of different types.
    push_code_wrapper_start(s, original_code);
    s.push_str(&format!(
        r#"<div class="lang {}" data-lang="{}"></div>"#,
        html_id,
        html_escape::encode_safe(lang_id)
    ));
    s.push_str("<pre>");
    push_code_highlight(s, html_id, highlighted_code);
    s.push_str(r#"</pre>"#);
    s.push_str(r#"</div>"#);
}

fn push_code_block_no_highlight(s: &mut String, code: &str) {
    push_code_wrapper_start(s, code);
    s.push_str("<pre>");
    push_code_no_highlight(s, code);
    s.push_str(r#"</pre>"#);
    s.push_str(r#"</div>"#);
}

fn push_code_highlight(s: &mut String, html_id: &str, highlighted_code: &str) {
    s.push_str(r#"<code class="highlight "#);
    s.push_str(html_id);
    s.push_str(r#"">"#);
    s.push_str(highlighted_code);
    s.push_str("</code>");
}

fn push_code_no_highlight(s: &mut String, code: &str) {
    s.push_str("<code>");
    s.push_str(&html_escape::encode_safe(&code));
    s.push_str("</code>");
}

fn push_code_wrapper_start(s: &mut String, raw_code: &str) {
    let mut classes = vec!["code-wrapper"];
    if let Some(extra) = extra_code_class(raw_code) {
        classes.push(extra);
    }
    s.push_str(&format!(r#"<div class="{}">"#, classes.join(" ")));
}

fn extra_code_class(raw_code: &str) -> Option<&'static str> {
    let count = largest_line_count(raw_code);

    if count > 66 {
        Some("wide")
    } else {
        None
    }
}

fn largest_line_count(s: &str) -> usize {
    s.lines().map(|x| x.chars().count()).max().unwrap_or(0)
}

enum HighlighterType<'a> {
    Syntect(SyntectHighlighter<'a>),
    Treesitter(TreesitterHighlighter<'a>),
}

impl<'a> HighlighterType<'a> {
    fn find(lang_id: &str) -> Option<Self> {
        if let Some(x) = TreesitterHighlighter::find(lang_id) {
            return Some(Self::Treesitter(x));
        }
        if let Some(x) = SyntectHighlighter::find(lang_id) {
            return Some(Self::Syntect(x));
        }
        return None;
    }
}

struct Highlighter<'a> {
    lang_id: String,
    highlighter: HighlighterType<'a>,
}

impl<'a> Highlighter<'a> {
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
        x => x,
    }
}

lazy_static! {
    pub static ref BLOCK_CODE_SPEC: Regex = Regex::new(r"^\w+$").unwrap();
}
lazy_static! {
    pub static ref INLINE_CODE_SPEC: Regex = Regex::new(r"^(\w+)(.*?)$").unwrap();
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_extra_code_class() {
        assert_eq!(largest_line_count("one\ntwo\nthree"), 5);
        assert_eq!(extra_code_class("one\ntwo\nthree"), None);
        assert_eq!(
            extra_code_class(
                r"
One line that is exactly 67... Which line, it's this one...........
This is just a small line
"
            ),
            Some("wide")
        );
    }
}
