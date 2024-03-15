use crate::util;
use camino::{Utf8Path, Utf8PathBuf};
use colored::Colorize;
use eyre::eyre;
use eyre::Result;
use lazy_static::lazy_static;
use regex::Regex;
use syntect::parsing::SyntaxReference;
use syntect::{
    dumps, highlighting::ThemeSet, html, html::ClassStyle, html::ClassedHTMLGenerator,
    parsing::SyntaxSet, util::LinesWithEndings,
};
use tracing::{info, warn};

lazy_static! {
    static ref SS: SyntaxSet = syntax_set();
}

pub fn dump_syntax_binary() -> Result<()> {
    let file = "syntaxes/syntax_set.packdump";
    info!("Dumping syntax binary to: {file}");
    let mut builder = SyntaxSet::load_defaults_newlines().into_builder();
    builder.add_from_folder("syntaxes", true).unwrap();
    let ss = builder.build();
    dumps::dump_to_uncompressed_file(&ss, file)?;
    Ok(())
}

pub fn dump_theme(file: &Utf8Path) -> Result<()> {
    let theme = ThemeSet::get_theme(file)?;
    let output_file =
        Utf8PathBuf::from_path_buf(format!("css/themes/{}.scss", file.file_stem().unwrap()).into())
            .unwrap();
    let css = html::css_for_theme_with_class_style(&theme, ClassStyle::Spaced)?;
    util::write_to_file(&output_file, &css)?;
    println!("{} {}", "[Created]".green(), output_file);
    Ok(())
}

struct HighlightSpec<'a> {
    pub html_id: String,
    pub display_name: String,
    pub syntax: &'a SyntaxReference,
}

impl<'a> HighlightSpec<'a> {
    fn find(original: &str) -> Option<Self> {
        if original.is_empty() {
            return None;
        }
        let id = to_language_id(original);

        let syntect_name = syntect_lang_name(&id);

        let syntax = SS
            .find_syntax_by_name(syntect_name)
            .or_else(|| SS.find_syntax_by_extension(syntect_name))?;

        let display_name = lang_display_name(&id).to_owned();

        Some(Self {
            html_id: id,
            display_name,
            syntax,
        })
    }
}

pub fn push_code_block<S: AsRef<str>>(s: &mut String, lang: Option<S>, code: &str) {
    match lang {
        Some(lang) => match HighlightSpec::find(lang.as_ref()) {
            Some(spec) => match highlight(&spec, code) {
                Ok(highlight) => {
                    push_code_block_highlight(
                        s,
                        &spec.html_id,
                        &spec.display_name,
                        code,
                        &highlight,
                    );
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
    match HighlightSpec::find(lang) {
        Some(spec) => match highlight(&spec, code) {
            Ok(highlight) => {
                push_code_highlight(s, &spec.html_id, &highlight);
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
    html_id: &str,
    display_name: &str,
    original_code: &str,
    highlighted_code: &str,
) {
    // Wrap things in an extra div to allow the language display div to
    // be visible outside the <pre> tag.
    // Set the language as a class and insert text using ::before to
    // not interfere with readers of different types.
    push_code_wrapper_start(s, original_code);
    s.push_str(&format!(
        r#"<div class="lang {}" data-lang="{}"></div>"#,
        html_id,
        html_escape::encode_safe(display_name)
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

fn highlight(spec: &HighlightSpec, code: &str) -> Result<String> {
    lazy_static! {
        static ref SPAN_WRAPPER: Regex = Regex::new(r"^<span [^>]+>(?s)(.+)</span>$").unwrap();
    }

    let mut html_generator =
        ClassedHTMLGenerator::new_with_class_style(spec.syntax, &SS, ClassStyle::Spaced);
    for line in LinesWithEndings::from(code) {
        html_generator.parse_html_for_line_which_includes_newline(line)?;
    }
    let generated = html_generator.finalize();

    // Empty syntax blocks will panic when we try to replace the generated <span>
    if generated.is_empty() {
        return Ok("".into());
    }

    // Strip wrapping <span>, later to be replaced with a <code> tag.
    let cap = SPAN_WRAPPER
        .captures(&generated)
        .ok_or_else(|| eyre!("Failed to match syntax span"))?;
    Ok(cap[1].trim().to_string())
}

// Use a binary dump to radically speedup SyntaxSet creation.
// If a syntax file is changed the below dump function updates it.
fn syntax_set() -> SyntaxSet {
    dumps::from_uncompressed_data(include_bytes!("../../syntaxes/syntax_set.packdump")).unwrap()
}

// Elixir uses new unsupported features:
// https://github.com/trishume/syntect/issues/323
// Should update the submodule when it's fixed to get heex support.
//
// See Zola for more language refs:
// https://github.com/getzola/zola/tree/master/sublime/syntaxes
fn syntect_lang_name(lang: &str) -> &str {
    match lang {
        // Some language specs used that don't fit syntect's file extension or name.
        "elixir" => "Elixir",
        "haskell" => "Haskell",
        "perl" => "Perl",
        "python" => "Python",
        "python3" => "Python",
        "racket" => "Racket",
        "ruby" => "Ruby",
        "rust" => "Rust",
        "shell" => "Shell-Unix-Generic",
        "erlang" => "Erlang",
        "js" => "JavaScript",
        "djot" => "Djot",

        // "pollen" => "",
        x => x,
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

fn to_language_id(s: &str) -> String {
    let s = s.trim().to_lowercase();

    match s.as_str() {
        "c++" => "cpp".to_string(),
        "javascript" => "js".to_string(),
        _ => s,
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
