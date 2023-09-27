use crate::util;
use camino::{Utf8Path, Utf8PathBuf};
use colored::Colorize;
use eyre::Result;
use lazy_static::lazy_static;
use pulldown_cmark::{CodeBlockKind, Event, Tag};
use regex::Regex;
use std::iter::Peekable;
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
    pub syntax: &'a SyntaxReference,
}

impl<'a> HighlightSpec<'a> {
    fn find(original: &str) -> Option<Self> {
        if original.is_empty() {
            return None;
        }
        let mapped = syntect_lang_name(original);

        let syntax = SS
            .find_syntax_by_name(mapped)
            .or_else(|| SS.find_syntax_by_extension(mapped))?;

        Some(Self {
            html_id: util::to_id(&syntax.name),
            syntax,
        })
    }
}

fn push_code_highlight<S: AsRef<str>>(s: &mut String, lang: Option<S>, code: &str) {
    if let Some(spec) = lang.as_ref().and_then(|x| HighlightSpec::find(x.as_ref())) {
        match highlight(&spec, code) {
            Ok(highlight) => {
                s.push_str(r#"<code class="highlight "#);
                s.push_str(&spec.html_id);
                s.push_str(r#"">"#);
                s.push_str(&highlight);
                s.push_str("</code>");
                return;
            }
            Err(err) => {
                panic!("Syntax highlight error: {}", err);
            }
        }
    };

    if let Some(lang) = lang {
        warn!("No highlighter found for: {:?}", lang.as_ref())
    }

    s.push_str("<code>");
    s.push_str(&html_escape::encode_safe(&code));
    s.push_str("</code>");
}

fn extra_code_class(s: &str) -> Option<&'static str> {
    let count = largest_line_count(s);

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

    // Strip wrapping <span>, later to be replaced with a <code> tag.
    // FIXME panics if highlighting is empty
    let cap = SPAN_WRAPPER
        .captures(&generated)
        .expect("Failed to match syntax span");
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

        // "pollen" => "",
        x => x,
    }
}

pub struct CodeBlockSyntaxHighlight<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
}

impl<'a, I: Iterator<Item = Event<'a>>> CodeBlockSyntaxHighlight<'a, I> {
    pub fn new(parent: I) -> Self {
        Self { parent }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for CodeBlockSyntaxHighlight<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let lang = match self.parent.next()? {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => lang,
            other => return Some(other),
        };

        let lang = parse_code_spec(&lang);

        let mut code = String::new();
        // Next should eat the End event as well.
        while let Some(Event::Text(text)) = self.parent.next() {
            code.push_str(&text);
        }

        let mut res = String::new();

        match extra_code_class(&code) {
            Some(class) => {
                res.push_str(r#"<pre class=""#);
                res.push_str(class);
                res.push_str(r#"">"#);
            }
            None => {
                res.push_str("<pre>");
            }
        }
        push_code_highlight(&mut res, lang, &code);
        res.push_str("</pre>");

        Some(Event::Html(res.into()))
    }
}

pub struct InlineCodeSyntaxHighlight<'a, I: Iterator<Item = Event<'a>>> {
    parent: Peekable<I>,
    trailing: Option<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> InlineCodeSyntaxHighlight<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.peekable(),
            trailing: None,
        }
    }
}

lazy_static! {
    static ref BLOCK_CODE_SPEC: Regex = Regex::new(r"^\w+$").unwrap();
}
lazy_static! {
    static ref INLINE_CODE_SPEC: Regex = Regex::new(r"^(\w+)(.*?)$").unwrap();
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for InlineCodeSyntaxHighlight<'a, I> {
    type Item = Event<'a>;

    // Maybe we should support `code`{.lang} or something too?
    // It's possible if we peek for it.
    fn next(&mut self) -> Option<Self::Item> {
        if self.trailing.is_some() {
            return self.trailing.take();
        }

        let code = match self.parent.next()? {
            Event::Code(code) => code,
            other => return Some(other),
        };

        match self.parent.peek() {
            Some(Event::Text(txt)) => {
                if !INLINE_CODE_SPEC.is_match(txt) {
                    return Some(Event::Code(code));
                }
            }
            _ => return Some(Event::Code(code)),
        }

        let txt = match self.parent.next()? {
            Event::Text(txt) => txt,
            other => panic!("Should have peeked a surefire match, got: {other:?}"),
        };

        let (lang, trailing) = match inline_code_spec(&txt) {
            Some(x) => x,
            None => panic!(
                "Inline code spec not matched: {}",
                format!("`{code}`{txt}").red()
            ),
        };

        if !trailing.is_empty() {
            self.trailing = Some(Event::Text(trailing.into()));
        }

        let mut res = String::new();
        push_code_highlight(&mut res, Some(&lang), &code);
        Some(Event::Html(res.into()))
    }
}

fn inline_code_spec(s: &str) -> Option<(String, String)> {
    if s.is_empty() {
        return None;
    }
    let captures = INLINE_CODE_SPEC.captures(s)?;

    Some((captures[1].to_string(), captures[2].to_string()))
}

fn parse_code_spec(s: &str) -> Option<String> {
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
    use pulldown_cmark::{html, Options, Parser};

    fn convert(s: &str) -> String {
        let parser = Parser::new_ext(s, Options::all());
        let transformed = CodeBlockSyntaxHighlight::new(parser);
        let transformed = InlineCodeSyntaxHighlight::new(transformed);
        let mut body = String::new();
        html::push_html(&mut body, transformed);
        body
    }

    #[test]
    fn test_highlight_code_block() {
        let s = r"
```rust
let x = 2;
```";
        let res = convert(s);
        assert!(res.starts_with(r#"<pre><code class="highlight rust">"#));
        assert!(res.trim_end().ends_with("</code></pre>"));
    }

    #[test]
    fn test_highlight_extension() {
        let s = r"
```ml
let square x = x * x
```";
        let res = convert(s);
        assert!(res.starts_with(r#"<pre><code class="highlight ocaml">"#));
        assert!(res.trim_end().ends_with("</code></pre>"));
    }

    #[test]
    fn test_highlight_inline_code() {
        // Text
        // ..
        // Code
        // Text("rust end")
        let s = r"Inline `let x = 2;`rust end";
        let res = convert(s);
        assert!(res.starts_with(r#"<p>Inline <code class="highlight rust">"#));
        assert!(res.trim_end().ends_with(r#"</code> end</p>"#));
    }

    #[test]
    fn test_highlight_inline_code_no_lang() {
        // Text
        // ..
        // Code
        // Text(" no lang")
        let s = r"Inline `xxx` no lang";
        let res = convert(s);
        assert_eq!(res.trim_end(), "<p>Inline <code>xxx</code> no lang</p>");
    }

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
