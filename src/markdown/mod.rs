mod attrs;
mod auto_figures;
mod embed_youtube;
mod fenced_blocks;
mod html;
mod pd_html;
mod quote_attrs;
mod syntax_highlight;
mod table_attrs;
mod transform_headers;

pub use syntax_highlight::{dump_syntax_binary, dump_theme};

use auto_figures::AutoFigures;
use camino::Utf8Path;
use embed_youtube::EmbedYoutube;
use fenced_blocks::parse_fenced_blocks;
use pulldown_cmark::{BrokenLink, CowStr, LinkType, Options, Parser};
use quote_attrs::QuoteAttrs;
use scraper::{Html, Selector};
use std::borrow::Cow;
use syntax_highlight::{CodeBlockSyntaxHighlight, InlineCodeSyntaxHighlight};
use table_attrs::TableAttrs;
use tracing::warn;
use transform_headers::TransformHeaders;
use walkdir::WalkDir;

use crate::{paths::FilePath, util};

pub fn markdown_to_html(markdown: &str) -> String {
    parse_markdown(&preprocess(markdown))
}

pub fn markdown_to_html_feed(markdown: &str) -> String {
    parse_markdown_to_feed(&preprocess(markdown))
}

fn preprocess(s: &str) -> Cow<str> {
    parse_fenced_blocks(s)
}

fn parse_markdown(s: &str) -> String {
    let mut cb = |link: BrokenLink<'_>| -> Option<(CowStr, CowStr)> {
        match link.link_type {
            // Try to convert shortcut links to fragment links
            LinkType::Shortcut => {
                // display_broken_link(&link, s);
                let reference = link.reference.into_string();
                let href = format!("#{}", util::to_id(&util::html_text(&reference)));
                let name = reference.into();
                Some((href.into(), name))
            }
            _ => {
                display_broken_link(&link, s);
                None
            }
        }
    };
    let transformed = Parser::new_with_broken_link_callback(s, Options::all(), Some(&mut cb));
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = EmbedYoutube::new(transformed);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = QuoteAttrs::new(transformed);
    let transformed = TableAttrs::new(transformed);

    let mut body = String::new();
    pd_html::push_html(&mut body, transformed);
    body
}

fn parse_markdown_to_feed(s: &str) -> String {
    let transformed = Parser::new_ext(s, Options::all());
    let transformed = AutoFigures::new(transformed);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = QuoteAttrs::new(transformed);
    let transformed = TableAttrs::new(transformed);

    let mut body = String::new();
    pd_html::push_html(&mut body, transformed);
    body
}

pub fn inline_markdown_to_html(markdown: &str) -> String {
    let parser = Parser::new_ext(markdown, Options::all());

    let mut body = String::new();
    pd_html::push_html(&mut body, parser);
    body
}

pub fn markdown_to_html_strip_one_paragraph(markdown: &str) -> String {
    let html = inline_markdown_to_html(markdown);
    strip_one_paragraph(&html)
}

pub fn strip_one_paragraph(html: &str) -> String {
    let parsed = Html::parse_fragment(html);
    let selector = Selector::parse("p").unwrap();
    let mut paragraphs = parsed.select(&selector);

    let first = match paragraphs.next() {
        Some(p) => p,
        // No paragraph, just skip it.
        None => return html.to_string(),
    };

    match paragraphs.next() {
        // At least two paragraphs, don't strip anything.
        Some(_) => html.to_string(),
        // Only one paragraph, strip it
        None => first.inner_html(),
    }
}

fn display_broken_link(link: &BrokenLink<'_>, markdown: &str) {
    warn!("Broken link: {}", &markdown[link.span.clone()]);
}

pub fn find_markdown_files<'a, P: 'a + AsRef<Utf8Path>>(dir: P) -> Vec<FilePath> {
    let dir = dir.as_ref();
    WalkDir::new(dir.as_std_path())
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| match e.metadata() {
            Ok(e) => !e.is_dir(),
            _ => false,
        })
        // .filter_map(|e| Utf8PathBuf::from_path_buf(e.into_path()).ok())
        .filter_map(|e| FilePath::from_std_path(dir, e.into_path()).ok())
        .filter(|e| has_markdown_ext(&e.rel_path.0))
        .collect()
}

fn has_markdown_ext(file: &Utf8Path) -> bool {
    match file.extension() {
        Some(ext) => ext == "markdown" || ext == "md",
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_one_markdown_paragraph() {
        assert_eq!(
            markdown_to_html_strip_one_paragraph(r"One *thing*"),
            r"One <em>thing</em>"
        );

        assert_eq!(
            markdown_to_html_strip_one_paragraph("One\n\nTwo"),
            "<p>One</p>\n<p>Two</p>\n"
        );
    }
}
