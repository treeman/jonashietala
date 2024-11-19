mod attrs;
mod auto_figures;
mod code;
mod embed_youtube;
mod fenced_blocks;
mod html;
mod pd_html;
mod quote_attrs;
mod table_attrs;
mod transform_headers;

use auto_figures::AutoFigures;
use code::{CodeBlockSyntaxHighlight, InlineCodeSyntaxHighlight};
use embed_youtube::EmbedYoutube;
use fenced_blocks::parse_fenced_blocks;
use pulldown_cmark::{BrokenLink, CowStr, LinkType, Options, Parser};
use quote_attrs::QuoteAttrs;
use std::borrow::Cow;
use table_attrs::TableAttrs;
use tracing::warn;
use transform_headers::TransformHeaders;

use crate::util;

use crate::markup::{FeedHtml, Html, HtmlParseRes};

pub fn markdown_to_html(markdown: &str) -> HtmlParseRes {
    HtmlParseRes {
        html: Html(parse_markdown(&preprocess(markdown))),
        lookup: None,
        embedded_files: Vec::new(),
    }
}

pub fn markdown_to_html_feed(markdown: &str) -> FeedHtml {
    FeedHtml(parse_markdown_to_feed(&preprocess(markdown)))
}

fn preprocess(s: &str) -> Cow<str> {
    parse_fenced_blocks(s)
}

fn parse_markdown(s: &str) -> String {
    let mut cb = |link: BrokenLink<'_>| -> Option<(CowStr, CowStr)> {
        match link.link_type {
            // Try to convert shortcut links to fragment links
            LinkType::Shortcut => {
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
    let transformed = EmbedYoutube::new(transformed, true);
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
    let transformed = EmbedYoutube::new(transformed, false);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = QuoteAttrs::new(transformed);
    let transformed = TableAttrs::new(transformed);

    let mut body = String::new();
    pd_html::push_html(&mut body, transformed);
    body
}

fn display_broken_link(link: &BrokenLink<'_>, markdown: &str) {
    warn!("Broken link: {}", &markdown[link.span.clone()]);
}
