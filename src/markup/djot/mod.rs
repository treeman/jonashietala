mod auto_figures;
mod changelog;
mod code;
mod div_transforms;
mod drop_offset;
mod embed_dot;
mod embed_svg;
mod embed_youtube;
mod lookup_register;
mod quote_transforms;
mod strip_elements;
mod symbol_transforms;
mod table_of_content;
mod todos;
mod transform_headers;

use self::auto_figures::AutoFigures;
use self::changelog::move_changelog;
use self::code::{CodeBlockSyntaxHighlight, InlineCodeSyntaxHighlight};
pub use self::div_transforms::DivTransform;
use self::div_transforms::DivTransforms;
use self::drop_offset::DropOffset;
use self::embed_dot::EmbedDot;
use self::embed_svg::EmbedSvg;
use self::embed_youtube::EmbedYoutube;
use self::lookup_register::LookupRegister;
use self::quote_transforms::QuoteTransforms;
use self::strip_elements::{StripElements, StripSymbols};
pub use self::symbol_transforms::SymbolTransform;
use self::symbol_transforms::SymbolTransforms;
use self::table_of_content::{insert_toc, remove_toc};
use self::todos::TransformTodoComments;
use self::transform_headers::TransformHeaders;
use crate::markup::{self, Html, HtmlParseRes, MarkupLookup, ParseContext};
use eyre::Result;
use jotdown::{html::Renderer, Parser, Render};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

pub fn djot_to_html(djot: &str, context: ParseContext) -> Result<HtmlParseRes> {
    let lookup = Rc::new(RefCell::new(MarkupLookup::new(
        djot,
        context.markup_meta_line_count,
    )));

    let embedded_files = Rc::new(RefCell::new(HashSet::new()));

    let transformed = Parser::new(djot).into_offset_iter();

    let transformed = LookupRegister::new(transformed, djot, lookup.clone(), context);
    let transformed = TransformTodoComments::new(transformed, context, lookup.clone());
    let transformed = DropOffset::new(transformed);

    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = EmbedSvg::new(transformed, embedded_files.clone());
    let transformed = EmbedDot::new(transformed, embedded_files.clone());
    let transformed = EmbedYoutube::new(transformed, true);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = DivTransforms::new(transformed);
    let transformed = SymbolTransforms::new(transformed);
    let transformed = QuoteTransforms::new(transformed);

    let mut body = String::new();
    Renderer::default().push(transformed, &mut body)?;

    let lookup = Rc::try_unwrap(lookup)
        .expect("Should be able to unwrap lookup")
        .into_inner();

    let embedded_files = Rc::try_unwrap(embedded_files)
        .expect("Should be able to unwrap embedded_files")
        .into_inner();

    body = insert_toc(&body, &lookup).to_string();
    body = move_changelog(&body).to_string();

    Ok(HtmlParseRes {
        html: Html(body),
        lookup: Some(lookup),
        embedded_files,
    })
}

pub fn djot_to_html_feed(djot: &str) -> Result<markup::FeedHtml> {
    let transformed = Parser::new(djot);
    let transformed = StripSymbols::new(transformed, ["table-of-content"].into());
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = EmbedYoutube::new(transformed, false);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = DivTransforms::new(transformed);
    let transformed = QuoteTransforms::new(transformed);

    let mut body = String::new();
    Renderer::default().push(transformed, &mut body)?;

    body = remove_toc(&body).to_string();
    body = move_changelog(&body).to_string();

    Ok(markup::FeedHtml(body))
}

pub fn djot_to_html_stripped(djot: &str) -> Result<Html> {
    let transformed = Parser::new(djot);
    let transformed = StripElements::new(transformed);
    let mut body = String::new();
    Renderer::default().push(transformed, &mut body)?;
    Ok(Html(body))
}
