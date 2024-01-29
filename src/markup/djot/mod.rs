mod auto_figures;
mod code;
mod div_transforms;
mod embed_youtube;
mod quote_transforms;
mod transform_headers;

use eyre::Result;
use jotdown::{html::Renderer, Parser, Render};

use self::auto_figures::AutoFigures;
use self::code::{CodeBlockSyntaxHighlight, InlineCodeSyntaxHighlight};
use self::div_transforms::DivTransforms;
use self::embed_youtube::EmbedYoutube;
use self::quote_transforms::QuoteTransforms;
use self::transform_headers::TransformHeaders;
use crate::markup;

pub fn djot_to_html(djot: &str) -> Result<markup::Html> {
    let transformed = Parser::new(djot);
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = EmbedYoutube::new(transformed);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = DivTransforms::new(transformed);
    let transformed = QuoteTransforms::new(transformed);

    let mut body = String::new();
    Renderer::default().push(transformed, &mut body)?;
    Ok(markup::Html(body))
}

pub fn djot_to_html_feed(djot: &str) -> Result<markup::FeedHtml> {
    let transformed = Parser::new(djot);
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = DivTransforms::new(transformed);

    let mut body = String::new();
    Renderer::default().push(transformed, &mut body)?;
    Ok(markup::FeedHtml(body))
}
