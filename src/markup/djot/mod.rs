mod auto_figures;
mod broken_link;
mod code;
mod div_transforms;
mod embed_youtube;
mod quote_transforms;
mod transform_headers;

use eyre::Result;
use jotdown::{html::Renderer, Parser, Render};

use self::auto_figures::AutoFigures;
use self::broken_link::BrokenLink;
use self::code::{CodeBlockSyntaxHighlight, InlineCodeSyntaxHighlight};
use self::div_transforms::DivTransforms;
use self::embed_youtube::EmbedYoutube;
use self::quote_transforms::QuoteTransforms;
use self::transform_headers::TransformHeaders;
use crate::markup::{self, ParseContext};

// Syntax discussons in progress:
// Blockquote attribution: https://github.com/jgm/djot/issues/198
// Auto block-level figures: https://github.com/jgm/djot/issues/87
// Metadata: https://github.com/jgm/djot/issues/35
// Captions: https://github.com/jgm/djot/issues/31

pub fn djot_to_html(djot: &str, context: ParseContext) -> Result<markup::Html> {
    let transformed = Parser::new(djot);
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = EmbedYoutube::new(transformed);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = DivTransforms::new(transformed);
    let transformed = QuoteTransforms::new(transformed);
    let transformed = BrokenLink::new(transformed, context);

    let mut body = String::new();
    Renderer::default().push(transformed, &mut body)?;
    Ok(markup::Html(body))
}

pub fn djot_to_html_feed(djot: &str, context: ParseContext) -> Result<markup::FeedHtml> {
    let transformed = Parser::new(djot);
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = DivTransforms::new(transformed);
    let transformed = BrokenLink::new(transformed, context);

    let mut body = String::new();
    Renderer::default().push(transformed, &mut body)?;
    Ok(markup::FeedHtml(body))
}
