mod auto_figures;
mod code;
mod div_transforms;
mod embed_youtube;
mod transform_headers;

use eyre::Result;
use jotdown::{html, Parser, Render};

use self::auto_figures::AutoFigures;
use self::code::{CodeBlockSyntaxHighlight, InlineCodeSyntaxHighlight};
use self::div_transforms::DivTransforms;
use self::embed_youtube::EmbedYoutube;
use self::transform_headers::TransformHeaders;

pub fn djot_to_html(djot: &str) -> Result<String> {
    let transformed = Parser::new(djot);
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = EmbedYoutube::new(transformed);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = DivTransforms::new(transformed);

    let mut body = String::new();
    html::Renderer::default().push(transformed, &mut body)?;
    Ok(body)
}

pub fn djot_to_html_feed(djot: &str) -> Result<String> {
    let transformed = Parser::new(djot);
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);
    let transformed = CodeBlockSyntaxHighlight::new(transformed);
    let transformed = InlineCodeSyntaxHighlight::new(transformed);
    let transformed = DivTransforms::new(transformed);

    let mut body = String::new();
    html::Renderer::default().push(transformed, &mut body)?;
    Ok(body)
}
