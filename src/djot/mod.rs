mod transform_headers;

use eyre::Result;
use jotdown::{html, Parser, Render};

use self::auto_figures::AutoFigures;
use self::transform_headers::TransformHeaders;

pub fn djot_to_html(djot: &str) -> Result<String> {
    let transformed = Parser::new(djot);
    let transformed = TransformHeaders::new(transformed);
    let transformed = AutoFigures::new(transformed);

    let mut body = String::new();
    html::Renderer::default().push(transformed, &mut body)?;
    Ok(body)
}
