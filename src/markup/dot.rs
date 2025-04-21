use crate::util;
use camino::Utf8PathBuf;
use eyre::{eyre, Result};
use graphviz_rust::dot_structures::*;
use graphviz_rust::{cmd::Format, exec, parse, printer::PrinterContext};
use std::fs;

pub fn generate_dot(src: &str) -> Result<String> {
    let mut svg_path = Utf8PathBuf::from("images/dot");
    svg_path.push(src);
    svg_path.set_extension("svg");

    let svg = if svg_path.exists() {
        fs::read_to_string(svg_path)?
    } else {
        let content = fs::read_to_string(src)?;
        let generated = convert_to_svg(&content)?;
        util::write_to_file(svg_path, &generated)?;
        generated
    };

    Ok(svg)
}

fn convert_to_svg(dot: &str) -> Result<String> {
    let g: Graph = parse(dot).map_err(|err| eyre!("Error parsing dot: {err}"))?;
    let graph_svg = exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])?;
    Ok(String::from_utf8(graph_svg)?)
}
