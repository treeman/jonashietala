use crate::paths::RelPath;
use crate::util;
use camino::{Utf8Path, Utf8PathBuf};
use eyre::{eyre, Result};
use graphviz_rust::dot_structures::*;
use graphviz_rust::{cmd::Format, exec, parse, printer::PrinterContext};
use std::fs;
use tracing::info;

pub fn generate_dot(src: &RelPath) -> Result<String> {
    let output_path = svg_path(src);
    let svg = if output_path.exists() {
        fs::read_to_string(output_path)?
    } else {
        read_dot_write_svg(src, &output_path)?
    };

    Ok(svg)
}

pub fn regenerate_dot(src: &RelPath) -> Result<String> {
    let output_path = svg_path(src);
    read_dot_write_svg(src, &output_path)
}

fn read_dot_write_svg(src: &RelPath, output_path: &Utf8Path) -> Result<String> {
    info!("Convert dot to svg: {} -> {}", src, output_path);
    let content = fs::read_to_string(src.0.as_str())?;
    let generated = convert_to_svg(&content)?;
    util::write_to_file(output_path, &generated)?;
    Ok(generated)
}

fn svg_path(src: &RelPath) -> Utf8PathBuf {
    let mut svg_path = Utf8PathBuf::from("images/dot");
    svg_path.push(src.0.as_str());
    svg_path.set_extension("svg");
    svg_path
}

fn convert_to_svg(dot: &str) -> Result<String> {
    let g: Graph = parse(dot).map_err(|err| eyre!("Error parsing dot: {err}"))?;
    let graph_svg = exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])?;
    Ok(String::from_utf8(graph_svg)?)
}
