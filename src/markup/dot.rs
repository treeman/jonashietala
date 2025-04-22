use crate::paths::RelPath;
use crate::util;
use camino::{Utf8Path, Utf8PathBuf};
use eyre::{eyre, Result};
use graphviz_rust::dot_structures::*;
use graphviz_rust::{cmd::Format, exec, parse, printer::PrinterContext};
use std::fs;
use tracing::info;

pub struct DotGenerationRes {
    pub path: RelPath,
    pub content: String,
}

pub fn generate_dot(src: &RelPath) -> Result<DotGenerationRes> {
    let output_path = svg_path(src);
    if output_path.0.exists() {
        Ok(DotGenerationRes {
            content: fs::read_to_string(&output_path.0)?,
            path: output_path,
        })
    } else {
        read_dot_write_svg(src, &output_path.0)
    }
}

pub fn regenerate_dot(src: &RelPath) -> Result<DotGenerationRes> {
    let output_path = svg_path(src);
    read_dot_write_svg(src, &output_path.0)
}

fn read_dot_write_svg(src: &RelPath, output_path: &Utf8Path) -> Result<DotGenerationRes> {
    info!("Convert dot to svg: {} -> {}", src, output_path);
    let content = fs::read_to_string(src.0.as_str())?;
    let generated = convert_to_svg(&content)?;
    util::write_to_file(output_path, &generated)?;

    Ok(DotGenerationRes {
        content: generated,
        path: RelPath(output_path.into()),
    })
}

fn svg_path(src: &RelPath) -> RelPath {
    let mut svg_path = Utf8PathBuf::from("images/dot");
    svg_path.push(src.0.as_str());
    svg_path.set_extension("svg");
    RelPath(svg_path)
}

fn convert_to_svg(dot: &str) -> Result<String> {
    let g: Graph = parse(dot).map_err(|err| eyre!("Error parsing dot: {err}"))?;
    let graph_svg = exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])?;
    Ok(convert_nodes_to_icons(dot, String::from_utf8(graph_svg)?))
}

fn convert_nodes_to_icons(dot: &str, svg: String) -> String {
    svg
}
