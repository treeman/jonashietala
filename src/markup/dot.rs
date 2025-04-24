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
    Ok(convert_nodes_to_icons(String::from_utf8(graph_svg)?))
}

fn convert_nodes_to_icons(svg: String) -> String {
    svg
}

#[cfg(test)]
mod tests {
    // use super::*;
    //
    //     #[test]
    //     fn test_insert_icons() {
    //         let input = r#"
    // <g id="node2" class="node switch">
    // <title>switch_office</title>
    // <ellipse fill="none" stroke="black" cx="99.27" cy="-89.39" rx="91.27" ry="18"/>
    // <text text-anchor="middle" x="99.27" y="-84.34" font-family="Times,serif" font-size="14.00">switch | 192.168.1.38</text>
    // </g>
    // <!-- ap_office -->
    // <g id="node3" class="node ap">
    // <title>ap_office</title>
    // <ellipse fill="none" stroke="black" cx="212.53" cy="-145.49" rx="75.92" ry="18"/>
    // <text text-anchor="middle" x="212.53" y="-140.44" font-family="Times,serif" font-size="14.00">ap | 192.168.1.21</text>
    // </g>"#;
    //
    //         assert_eq!(
    //             convert_to_svg(input).unwrap(),
    //             r#"
    // <g id="node2" class="node switch">
    // <title>switch_office</title>
    // <ellipse fill="none" stroke="black" cx="99.27" cy="-89.39" rx="91.27" ry="18"/>
    // <text text-anchor="middle" x="99.27" y="-84.34" font-family="Times,serif" font-size="14.00">switch | 192.168.1.38</text>
    // </g>
    // <!-- ap_office -->
    // <g id="node3" class="node ap">
    // <title>ap_office</title>
    // <ellipse fill="none" stroke="black" cx="212.53" cy="-145.49" rx="75.92" ry="18"/>
    // <text text-anchor="middle" x="212.53" y="-140.44" font-family="Times,serif" font-size="14.00">ap | 192.168.1.21</text>
    // </g>"#
    //         );
    //     }
}
