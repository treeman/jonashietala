use std::collections::HashMap;

use eyre::Result;
use lazy_static::lazy_static;
use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter};

pub struct TreesitterHighlighter<'a> {
    config: &'a HighlightConfiguration,
}

impl<'a> TreesitterHighlighter<'a> {
    pub fn find(lang_id: &str) -> Option<Self> {
        CONFIGS.get(lang_id).map(|config| Self { config })
    }

    pub fn highlight(&self, lang_id: &str, code: &str) -> Result<String> {
        let mut highlighter = Highlighter::new();

        let highlights = highlighter
            .highlight(self.config, code.as_bytes(), None, |_| None)
            .unwrap();

        // This isn't very nice... How to generate strings dynamically from inside a Fn closure
        // that returns a byte slice?
        // Not very easily.
        // let mut renderer = HtmlRenderer::new();
        // renderer.render(highlights, code.as_bytes(), &|attr| {
        //     CLASSES[attr.0].as_bytes()
        // })?;
        // let res = renderer.lines().join("");
        // Ok(res)

        let mut res = String::new();

        for event in highlights {
            match event? {
                HighlightEvent::Source { start, end } => res.push_str(&code[start..end]),
                HighlightEvent::HighlightEnd => res.push_str("</span>"),
                HighlightEvent::HighlightStart(attr) => {
                    res.push_str(&format!(
                        r#"<span class="{} {}">"#,
                        HIGHLIGHT_NAMES[attr.0].replace(".", " "),
                        lang_id
                    ));
                }
            }
        }

        if !res.ends_with("\n") {
            res.push('\n');
        }

        Ok(res)
    }
}

// TODO more things...
static HIGHLIGHT_NAMES: &[&str] = &[
    "attribute",
    "constant",
    "function.builtin",
    "function",
    "keyword",
    "operator",
    "property",
    "punctuation",
    "punctuation.bracket",
    "punctuation.delimiter",
    "punctuation.delimiter.regex",
    "string",
    "string.special",
    "tag",
    "type",
    "type.builtin",
    "variable",
    "variable.builtin",
    "variable.parameter",
];

lazy_static! {
    // static ref CLASSES: Vec<String> = HIGHLIGHT_NAMES
    //     .iter()
    //     .map(|name| format!(r#"class="{}""#, name.replace(".", " ")))
    //     .collect();
    static ref CONFIGS: HashMap<String, HighlightConfiguration> = init_configurations();
}

fn init_configurations() -> HashMap<String, HighlightConfiguration> {
    [
        (
            "rust",
            HighlightConfiguration::new(
                tree_sitter_rust::language(),
                tree_sitter_rust::HIGHLIGHT_QUERY,
                tree_sitter_rust::INJECTIONS_QUERY,
                "",
            )
            .unwrap(),
        ),
        (
            "c",
            HighlightConfiguration::new(
                tree_sitter_c::language(),
                tree_sitter_c::HIGHLIGHT_QUERY,
                "",
                "",
            )
            .unwrap(),
        ),
    ]
    .into_iter()
    .map(|(name, mut config)| {
        config.configure(&HIGHLIGHT_NAMES);
        (name.to_string(), config)
    })
    .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_treesitter_highlight() {
        let highlighter = TreesitterHighlighter::find("rust").unwrap();
        assert_eq!(highlighter.highlight("rust", "let x = 2;").unwrap(), "<span class=\"keyword rust\">let</span> x = <span class=\"constant rust\">2</span><span class=\"punctuation delimiter rust\">;</span>\n");

        //         assert_eq!(
        //             highlighter
        //                 .highlight(
        //                     "rust",
        //                     r##"
        // lazy_static! {
        //     static ref BLOCK: Regex = Regex::new(
        //         r#"
        //         ^
        //         $
        //         "#
        //     ).unwrap()
        // }
        // "##
        //                 )
        //                 .unwrap(),
        //             "x"
        //         );
    }
}
