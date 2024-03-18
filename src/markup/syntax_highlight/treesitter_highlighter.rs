use eyre::Result;
use itertools::Itertools;
use lazy_static::lazy_static;
use std::collections::HashMap;
use tracing::warn;
use tree_sitter_highlight::{HighlightConfiguration, Highlighter, HtmlRenderer};

pub struct TreesitterHighlighter<'a> {
    config: &'a HighlightConfiguration,
}

impl<'a> TreesitterHighlighter<'a> {
    pub fn find(lang_id: &str) -> Option<Self> {
        CONFIGS.get(lang_id).map(|config| Self { config })
    }

    pub fn highlight(&self, code: &str) -> Result<String> {
        let mut highlighter = Highlighter::new();

        let highlights = highlighter
            .highlight(self.config, code.as_bytes(), None, |lang| {
                let res = CONFIGS.get(lang);
                if !res.is_some() {
                    warn!("Couldn't find treesitter grammar for `{lang}` to inject");
                }
                res
            })
            .unwrap();

        // This isn't very nice... How to generate strings dynamically from inside a Fn closure
        // that returns a byte slice?
        // Not very easily.
        let mut renderer = HtmlRenderer::new();
        renderer.render(highlights, code.as_bytes(), &|attr| {
            CLASSES[attr.0].as_bytes()
        })?;
        let res = renderer.lines().join("");
        Ok(res)
    }
}

static HIGHLIGHT_NAMES: &[&str] = &[
    "attribute",
    "boolean",
    "character",
    "comment",
    "comment.documentation",
    "constant",
    "constant.builtin",
    "define",
    "exception",
    "fold",
    "function",
    "function.builtin",
    "function.call",
    "function.macro",
    "indent.align",
    "indent.auto",
    "indent.begin",
    "indent.branch",
    "indent.dedent",
    "indent.end",
    "indent.ignore",
    "indent.zero",
    "injection.content",
    "injection.language",
    "keyword",
    "keyword.conditional",
    "keyword.conditional.ternary",
    "keyword.coroutine",
    "keyword.debug",
    "keyword.directive",
    "keyword.directive.define",
    "keyword.exception",
    "keyword.function",
    "keyword.import",
    "keyword.operator",
    "keyword.repeat",
    "keyword.return",
    "keyword.storage",
    "label",
    "local.definition",
    "local.definition.field",
    "local.definition.function",
    "local.definition.import",
    "local.definition.macro",
    "local.definition.method",
    "local.definition.namespace",
    "local.definition.parameter",
    "local.definition.type",
    "local.definition.var",
    "local.reference",
    "local.scope",
    "markup",
    "markup.caption",
    "markup.delete",
    "markup.fixme",
    "markup.footnote",
    "markup.footnote.definition",
    "markup.footnote.reference",
    "markup.heading.1",
    "markup.heading.2",
    "markup.heading.3",
    "markup.heading.4",
    "markup.heading.5",
    "markup.heading.6",
    "markup.highlighted",
    "markup.insert",
    "markup.italic",
    "markup.link.definition",
    "markup.link.label",
    "markup.link.reference",
    "markup.link.url",
    "markup.list",
    "markup.list.checked",
    "markup.list.unchecked",
    "markup.math",
    "markup.note",
    "markup.quote",
    "markup.raw",
    "markup.strong",
    "markup.subscript",
    "markup.superscript",
    "markup.symbol",
    "markup.todo",
    "module",
    "nospell",
    "number",
    "number.float",
    "operator",
    "property",
    "punctuation",
    "punctuation.bracket",
    "punctuation.delimiter",
    "punctuation.special",
    "spell",
    "string",
    "string.escape",
    "string.special",
    "tag",
    "tag.attribute",
    "tag.delimiter",
    "text.title",
    "type",
    "type.builtin",
    "type.definition",
    "type.qualifier",
    "variable",
    "variable.builtin",
    "variable.member",
    "variable.parameter",
];

lazy_static! {
    static ref CLASSES: Vec<String> = HIGHLIGHT_NAMES
        .iter()
        .map(|name| format!(r#"class="{}""#, name.replace(".", " ")))
        .collect();
    static ref CONFIGS: HashMap<String, HighlightConfiguration> = init_configurations();
}

fn init_configurations() -> HashMap<String, HighlightConfiguration> {
    [
        // (
        //     "rust",
        //     HighlightConfiguration::new(
        //         tree_sitter_rust::language(),
        //         tree_sitter_rust::HIGHLIGHT_QUERY,
        //         tree_sitter_rust::INJECTIONS_QUERY,
        //         "",
        //     )
        //     .unwrap(),
        // ),
        (
            "sdjot",
            HighlightConfiguration::new(
                tree_sitter_sdjot::language(),
                tree_sitter_sdjot::HIGHLIGHTS_QUERY,
                tree_sitter_sdjot::INJECTIONS_QUERY,
                "",
            )
            .unwrap(),
        ),
        (
            "djot",
            HighlightConfiguration::new(
                tree_sitter_djot::language(),
                tree_sitter_djot::HIGHLIGHTS_QUERY,
                tree_sitter_djot::INJECTIONS_QUERY,
                "",
            )
            .unwrap(),
        ),
        (
            "gleam",
            HighlightConfiguration::new(
                tree_sitter_gleam::language(),
                tree_sitter_gleam::HIGHLIGHTS_QUERY,
                "",
                tree_sitter_gleam::LOCALS_QUERY,
            )
            .unwrap(),
        ),
        (
            "fish",
            HighlightConfiguration::new(
                tree_sitter_fish::language(),
                tree_sitter_fish::HIGHLIGHTS_QUERY,
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
        assert_eq!(
            highlighter.highlight("let x = 2;").unwrap(),
            "<span class=\"keyword\">let</span> x = <span class=\"constant\">2</span><span class=\"punctuation delimiter\">;</span>\n"
        );
    }

    #[test]
    fn test_treesitter_highlight_sdjot_injection() {
        let lang = "sdjot";
        let highlighter = TreesitterHighlighter::find(lang).unwrap();
        assert_eq!(
            highlighter.highlight("```rust
let x = 2;
```
").unwrap(),
"<span class=\"markup raw\"><span class=\"punctuation delimiter\">```</span><span class=\"tag attribute\">rust</span>
<span class=\"keyword\">let</span> x = <span class=\"constant builtin\">2</span><span class=\"punctuation delimiter\">;</span>
<span class=\"punctuation delimiter\">```</span></span>
"
        );
    }
}
