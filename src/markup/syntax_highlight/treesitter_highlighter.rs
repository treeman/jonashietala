use eyre::Result;
use lazy_static::lazy_static;
use regex::Regex;
use std::borrow::Cow;
use std::collections::HashMap;
use tracing::warn;
use tree_sitter_highlight::{HighlightConfiguration, Highlighter, HtmlRenderer};

pub struct TreesitterHighlighter<'a> {
    config: &'a HighlightConfiguration,
}

impl TreesitterHighlighter<'_> {
    pub fn find(lang_id: &str) -> Option<Self> {
        CONFIGS.get(lang_id).map(|config| Self { config })
    }

    pub fn highlight(&self, code: &str) -> Result<String> {
        let mut highlighter = Highlighter::new();

        let highlights = highlighter.highlight(self.config, code.as_bytes(), None, |lang| {
            let res = CONFIGS.get(lang);
            if res.is_none() {
                warn!("Couldn't find treesitter grammar for `{lang}` to inject");
            }
            res
        })?;

        // This isn't very nice... How to generate strings dynamically from inside a Fn closure
        // that returns a byte slice?
        // Not very easily.
        let mut renderer = HtmlRenderer::new();
        renderer.render(highlights, code.as_bytes(), &|attr| {
            CLASSES[attr.0].as_bytes()
        })?;

        let mut res: Vec<Cow<str>> = renderer.lines().map(Into::into).collect();
        if self.config.language_name == "djot" {
            strip_empty_end_djot(&mut res);
        }

        Ok(res.join(""))
    }
}

fn strip_empty_end_djot(res: &mut Vec<Cow<str>>) {
    if let Some(last) = res.last() {
        let replaced = EMPTY_SPAN.replace_all(last, "");
        if replaced.as_ref() != last.as_ref() {
            let skip = replaced.is_empty() || replaced == "\n";
            let x = replaced.into_owned();
            res.pop();
            if !skip {
                res.push(Cow::Owned(x));
            }
        }
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
        .map(|name| format!(r#"class="{}""#, name.replace('.', " ")))
        .collect();
    static ref CONFIGS: HashMap<String, HighlightConfiguration> = init_configurations();
    static ref EMPTY_RAW_MARKUP_SPAN: Regex =
        Regex::new(r#"^<span class="markup [^"]*">(<span class="[^"]+"></span>)*</span>\n?$"#)
            .unwrap();
    static ref EMPTY_SPAN: Regex =
        Regex::new(r#"<span class="[^"]*">(<span class="[^"]+"></span>)*</span>"#).unwrap();
}

fn init_configurations() -> HashMap<String, HighlightConfiguration> {
    [
        (
            "sdjot",
            HighlightConfiguration::new(
                tree_sitter_sdjot::language(),
                "sdjot",
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
                "djot",
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
                "gleam",
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
                "fish",
                tree_sitter_fish::HIGHLIGHTS_QUERY,
                "",
                "",
            )
            .unwrap(),
        ),
        (
            "fish-shell",
            HighlightConfiguration::new(
                tree_sitter_fishshell::language(),
                "fish-shell",
                tree_sitter_fishshell::HIGHLIGHTS_QUERY,
                tree_sitter_fishshell::INJECTIONS_QUERY,
                "",
            )
            .unwrap(),
        ),
        (
            "toml",
            HighlightConfiguration::new(
                tree_sitter_toml::language(),
                "toml",
                tree_sitter_toml::HIGHLIGHTS_QUERY,
                "",
                "",
            )
            .unwrap(),
        ),
        (
            "query",
            HighlightConfiguration::new(
                tree_sitter_query::language(),
                "query",
                tree_sitter_query::HIGHLIGHTS_QUERY,
                "",
                "",
            )
            .unwrap(),
        ),
        (
            "ghostty",
            HighlightConfiguration::new(
                tree_sitter_ghostty::language(),
                "ghostty",
                tree_sitter_ghostty::HIGHLIGHTS_QUERY,
                "",
                "",
            )
            .unwrap(),
        ),
        // Not as good as syntect atm.
        // (
        //     "lua",
        //     HighlightConfiguration::new(
        //         tree_sitter_lua::language(),
        //         "lua",
        //         tree_sitter_lua::HIGHLIGHTS_QUERY,
        //         tree_sitter_lua::INJECTIONS_QUERY,
        //         tree_sitter_lua::LOCALS_QUERY,
        //     )
        //     .unwrap(),
        // ),
    ]
    .into_iter()
    .map(|(name, mut config)| {
        config.configure(HIGHLIGHT_NAMES);
        (name.to_string(), config)
    })
    .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_treesitter_highlight_gleam() {
        let highlighter = TreesitterHighlighter::find("gleam").unwrap();
        assert_eq!(
            highlighter.highlight("let x = 2;").unwrap(),
            "<span class=\"keyword\">let</span> <span class=\"variable\">x</span> <span class=\"punctuation delimiter\">=</span> <span class=\"number\">2</span>;\n"
            );
    }

    #[test]
    fn test_treesitter_highlight_sdjot_injection() {
        let lang = "sdjot";
        let highlighter = TreesitterHighlighter::find(lang).unwrap();
        assert_eq!(
            highlighter.highlight("```gleam
let x = 2;
```
").unwrap(),
"<span class=\"markup raw\"><span class=\"punctuation delimiter\">```</span><span class=\"tag attribute\">gleam</span></span>
<span class=\"markup raw\"><span class=\"keyword\">let</span> <span class=\"variable\">x</span> <span class=\"punctuation delimiter\">=</span> <span class=\"number\">2</span>;</span>
<span class=\"markup raw\"><span class=\"punctuation delimiter\">```</span></span>
"
        );
    }

    #[test]
    fn test_treesitter_highlight_djot() {
        // NOTE this isn't really what we want to do.
        // We want toml highlighting, not just `raw` rows.
        // But I suspect there's some bug in the rust tree-sitter library,
        // using the `tree-sitter` cli the grammars work.
        let highlighter = TreesitterHighlighter::find("djot").unwrap();
        assert_eq!(
            highlighter
                .highlight(
                    r#"---toml
title = "Title"
---
"#
                )
                .unwrap(),
            r#"<span class="markup raw"><span class="punctuation delimiter">---</span><span class="attribute">toml</span></span>
<span class="markup raw">title = &quot;Title&quot;</span>
<span class="markup raw"><span class="punctuation delimiter">---</span></span>
"#
        );
    }

    #[test]
    fn test_treesitter_highlight_strip_last_empty_djot() {
        let highlighter = TreesitterHighlighter::find("djot").unwrap();
        assert_eq!(
            highlighter
                .highlight(
                    r#"> Inside
"#
                )
                .unwrap(),
            // NOTE that there's a bug here with double quote markers...
            r#"<span class="markup quote"><span class="markup quote">&gt; </span><span class="spell">Inside</span></span>
"#
        );
    }

    #[test]
    fn test_treesitter_highlight_strip_last_empty_list_djot() {
        // NOTE there may be a bug here too
        // r#"<span class="markup raw"><span class="punctuation delimiter">```</span><span class="attribute">djot</span></span>
        // <span class="markup raw"><span class="markup list">- </span><span class="spell">List</span></span>
        // <span class="markup raw"><span class="spell"></span></span>
        // <span class="markup raw"><span class="markup list">  - </span><span class="spell">This is fine in both Djot an Markdown</span></span>
        // <span class="markup raw"><span class="punctuation delimiter">```</span></span>
        // "#
        let highlighter = TreesitterHighlighter::find("djot").unwrap();
        assert_eq!(
            highlighter
                .highlight(
                    r#"```djot
- List

  - This is fine in both Djot an Markdown
```
"#
                )
                .unwrap(),
            r#"<span class="markup raw"><span class="punctuation delimiter">```</span><span class="attribute">djot</span></span>
<span class="markup raw"><span class="markup list">- </span><span class="spell">List</span></span>
<span class="markup raw"><span class="spell"></span></span>
<span class="markup raw">  <span class="markup list">- </span><span class="spell">This is fine in both Djot an Markdown</span></span>
<span class="markup raw"><span class="spell"></span><span class="punctuation delimiter">```</span></span>
"#
        );
    }
}
