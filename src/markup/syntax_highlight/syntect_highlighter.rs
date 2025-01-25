use crate::util;
use camino::{Utf8Path, Utf8PathBuf};
use colored::Colorize;
use eyre::eyre;
use eyre::Result;
use lazy_static::lazy_static;
use regex::Regex;
use syntect::parsing::SyntaxReference;
use syntect::{
    dumps, highlighting::ThemeSet, html, html::ClassStyle, html::ClassedHTMLGenerator,
    parsing::SyntaxSet, util::LinesWithEndings,
};
use tracing::info;

pub struct SyntectHighlighter<'a> {
    syntax: &'a SyntaxReference,
}

impl SyntectHighlighter<'_> {
    pub fn find(original: &str) -> Option<Self> {
        if original.is_empty() {
            return None;
        }
        let lang_id = to_language_id(original);

        let syntect_name = syntect_lang_name(&lang_id);

        let syntax = SS
            .find_syntax_by_name(syntect_name)
            .or_else(|| SS.find_syntax_by_extension(syntect_name))?;

        Some(Self { syntax })
    }

    pub fn highlight(&self, code: &str) -> Result<String> {
        let mut html_generator =
            ClassedHTMLGenerator::new_with_class_style(self.syntax, &SS, ClassStyle::Spaced);
        for line in LinesWithEndings::from(code) {
            html_generator.parse_html_for_line_which_includes_newline(line)?;
        }

        // The purpose here is to remove the span that wraps the entire output.
        // Usually that's not a problem, but I want to wrap each line in their own element
        // so I can highlight lines. An element that wraps the entire output makes this hard to
        // accomplish.
        let html = html_generator.finalize();
        if html.is_empty() {
            return Ok(html);
        }

        if let Some(caps) = SOURCE_SPAN.captures(&html) {
            Ok(caps[1].to_string())
        } else {
            Err(eyre!(
                "`source` span isn't wrapping highlighted code: `{html}`"
            ))
        }
    }
}

pub fn dump_syntax_binary() -> Result<()> {
    let file = "syntaxes/syntax_set.packdump";
    info!("Dumping syntax binary to: {file}");
    let mut builder = SyntaxSet::load_defaults_newlines().into_builder();
    builder.add_from_folder("syntaxes", true).unwrap();
    let ss = builder.build();
    dumps::dump_to_uncompressed_file(&ss, file)?;
    Ok(())
}

pub fn dump_theme(file: &Utf8Path) -> Result<()> {
    let theme = ThemeSet::get_theme(file)?;
    let output_file =
        Utf8PathBuf::from_path_buf(format!("css/themes/{}.scss", file.file_stem().unwrap()).into())
            .unwrap();
    let css = html::css_for_theme_with_class_style(&theme, ClassStyle::Spaced)?;
    util::write_to_file(&output_file, css)?;
    println!("{} {}", "[Created]".green(), output_file);
    Ok(())
}

lazy_static! {
    static ref SS: SyntaxSet = syntax_set();
    static ref SOURCE_SPAN: Regex =
        Regex::new(r#"(?s)^<span class="(?:source|text) [^"]+">(.+)</span>$"#).unwrap();
}

// Use a binary dump to radically speedup SyntaxSet creation.
// If a syntax file is changed the below dump function updates it.
fn syntax_set() -> SyntaxSet {
    dumps::from_uncompressed_data(include_bytes!("../../../syntaxes/syntax_set.packdump")).unwrap()
}

// Elixir uses new unsupported features:
// https://github.com/trishume/syntect/issues/323
// Should update the submodule when it's fixed to get heex support.
//
// See Zola for more language refs:
// https://github.com/getzola/zola/tree/master/sublime/syntaxes
fn syntect_lang_name(lang: &str) -> &str {
    match lang {
        // Some language specs used that don't fit syntect's file extension or name.
        "elixir" => "Elixir",
        "haskell" => "Haskell",
        "perl" => "Perl",
        "python" => "Python",
        "python3" => "Python",
        "racket" => "Racket",
        "ruby" => "Ruby",
        "rust" => "Rust",
        "shell" => "Shell-Unix-Generic",
        "erlang" => "Erlang",
        "js" => "JavaScript",
        "djot" => "Djot",

        // "pollen" => "",
        x => x,
    }
}

fn to_language_id(s: &str) -> String {
    let s = s.trim().to_lowercase();

    match s.as_str() {
        "c++" => "cpp".to_string(),
        "javascript" => "js".to_string(),
        _ => s,
    }
}
