mod djot;
mod embed_youtube;
mod graphs;
mod markdown;
pub mod markup_lookup;
mod syntax_highlight;

use camino::Utf8Path;
use eyre::{eyre, Result};
use lazy_static::lazy_static;
use regex::Regex;
use serde::de::DeserializeOwned;
use std::borrow::Cow;
use std::fs;
use std::ops::Deref;
use tracing::{error, warn};
use walkdir::WalkDir;
use yaml_front_matter::{Document, YamlFrontMatter};

pub use self::djot::{DivTransform, SymbolTransform};
pub use self::syntax_highlight::syntect_highlighter;
pub use markup_lookup::MarkupLookup;

use self::djot::{djot_to_html, djot_to_html_feed};
use self::markdown::{markdown_to_html, markdown_to_html_feed};
use crate::paths::AbsPath;
use crate::paths::FilePath;

#[derive(Debug, Copy, Clone)]
pub enum MarkupType {
    Markdown,
    Djot,
}

impl MarkupType {
    pub fn from_file(file: &Utf8Path) -> Option<Self> {
        file.extension().and_then(Self::from_extension)
    }

    pub fn from_extension(ext: &str) -> Option<Self> {
        match ext {
            "markdown" | "md" => Some(MarkupType::Markdown),
            "dj" => Some(MarkupType::Djot),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Html(pub String);

impl Deref for Html {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl Html {
    pub fn strip_one_paragraph(self) -> Html {
        Html(strip_one_paragraph(self.0.into()).into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FeedHtml(pub String);

impl Deref for FeedHtml {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

pub struct HtmlParseRes {
    pub html: Html,
    pub lookup: Option<MarkupLookup>,
}

#[derive(Debug, Clone)]
pub enum Markup {
    Markdown(String),
    Djot(String),
}

impl Markup {
    pub fn new(s: String, t: MarkupType) -> Self {
        match t {
            MarkupType::Markdown => Self::Markdown(s),
            MarkupType::Djot => Self::Djot(s),
        }
    }

    pub fn t(&self) -> MarkupType {
        match self {
            Self::Markdown(_) => MarkupType::Markdown,
            Self::Djot(_) => MarkupType::Djot,
        }
    }

    pub fn parse(&self, context: ParseContext) -> Result<HtmlParseRes> {
        match self {
            Self::Markdown(s) => Ok(markdown_to_html(s)),
            Self::Djot(s) => djot_to_html(s, context.in_feed(false)),
        }
    }

    pub fn parse_feed(&self, _context: ParseContext) -> Result<FeedHtml> {
        match self {
            Self::Markdown(s) => Ok(markdown_to_html_feed(s)),
            Self::Djot(s) => djot_to_html_feed(s),
        }
    }

    pub fn content(&self) -> &str {
        match self {
            Self::Markdown(s) => s.as_str(),
            Self::Djot(s) => s.as_str(),
        }
    }
}

#[derive(Debug)]
pub struct RawMarkupFile<Meta: DeserializeOwned> {
    pub markup: Markup,
    pub path: AbsPath,
    pub markup_meta: Meta,
    pub meta_line_count: usize,
}

impl<Meta: DeserializeOwned> RawMarkupFile<Meta> {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let content = fs::read_to_string(&path)?;
        Self::from_content(content, path)
    }

    pub fn from_content(content: String, path: AbsPath) -> Result<Self> {
        let t = MarkupType::from_file(&path)
            .ok_or_else(|| eyre!("Unsupported file format: `{}`", &path))?;
        let meta = ExtractMetadataRes::extract(t, &content, &path)?;

        Ok(Self {
            markup: Markup::new(meta.content, t),
            markup_meta: meta.metadata,
            meta_line_count: meta.meta_line_count,
            path,
        })
    }

    pub fn parse(self, context: ParseContext) -> Result<MarkupFile<Meta>> {
        let res = self.markup.parse(context.with_path(&self.path))?;
        Ok(MarkupFile {
            markup: self.markup,
            markup_lookup: res.lookup,
            html: res.html,
            path: self.path,
            markup_meta: self.markup_meta,
        })
    }
}

pub struct ExtractMetadataRes<Meta: DeserializeOwned> {
    pub metadata: Meta,
    pub content: String,
    pub meta_line_count: usize,
}

impl<Meta: DeserializeOwned> ExtractMetadataRes<Meta> {
    pub fn extract(t: MarkupType, content: &str, path: &AbsPath) -> Result<Self> {
        match t {
            MarkupType::Markdown => {
                let Document { metadata, content } = YamlFrontMatter::parse::<Meta>(content)
                    .map_err(|err| {
                        eyre!("Failed to parse yaml metadata for file: {}\n{}", path, err)
                    })?;
                // NOTE Meta line count isn't used for Markdown because lookup isn't
                // implemented for it.
                Ok(Self {
                    metadata,
                    content,
                    meta_line_count: 0,
                })
            }
            MarkupType::Djot => {
                let line_count_before = content.lines().count();
                let (metadata, content) =
                    toml_frontmatter::parse::<Meta>(content).map_err(|err| {
                        eyre!("Failed to parse toml metadata for file: {}\n{}", path, err)
                    })?;
                let line_count_after = content.lines().count();
                Ok(Self {
                    metadata,
                    content: content.to_string(),
                    meta_line_count: line_count_before - line_count_after,
                })
            }
        }
    }
}

#[derive(Debug)]
pub struct MarkupFile<Meta: DeserializeOwned> {
    pub markup: Markup,
    pub markup_lookup: Option<MarkupLookup>,
    pub html: Html,
    pub path: AbsPath,
    pub markup_meta: Meta,
}

pub fn find_markup_files<'a, B: AsRef<Utf8Path>, P: 'a + AsRef<Utf8Path>>(
    base: B,
    dirs: &[P],
) -> Vec<FilePath> {
    dirs.iter()
        .flat_map(|dir| {
            let base = base.as_ref();
            let dir = dir.as_ref();
            WalkDir::new(dir.as_std_path())
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| match e.metadata() {
                    Ok(e) => !e.is_dir(),
                    _ => false,
                })
                .filter_map(move |e| FilePath::from_std_path(base, e.into_path()).ok())
                .filter(|e| MarkupType::from_file(&e.rel_path.0).is_some())
        })
        .collect()
}

fn strip_one_paragraph(html: Cow<str>) -> Cow<str> {
    // Why do something insane like use regex to strip a paragraph?
    // I tried to use `scraper::Html` to parse it properly, but the attribute order
    // wasn't deterministic when parsing and rebuilding.
    // This jank seems to work fine, so why not?!
    lazy_static! {
        static ref PARAGRAPH: Regex = Regex::new(r"<p>(.+?)</p>").unwrap();
    }

    let paragraphs: Vec<_> = PARAGRAPH.captures_iter(&html).collect();

    match paragraphs.len() {
        1 => paragraphs[0].get(1).unwrap().as_str().to_owned().into(),
        _ => html,
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct ParseContext<'a> {
    pub path: Option<&'a Utf8Path>,
    pub is_draft: bool,
    pub in_feed: bool,
    pub create_lookup: bool,
    pub markup_meta_line_count: usize,
}

impl<'a> ParseContext<'a> {
    pub fn new(markup_meta_line_count: usize) -> Self {
        Self {
            markup_meta_line_count,
            ..Default::default()
        }
    }
    pub fn new_post_context(is_draft: bool, markup_meta_line_count: usize) -> Self {
        Self {
            is_draft,
            markup_meta_line_count,
            ..Default::default()
        }
    }

    pub fn is_draft(mut self, is_draft: bool) -> Self {
        self.is_draft = is_draft;
        self
    }

    pub fn with_path(mut self, path: &'a Utf8Path) -> Self {
        self.path = Some(path);
        self
    }

    pub fn in_feed(mut self, in_feed: bool) -> Self {
        self.in_feed = in_feed;
        self
    }

    pub fn log_broken_link(self, target: &str) {
        if self.in_feed {
            return;
        }

        let msg = format!("Broken reference to: `{target}` in {}", self.format_path());

        if self.is_draft {
            warn!("{}", msg);
        } else {
            error!("{}", msg);
        }
    }

    pub fn log_todo_comment(self, comment: &str) {
        warn!("{} in {}", comment, self.format_path())
    }

    fn format_path(self) -> String {
        match self.path {
            Some(path) => format!("`{}`", path),
            None => "unknown path".into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_one_paragraph() {
        assert_eq!(
            strip_one_paragraph("One thing".into()).as_ref(),
            r"One thing"
        );

        assert_eq!(
            strip_one_paragraph("<p>One</p>\n<p>Two</p>".into()).as_ref(),
            "<p>One</p>\n<p>Two</p>"
        );
    }
}
