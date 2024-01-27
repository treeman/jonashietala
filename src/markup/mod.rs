mod djot;
mod markdown;
mod syntax_highlight;

use camino::Utf8Path;
use eyre::{eyre, Result};
use lazy_static::lazy_static;
use regex::Regex;
use serde::de::DeserializeOwned;
use std::borrow::Cow;
use std::fs;
use std::ops::Deref;
use walkdir::WalkDir;
use yaml_front_matter::{Document, YamlFrontMatter};

pub use self::syntax_highlight::{dump_syntax_binary, dump_theme};

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
        match file.extension() {
            Some("markdown" | "md") => Some(MarkupType::Markdown),
            Some("dj") => Some(MarkupType::Djot),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct RawMarkup {
    pub t: MarkupType,
    pub content: String,
    pub path: AbsPath,
}

impl RawMarkup {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let t = if let Some(t) = MarkupType::from_file(&path) {
            t
        } else {
            return Err(eyre!("Unsupported file format: `{}`", &path));
        };

        let content = fs::read_to_string(&path)?;

        Ok(RawMarkup { t, path, content })
    }

    pub fn transform<Meta: DeserializeOwned>(self: Self) -> Result<Markup<Meta>> {
        let Document { metadata, content } = YamlFrontMatter::parse::<Meta>(&self.content)
            .map_err(|err| eyre!("Failed to parse metadata for file: {}\n{}", self.path, err))?;

        let content = TransformedMarkup::parse(&content, self.t)?;

        Ok(Markup {
            t: self.t,
            path: self.path,
            content,
            raw_content: self.content,
            metadata,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TransformedMarkup(pub String);

impl TransformedMarkup {
    pub fn parse(s: &str, t: MarkupType) -> Result<Self> {
        let res = match t {
            MarkupType::Markdown => markdown_to_html(&s),
            MarkupType::Djot => djot_to_html(&s)?,
        };
        Ok(TransformedMarkup(res))
    }

    pub fn strip_one_paragraph(self) -> TransformedMarkup {
        TransformedMarkup(strip_one_paragraph(self.0.into()).into())
    }
}

impl Deref for TransformedMarkup {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct TransformedFeedMarkup(pub String);

impl TransformedFeedMarkup {
    pub fn parse(s: &str, t: MarkupType) -> Result<Self> {
        let res = match t {
            MarkupType::Markdown => markdown_to_html_feed(&s),
            MarkupType::Djot => djot_to_html_feed(&s)?,
        };
        Ok(TransformedFeedMarkup(res))
    }
}

#[derive(Debug)]
pub struct Markup<Meta: DeserializeOwned> {
    pub t: MarkupType,
    pub content: TransformedMarkup,
    pub raw_content: String,
    pub path: AbsPath,
    pub metadata: Meta,
}

impl<Meta: DeserializeOwned> Markup<Meta> {
    pub fn transform_feed(&self) -> Result<TransformedFeedMarkup> {
        TransformedFeedMarkup::parse(self.raw_content.as_str(), self.t)
    }
}

pub fn find_markup_files<'a, P: 'a + AsRef<Utf8Path>>(dirs: &[P]) -> Vec<FilePath> {
    dirs.iter()
        .map(|dir| {
            let dir = dir.as_ref();
            WalkDir::new(dir.as_std_path())
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| match e.metadata() {
                    Ok(e) => !e.is_dir(),
                    _ => false,
                })
                .filter_map(move |e| FilePath::from_std_path(dir, e.into_path()).ok())
                .filter(|e| MarkupType::from_file(&e.rel_path.0).is_some())
        })
        .flatten()
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
        _ => html.into(),
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
