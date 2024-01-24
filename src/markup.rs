use camino::Utf8Path;
use eyre::{eyre, Result};
use serde::de::DeserializeOwned;
use std::fs;
use walkdir::WalkDir;
use yaml_front_matter::{Document, YamlFrontMatter};

use crate::markdown::markdown_to_html;
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

#[derive(Debug, Clone)]
pub struct TransformedMarkup(pub String);

#[derive(Debug)]
pub struct Markup<Meta: DeserializeOwned> {
    pub t: MarkupType,
    pub content: TransformedMarkup,
    pub raw_content: String,
    pub path: AbsPath,
    pub metadata: Meta,
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

        let content = match self.t {
            MarkupType::Markdown => markdown_to_html(&content),
            MarkupType::Djot => content.clone(),
        };

        Ok(Markup {
            t: self.t,
            path: self.path,
            content: TransformedMarkup(content),
            raw_content: self.content,
            metadata,
        })
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
