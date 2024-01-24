use eyre::{eyre, Result};
use serde::de::DeserializeOwned;
use std::fs;
use yaml_front_matter::{Document, YamlFrontMatter};

use crate::markdown::markdown_to_html;
use crate::paths::AbsPath;

#[derive(Debug, Copy, Clone)]
pub enum MarkupType {
    Markdown,
    Djot,
}

#[derive(Debug)]
pub struct RawMarkup {
    pub t: MarkupType,
    pub content: String,
    pub path: AbsPath,
}

#[derive(Debug)]
pub struct Markup<Meta: DeserializeOwned> {
    pub t: MarkupType,
    pub content: String,
    pub raw_content: String,
    pub path: AbsPath,
    pub metadata: Meta,
}

impl RawMarkup {
    pub fn from_file(path: AbsPath) -> Result<Self> {
        let t = match path.extension() {
            Some("markdown" | "md") => MarkupType::Markdown,
            Some("dj") => MarkupType::Djot,
            _ => return Err(eyre!("Unsupported file format: `{}`", &path)),
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
            content,
            raw_content: self.content,
            metadata,
        })
    }
}
