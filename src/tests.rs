#![cfg(test)]

use crate::content::PostItem;
use crate::content::SeriesItem;
use crate::paths::AbsPath;
use crate::site::{Site, SiteOptions};
use crate::site_url::{HrefUrl, ImgUrl};
use crate::tera::load_templates;
use crate::util;
use crate::util::{ParsedFile, ParsedFiles};
use camino::Utf8Path;
use camino::Utf8PathBuf;
use eyre::Result;
use hotwatch::notify::event::AccessKind;
use hotwatch::notify::event::AccessMode;
use hotwatch::notify::event::ModifyKind;
use hotwatch::notify::event::RemoveKind;
use hotwatch::notify::event::RenameMode;
use hotwatch::{Event, EventKind};
use lazy_static::lazy_static;
use regex::Regex;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;
use tera::Tera;
use thiserror::Error;

pub struct TestSite {
    pub site: Site,
    pub output_dir: TempDir,
    pub input_dir: TempDir,
}

impl TestSite {
    pub fn create_file(&mut self, file: &str, content: &str) -> Result<()> {
        let path = self.input_dir.path().join(file);
        util::write_to_file(&path, content)?;
        self.site.file_changed(
            Event::new(EventKind::Access(AccessKind::Close(AccessMode::Write))).add_path(path),
        )
    }

    pub fn create_test_file(&mut self, path: &str) -> Result<()> {
        self.create_file(
            path,
            r#"---toml
title = "New post title"
tags = ["Tag1"]
---

My created post
"#,
        )
    }

    pub fn change_file(&mut self, file: &str, from: &str, to: &str) -> Result<()> {
        let path = self.input_dir.path().join(file);
        let content = fs::read_to_string(&path)?.replace(from, to);
        util::write_to_file(&path, &content)?;
        self.site.file_changed(
            Event::new(EventKind::Access(AccessKind::Close(AccessMode::Write))).add_path(path),
        )
    }

    pub fn rename_file(&mut self, from: &str, to: &str) -> Result<()> {
        let from = self.input_dir.path().join(from);
        let to = self.input_dir.path().join(to);
        util::rename_file(&from, &to)?;
        self.site.file_changed(
            Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::Both)))
                .add_path(from)
                .add_path(to),
        )
    }

    pub fn find_post<'a>(&'a self, file_name: &str) -> Option<&'a PostItem> {
        self.site
            .content
            .posts
            .values()
            .find(|post| post.path.0.file_name() == Some(file_name))
    }

    pub fn find_series<'a>(&'a self, file_name: &str) -> Option<&'a SeriesItem> {
        self.site
            .content
            .series
            .values()
            .find(|series| series.path.0.file_name() == Some(file_name))
    }

    pub fn output_path(&self, file: &str) -> AbsPath {
        AbsPath::from_path_buf(self.output_dir.path().join(file))
    }

    pub fn input_path(&self, file: &str) -> AbsPath {
        AbsPath::from_path_buf(self.input_dir.path().join(file))
    }

    pub fn output_content(&self, file: &str) -> Result<String> {
        let path = self.output_path(file);
        let content = fs::read_to_string(&path)?;
        Ok(content)
    }

    pub fn read_file_to_string(&self, file: &str) -> std::io::Result<String> {
        fs::read_to_string(self.output_path(file))
    }

    pub fn remove_file(&mut self, file: &str) -> Result<()> {
        let path = self.input_dir.path().join(file);
        fs::remove_file(&path)?;
        self.site
            .file_changed(Event::new(EventKind::Remove(RemoveKind::Any)).add_path(path))
    }

    /// Persist the input and output dir, allowing us to inspect them
    /// after test has finished.
    #[allow(dead_code)]
    pub fn persist(self) -> (PathBuf, PathBuf) {
        let TestSite {
            output_dir,
            input_dir,
            ..
        } = self;
        (input_dir.into_path(), output_dir.into_path())
    }

    #[allow(dead_code)]
    pub fn skip_clean(self) {
        let (input_dir, output_dir) = self.persist();
        println!(
            "Skipping cleaning\n  input: {}\n  output: {}",
            input_dir.display(),
            output_dir.display()
        );
    }
}

pub struct TestSiteBuilder {
    pub include_drafts: bool,
    pub generate_markup_lookup: bool,
}

impl TestSiteBuilder {
    pub fn build(self) -> Result<TestSite> {
        let (output_dir, output_path) = AbsPath::new_tempdir()?;
        let (input_dir, input_path) = AbsPath::new_tempdir()?;

        fs_extra::dir::copy(
            "test-site",
            &input_path,
            &fs_extra::dir::CopyOptions {
                content_only: true,
                ..Default::default()
            },
        )?;

        let site = Site::load_content(SiteOptions {
            output_dir: output_path,
            input_dir: input_path,
            clear_output_dir: true,
            include_drafts: self.include_drafts,
            generate_feed: true,
            include_js: false,
            generate_markup_lookup: self.generate_markup_lookup,
            git_path_offset: Some(Utf8Path::new("test-site/")),
        })?;
        site.render_all()?;

        Ok(TestSite {
            site,
            output_dir,
            input_dir,
        })
    }
}

pub fn templates() -> &'static Tera {
    lazy_static! {
        static ref TEMPLATES: Tera = load_templates("templates/*.html").unwrap();
    }
    &TEMPLATES
}

#[derive(Error, Debug)]
pub enum GeneratedFileError<'a> {
    #[error("missing doctype")]
    MissingDocType,
    #[error("broken link `{0}`")]
    BrokenLink(&'a str),
    #[error("url not found `{0}`")]
    UrlNotFound(String),
    #[error("img not found `{0}`")]
    LocalImgNotFound(String),
    #[error("fragment not found `{0}`")]
    LocalFragmentNotFound(&'a str),
    #[error("fragment `{0}` not found in `{1}`")]
    OtherFragmentNotFound(String, Utf8PathBuf),
}

pub fn check_file<'a>(
    file: &'a ParsedFile,
    files: &'a ParsedFiles,
    output_dir: &Utf8Path,
) -> Vec<GeneratedFileError<'a>> {
    lazy_static! {
        static ref BROKEN_LINK: Regex = Regex::new(r"\[[^[\]]]+]\[[^[\]]]*]").unwrap();
    }
    let mut errors = Vec::new();
    if !file.content.starts_with("<!DOCTYPE html>") {
        errors.push(GeneratedFileError::MissingDocType);
    }

    for bad_link in BROKEN_LINK.find_iter(&file.content) {
        // FIXME these gives false positives when they're inside a code block.
        // Maybe find start/end of all code blocks, and then only add them if they're outside?
        // For now just ignore the offending file.
        if !file
            .path
            .ends_with("rewriting_my_blog_in_rust_for_fun_and_profit/index.html")
        {
            errors.push(GeneratedFileError::BrokenLink(bad_link.as_str()));
        }
    }

    let mut links: Vec<&HrefUrl> = file.links.iter().collect();
    links.sort();

    for link in links {
        match link {
            HrefUrl::Internal(ref internal) => {
                let output_file = internal.output_file(output_dir);

                // Just skip image links for now, handle errors in img check below.
                // It's not -exactly- the same, but it's good enough for me as I don't
                // add image links manually.
                if internal.is_img() {
                    continue;
                }
                // We skip feed generation because it's slow. This is fine.
                if internal.is_feed() {
                    continue;
                }

                let external_ref = match files.get(&output_file) {
                    Some(file) => file,
                    None => {
                        errors.push(GeneratedFileError::UrlNotFound(internal.href().to_string()));
                        continue;
                    }
                };

                if let Some(fragment) = internal.fragment() {
                    let fragment = format!("#{fragment}");
                    if !external_ref.fragments.contains(&fragment) {
                        errors.push(GeneratedFileError::OtherFragmentNotFound(
                            fragment,
                            external_ref.path.clone(),
                        ));
                    }
                }
            }
            HrefUrl::Fragment(ref fragment) => {
                if !file.fragments.contains(fragment) {
                    errors.push(GeneratedFileError::LocalFragmentNotFound(fragment));
                }
            }
            HrefUrl::External(_) => {}
        }
    }

    let mut imgs: Vec<&ImgUrl> = file.imgs.iter().collect();
    imgs.sort();

    for img in imgs {
        match img {
            ImgUrl::Internal(ref internal) => {
                let output_file = internal.output_file(output_dir);
                if !output_file.exists() {
                    errors.push(GeneratedFileError::LocalImgNotFound(
                        internal.href().to_string(),
                    ));
                }
            }
            ImgUrl::External(_) => {}
        }
    }

    errors
}
