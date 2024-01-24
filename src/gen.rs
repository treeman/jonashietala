use crate::{content::PostDirMetadata, markup, paths::AbsPath, util};
use camino::{Utf8Path, Utf8PathBuf};
use chrono::Utc;
use colored::Colorize;
use eyre::{eyre, Result};
use regex::Regex;
use serde::Deserialize;
use std::fs;
use tracing::info;
use yaml_front_matter::{Document, YamlFrontMatter};

pub fn new_post(title: String) -> Result<()> {
    let slug = util::slugify(&title);
    let path = post_path(&slug);
    new_prototype(&title, &path)
}

pub fn new_draft(title: String) -> Result<()> {
    let slug = util::slugify(&title);
    let path = draft_path(&slug);
    new_prototype(&title, &path)
}

fn post_path(slug: &str) -> Utf8PathBuf {
    let now = Utc::now();
    format!("posts/{}-{}.markdown", now.format("%Y-%m-%d"), slug).into()
}

fn draft_path(slug: &str) -> Utf8PathBuf {
    format!("drafts/{slug}.markdown").into()
}

fn new_prototype(title: &str, path: &Utf8Path) -> Result<()> {
    let content = prototype_post(title);
    fs::write(path, content)?;
    info!("Created {path}");
    Ok(())
}

fn prototype_post(title: &str) -> String {
    format!(
        r#"---
title: "{title}"
tags: [Tag1, Tag2]
---

Lorem ipsum...
"#
    )
}

pub fn promote(pattern: String) -> Result<()> {
    let draft_path = match_single_file_path(&pattern, "drafts/".into())?;
    let title = read_title(&draft_path)?;
    let slug = util::slugify(&title);
    let post_path = post_path(&slug);
    rename(&draft_path, &post_path, "Promoted")?;
    Ok(())
}

pub fn demote(pattern: String) -> Result<()> {
    let post_path = match_single_file_path(&pattern, "posts/".into())?;
    let PostDirMetadata { slug, .. } = PostDirMetadata::parse_post(&post_path)?;
    let draft_path = draft_path(&slug);
    rename(&post_path, &draft_path, "Demoted")?;
    Ok(())
}

fn rename(src: &Utf8Path, dest: &Utf8Path, notice: &str) -> Result<()> {
    if let Some(parent) = dest.parent() {
        fs::create_dir_all(parent)?;
    }
    println!(
        "{} {src} to {}",
        format!("[{notice}]").green(),
        dest.as_str().magenta()
    );
    fs::rename(src, dest)?;
    Ok(())
}

fn match_single_file_path(pattern: &str, dir: &Utf8Path) -> Result<AbsPath> {
    let found = match_file_path(pattern, dir)?;
    match found.len() {
        0 => {
            println!("{} no matches found", "[Error]".red());
            std::process::exit(1);
        }
        1 => return Ok(found.into_iter().next().unwrap()),
        x => {
            println!("{} {} matches found", "[Error]".red(), x);
            for path in found {
                let title = read_title(&path)?;
                println!(r#"{} "{}""#, path.as_str().magenta(), title.cyan());
            }
            std::process::exit(1);
        }
    }
}

fn match_file_path(pattern: &str, dir: &Utf8Path) -> Result<Vec<AbsPath>> {
    // Add in case insensitivity
    let re = Regex::new(&format!("(?i){pattern}"))?;
    let files = markup::find_markup_files(&[dir.as_str()]);

    let mut res = Vec::new();
    for file in files.into_iter() {
        let path = file.abs_path();
        if matches_file(&re, &path)? {
            res.push(path);
        }
    }
    Ok(res)
}

fn matches_file(re: &Regex, path: &Utf8Path) -> Result<bool> {
    if re.is_match(path.as_str()) {
        return Ok(true);
    }

    let title = read_title(path)?;
    if re.is_match(&title) {
        return Ok(true);
    }

    Ok(false)
}

fn read_title(path: &Utf8Path) -> Result<String> {
    let content = fs::read_to_string(&path)?;

    let Document {
        metadata,
        content: _,
    } = YamlFrontMatter::parse::<TitleMetadata>(&content)
        .map_err(|err| eyre!("Failed to parse metadata for : {:#?}\n{}", path, err))?;

    Ok(metadata.title)
}

#[derive(Deserialize, Debug)]
struct TitleMetadata {
    title: String,
}
