use camino::{Utf8Path, Utf8PathBuf};
use chrono::{DateTime, NaiveDateTime};
use eyre::eyre;
use eyre::Result;
use glob::glob;
use lazy_static::lazy_static;
use regex::Regex;
use scraper::{Html, Selector};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::{self, File};
use std::path::Path;
use std::time::UNIX_EPOCH;
use tracing::debug;

use crate::site_url::{HrefUrl, ImgUrl};

pub fn last_modified<P>(path: P) -> Result<NaiveDateTime>
where
    P: AsRef<Path>,
{
    let modified = fs::metadata(path)?.modified()?;
    Ok(DateTime::from_timestamp(
        modified.duration_since(UNIX_EPOCH)?.as_secs().try_into()?,
        0,
    )
    .unwrap()
    .naive_local())
}

pub fn create_file<P>(path: P) -> Result<File>
where
    P: AsRef<Path>,
{
    let dir = path.as_ref().parent().expect("Should have a parent dir");
    fs::create_dir_all(dir)?;
    let file = File::create(path)?;
    Ok(file)
}

pub fn write_to_file<P>(file: P, content: &str) -> Result<()>
where
    P: AsRef<Path>,
{
    debug!("Writing {:?}", file.as_ref());
    let dir = file.as_ref().parent().expect("Should have a parent dir");
    fs::create_dir_all(dir)?;
    fs::write(file, content)?;
    Ok(())
}

#[cfg(test)]
pub fn rename_file<P>(from: P, to: P) -> Result<()>
where
    P: AsRef<Path>,
{
    debug!("Renaming {:?} {:?}", from.as_ref(), to.as_ref());
    let dir = to.as_ref().parent().expect("Should have a parent dir");
    fs::create_dir_all(dir)?;
    fs::rename(&from, &to)?;
    Ok(())
}

pub fn copy_file<P>(from: P, to: P) -> Result<()>
where
    P: AsRef<Path>,
{
    debug!("Copying {:?} {:?}", from.as_ref(), to.as_ref());
    let dir = to.as_ref().parent().expect("Should have a parent dir");
    fs::create_dir_all(dir)?;
    fs::copy(from, to)?;
    Ok(())
}

/// Copy found files to a target dir, joining the existing directories
pub fn copy_files_keep_dirs(pattern: &str, base: &Utf8Path, target_dir: &Utf8Path) -> Result<u32> {
    let mut count = 0;
    for path in glob(base.join(pattern).as_str()).unwrap().flatten() {
        let path = Utf8Path::from_path(&path).expect("Non-utf8 path");
        if !path.is_file() {
            continue;
        }
        let rel_path = path.strip_prefix(base)?;
        copy_file(path, &target_dir.join(rel_path))?;
        count += 1;
    }
    Ok(count)
}

/// Copy found files to a target dir, discarding the file structure.
pub fn copy_files_to(pattern: &str, target_dir: &Utf8Path) -> Result<u32> {
    let mut count = 0;
    for path in glob(pattern).unwrap().flatten() {
        let path = Utf8Path::from_path(&path).expect("Non-utf8 path");
        if !path.is_file() {
            continue;
        }
        copy_file(path, &target_dir.join(path.file_name().unwrap()))?;
        count += 1;
    }
    Ok(count)
}

// Yes, this could be made faster... But meh. This should be fast enough
// and it's more readable than a handrolled loop and it's easy to add extra rules.
lazy_static! {
    static ref UND_RE: Regex = Regex::new(r"\s+|_+").unwrap();
    static ref DASH_RE: Regex = Regex::new(r"\s+|-+").unwrap();
    static ref SYM_RE: Regex = Regex::new(r"[^\sa-zA-Z0-9_-]+").unwrap();
}

pub fn to_id(s: &str) -> String {
    let s = s.trim();
    let s = SYM_RE.replace_all(s, "");
    let s = DASH_RE.replace_all(&s, "-");
    let s = s.trim_start_matches('_');
    let s = s.trim_start_matches('-');
    let s = s.trim_end_matches('_');
    let s = s.trim_end_matches('-');
    s.to_lowercase()
}

pub fn slugify(s: &str) -> String {
    let s = s.trim();
    let s = SYM_RE.replace_all(s, "");
    let s = UND_RE.replace_all(&s, "_");
    let s = s.trim_start_matches('_');
    let s = s.trim_start_matches('-');
    let s = s.trim_end_matches('_');
    let s = s.trim_end_matches('-');
    s.to_lowercase()
}

pub fn html_text(s: &str) -> String {
    Html::parse_fragment(s)
        .root_element()
        .text()
        .fold(String::new(), |mut acc, txt| {
            acc.push_str(txt);
            acc
        })
}

#[allow(dead_code)]
pub struct ParsedFile {
    pub path: Utf8PathBuf,
    pub html: Html,
    pub content: String,
    pub links: HashSet<HrefUrl>,
    pub imgs: HashSet<ImgUrl>,
    pub fragments: HashSet<String>,
}

pub type ParsedFiles = HashMap<Utf8PathBuf, ParsedFile>;

pub fn parse_html_files(output_dir: &Utf8Path) -> Result<ParsedFiles> {
    glob(&format!("{}/**/*.html", output_dir))
        .unwrap()
        .flatten()
        .map(|path| {
            let content = fs::read_to_string(&path)?;
            let html = Html::parse_document(&content);
            let path = Utf8PathBuf::from_path_buf(path).unwrap();

            let links = collect_links(&html)
                .map_err(|err| eyre!("Error parsing file `{}`:\n  {}", path, err))?;
            let imgs = collect_imgs(&html)
                .map_err(|err| eyre!("Error parsing file `{}`:\n  {}", path, err))?;
            let fragments = collect_fragments(&html)
                .map_err(|err| eyre!("Error parsing file `{}`:\n  {}", path, err))?;

            Ok((
                path.clone(),
                ParsedFile {
                    content,
                    html,
                    path,
                    links,
                    imgs,
                    fragments,
                },
            ))
        })
        .collect()
}

pub fn collect_links(document: &Html) -> Result<HashSet<HrefUrl>> {
    let selector = Selector::parse("a[href]").unwrap();
    let mut hrefs = HashSet::new();
    for element in document.select(&selector) {
        if let Some(href) = element.value().attr("href") {
            let parsed = HrefUrl::parse(href).map_err(|err| {
                eyre!(
                    "Error in parsing href in element: {:#?}\n  {}",
                    element.value(),
                    err
                )
            })?;
            hrefs.insert(parsed);
        }
    }
    Ok(hrefs)
}

pub fn collect_imgs(document: &Html) -> Result<HashSet<ImgUrl>> {
    let selector = Selector::parse("img[src]").unwrap();
    let mut imgs = HashSet::new();
    for element in document.select(&selector) {
        if let Some(src) = element.value().attr("src") {
            let parsed = ImgUrl::parse(src).map_err(|err| {
                eyre!(
                    "Error in parsing src in img: {:#?}\n  {}",
                    element.value(),
                    err
                )
            })?;
            imgs.insert(parsed);
        }
    }
    Ok(imgs)
}

pub fn collect_fragments(document: &Html) -> Result<HashSet<String>> {
    let selector = Selector::parse("[id]").unwrap();
    let mut fragments = HashSet::new();
    for element in document.select(&selector) {
        if let Some(id) = element.value().attr("id") {
            fragments.insert(format!("#{id}"));
        }
    }
    Ok(fragments)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Datelike;

    #[test]
    fn test_last_modified() {
        assert!(last_modified(Utf8Path::new("non_existant")).is_err());
        let modified = last_modified(Utf8Path::new("README.md")).unwrap();
        assert!(modified.year() >= 2020);
    }

    #[test]
    fn test_to_id() {
        assert_eq!(to_id("One Two"), "one-two");
        assert_eq!(to_id("1-2_3?4#5(6) 7!8&9"), "1-2_3456-789");
        assert_eq!(to_id("Mods & Symbols"), "mods-symbols");
        assert_eq!(to_id("()one---two???"), "one-two");
        assert_eq!(to_id("-trimmed--"), "trimmed");
        assert_eq!(to_id("_trimmed__"), "trimmed");
    }

    #[test]
    fn test_slugify() {
        assert_eq!(slugify("One Two"), "one_two");
        assert_eq!(slugify("1-2_3?4#5(6) 7!8&9"), "1-2_3456_789");
        assert_eq!(slugify("Mods & Symbols"), "mods_symbols");
        assert_eq!(slugify("()one___two???"), "one_two");
        assert_eq!(slugify("-trimmed--"), "trimmed");
        assert_eq!(slugify("_trimmed__"), "trimmed");
    }
}
