use super::messages::{
    BrokenLinkInfo, CompletionItem, ImgInfo, LinkDefInfo, PostInfo, SeriesInfo, StandaloneInfo,
    TagInfo,
};
use super::messages::{ExtraCompletionInfo, HeadingInfo};
use crate::content::SeriesItem;
use crate::content::StandaloneItem;
use crate::content::Tag;
use crate::content::{PostItem, PostRef};
use crate::paths::RelPath;
use crate::site::Site;
use crate::{markup::MarkupLookup, paths};
use camino::Utf8PathBuf;
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use serde_repr::*;

pub fn complete(
    cursor_before_line: &str,
    _col: usize,
    row: usize,
    path: &str,
    site: &Site,
) -> Vec<CompletionItem> {
    let lookup = match site.content.find_post_lookup_by_file_name(&path) {
        Some(x) => x,
        None => return vec![],
    };

    if lookup.in_frontmatter(row) {
        // Expand tags in frontmatter if line starts with `tags = `
        // if string.match(cursor_line, "^tags = ")
        if FRONTMATTER_TAG.is_match(cursor_before_line) {
            return tags_completions(site);
        }

        // Expand series in frontmatter if line starts with `series = `
        // if string.match(cursor_line, "^series = ")
        if FRONTMATTER_SERIES.is_match(cursor_before_line) {
            return series_completions(site);
        }
    } else {
        // Expand images separately because I only ever use it in a -- `![](/url)`
        // context and not mixing with other urls gives a more pleasant experience.
        // string.match(cursor_before_line, "!%[%]%([^%)]*$")
        if IMG_LINK.is_match(cursor_before_line) {
            return img_completions(site);
        }

        // Expand inline links, e.g. `[txt](/`
        // if INLINE_REL_LINK.is_match(cursor_before_line) {
        if let Some(caps) = INLINE_REL_LINK.captures(cursor_before_line) {
            if let Some(res) = split_heading_completions(caps, site) {
                return res;
            }
            return url_completions(site);
        }

        // Expanding headings in inline links, e.g. `[txt](#`
        if INLINE_HEADER_REF.is_match(cursor_before_line) {
            return heading_completions(lookup, HeadingSource::SameFile);
        }

        // Expand links in link ref definitions, e.g. `[label]: /`
        if let Some(caps) = LINK_DEF_REL_LINK.captures(cursor_before_line) {
            if let Some(res) = split_heading_completions(caps, site) {
                return res;
            }
            return url_completions(site);
        }

        // Expanding headings in ref defs, e.g. `[label]: #`
        if LINK_DEF_HEADER_REF.is_match(cursor_before_line) {
            return heading_completions(lookup, HeadingSource::SameFile);
        }

        // Expand url definition tags in `[text][tag]`
        // if string.match(cursor_before_line, "%[[^%]]+%]%[[^%]]*$")
        if FULL_LINK_TAG.is_match(cursor_before_line) {
            return link_tag_completions(lookup);
        }

        // Expand url definition tags in `[tag][]`, simplified to after a `[`
        // If first thing in a line, it could be a link def `[tag]: `
        // where we should complete broken link tags as well.
        if let Some(open_bracket) = OPEN_BRACKET.find(cursor_before_line) {
            let mut res = link_tag_completions(lookup);

            if open_bracket.start() == 0 {
                append_broken_link_completions(lookup, &mut res);
            }

            return res;
        }
    }

    vec![]
}

lazy_static! {
    static ref IMG_LINK: Regex = Regex::new(r"!\[\]\([^)]*$").unwrap();
    static ref INLINE_REL_LINK: Regex = Regex::new(r"]\((/[^)]*)$").unwrap();
    static ref INLINE_HEADER_REF: Regex = Regex::new(r"]\(#[^)]*$").unwrap();
    static ref LINK_DEF_REL_LINK: Regex = Regex::new(r"^\[.+\]:\s+(/.*)$").unwrap();
    static ref LINK_DEF_HEADER_REF: Regex = Regex::new(r"^\[.+\]:\s+#").unwrap();
    static ref FULL_LINK_TAG: Regex = Regex::new(r"\[[^\]]+\]\[[^\]]*$").unwrap();
    static ref FRONTMATTER_TAG: Regex = Regex::new(r"^tags(:| =) ").unwrap();
    static ref FRONTMATTER_SERIES: Regex = Regex::new(r"^series(:| =) ").unwrap();
    static ref OPEN_BRACKET: Regex = Regex::new(r"\[[^\]]*$").unwrap();
    static ref OPEN_BRACKET_FIRST: Regex = Regex::new(r"^\[[^\]]*$").unwrap();
}

fn img_completions(site: &Site) -> Vec<CompletionItem> {
    paths::list_files(&site.opts.input_dir.join("images"))
        .into_iter()
        .map(|img| CompletionItemBuilder::new_img(img.rel_path).into())
        .collect()
}

fn url_completions(site: &Site) -> Vec<CompletionItem> {
    let mut res = Vec::new();

    for item in site.content.posts.values() {
        if !item.is_draft {
            res.push(CompletionItemBuilder::new_post(item).into());
        }
    }

    for item in site.content.standalones.iter() {
        res.push(CompletionItemBuilder::new_standalone(item).into());
    }

    append_series(site, &mut res);
    append_tags(site, &mut res);

    // res.push(
    //     CompletionItemBuilder::from_url(
    //         &site.content.projects.title,
    //         &site.content.projects.url,
    //         CompletionItemKind::Constant,
    //     )
    //     .into(),
    // );

    res
}

enum HeadingSource<'a> {
    Path(&'a str),
    SameFile,
}

fn heading_completions(lookup: &MarkupLookup, _source: HeadingSource<'_>) -> Vec<CompletionItem> {
    lookup
        .headings
        .values()
        .map(|heading| CompletionItemBuilder::Heading(heading.into()).into())
        .collect()
}

fn split_heading_completions(caps: Captures<'_>, site: &Site) -> Option<Vec<CompletionItem>> {
    if let Some((url, _head)) = caps[1].split_once('#') {
        if let Some(lookup) = site.content.find_post_lookup_by_url(url) {
            return Some(heading_completions(lookup, HeadingSource::Path(url)));
        }
    }
    None
}

fn link_tag_completions(lookup: &MarkupLookup) -> Vec<CompletionItem> {
    lookup
        .link_defs
        .values()
        .map(|link| CompletionItemBuilder::LinkDefInfo(link.into()).into())
        .collect()
}

fn append_broken_link_completions(lookup: &MarkupLookup, res: &mut Vec<CompletionItem>) {
    for link in lookup.broken_links.iter() {
        res.push(CompletionItemBuilder::BrokenLink(link.into()).into());
    }
}

fn tags_completions(site: &Site) -> Vec<CompletionItem> {
    let mut res = Vec::new();
    append_tags(site, &mut res);
    res
}

fn append_tags(site: &Site, res: &mut Vec<CompletionItem>) {
    for (tag, posts) in site.lookup.tags.iter() {
        res.push(CompletionItemBuilder::new_tag(tag, posts, site).into());
    }
}

fn series_completions(site: &Site) -> Vec<CompletionItem> {
    let mut res = Vec::new();
    append_series(site, &mut res);
    res
}

fn append_series(site: &Site, res: &mut Vec<CompletionItem>) {
    for item in site.content.series.values() {
        res.push(CompletionItemBuilder::new_series(item, site).into());
    }
}

#[allow(dead_code)]
#[derive(Debug, Serialize_repr, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CompletionItemKind {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25,
}

impl Default for CompletionItemKind {
    fn default() -> Self {
        Self::Text
    }
}

// FIXME
// 3. Rows context
pub enum CompletionItemBuilder {
    Post(PostInfo),
    Standalone(StandaloneInfo),
    Img(ImgInfo),
    Series(SeriesInfo),
    Tag(TagInfo),
    BrokenLink(BrokenLinkInfo),
    Heading(HeadingInfo),
    LinkDefInfo(LinkDefInfo),
}

impl CompletionItemBuilder {
    pub fn new_img(path: RelPath) -> Self {
        Self::Img(ImgInfo {
            url: Utf8PathBuf::from("/images/").join(path.0).to_string(),
        })
    }

    pub fn new_post(item: &PostItem) -> Self {
        Self::Post(item.into())
    }

    pub fn new_standalone(item: &StandaloneItem) -> Self {
        Self::Standalone(item.into())
    }

    pub fn new_series(item: &SeriesItem, site: &Site) -> Self {
        Self::Series(SeriesInfo::from(item, site))
    }

    pub fn new_tag(tag: &Tag, posts: &[PostRef], site: &Site) -> Self {
        Self::Tag(TagInfo::from_tag(tag, posts, site))
    }
}

impl Into<CompletionItem> for CompletionItemBuilder {
    fn into(self) -> CompletionItem {
        match self {
            CompletionItemBuilder::Img(info) => CompletionItem {
                label: info.url.clone(),
                kind: CompletionItemKind::File,
                info: Some(ExtraCompletionInfo::Img(info)),
                ..Default::default()
            },
            CompletionItemBuilder::Post(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.title.as_str()].join("|")),
                label: info.title.clone(),
                insert_text: Some(info.url.clone()),
                kind: CompletionItemKind::File,
                info: Some(ExtraCompletionInfo::Post(info)),
                ..Default::default()
            },
            CompletionItemBuilder::Standalone(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.title.as_str()].join("|")),
                label: info.title.clone(),
                insert_text: Some(info.url.clone()),
                kind: CompletionItemKind::File,
                info: Some(ExtraCompletionInfo::Standalone(info)),
                ..Default::default()
            },
            CompletionItemBuilder::Series(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.title.as_str()].join("|")),
                label: format!("Series: {}", info.title),
                insert_text: Some(info.url.clone()),
                kind: CompletionItemKind::Module,
                info: Some(ExtraCompletionInfo::Series(info)),
                ..Default::default()
            },
            CompletionItemBuilder::Tag(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.name.as_str()].join("|")),
                label: format!("Tagged: {}", info.name),
                insert_text: Some(info.url.clone()),
                kind: CompletionItemKind::Folder,
                info: Some(ExtraCompletionInfo::Tag(info)),
                ..Default::default()
            },
            CompletionItemBuilder::Heading(info) => CompletionItem {
                label: format!("{} {}", "#".repeat(info.level.into()), info.content),
                filter_text: Some(info.content.clone()),
                insert_text: Some(info.id.clone()),
                kind: CompletionItemKind::Class,
                info: Some(ExtraCompletionInfo::Heading(info)),
                ..Default::default()
            },
            CompletionItemBuilder::LinkDefInfo(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.label.as_str()].join("|")),
                label: info.label.clone(),
                insert_text: Some(info.label.clone()),
                kind: CompletionItemKind::Reference,
                info: Some(ExtraCompletionInfo::LinkDef(info)),
                ..Default::default()
            },
            CompletionItemBuilder::BrokenLink(info) => CompletionItem {
                label: info.tag.clone(),
                kind: CompletionItemKind::Field,
                info: Some(ExtraCompletionInfo::BrokenLink(info)),
                ..Default::default()
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    use eyre::Result;

    fn find_insert_text<'a>(text: &str, items: &'a [CompletionItem]) -> Option<&'a CompletionItem> {
        items
            .iter()
            .find(|item| item.insert_text.as_deref() == Some(text))
    }

    fn find_label<'a>(text: &str, items: &'a [CompletionItem]) -> Option<&'a CompletionItem> {
        items.iter().find(|item| item.label == text)
    }

    #[test]
    fn test_rel_link_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let items = complete(
            "](/",
            0,
            6,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );

        assert_eq!(
            find_insert_text("/blog/2022/02/01/feb_post", &items),
            Some(&CompletionItem {
                label: "Feb post 1".into(),
                insert_text: Some("/blog/2022/02/01/feb_post".into()),
                filter_text: Some("/blog/2022/02/01/feb_post|Feb post 1".into()),
                kind: CompletionItemKind::File,
                path: Some(
                    test_site
                        .input_path("posts/2022-02-01-feb_post.dj")
                        .to_string()
                )
            })
        );
        assert_eq!(
            find_insert_text("/series/myseries", &items),
            Some(&CompletionItem {
                label: "Series: My series".into(),
                insert_text: Some("/series/myseries".into()),
                filter_text: Some("/series/myseries|My series".into()),
                kind: CompletionItemKind::Module,
                path: Some(test_site.input_path("series/myseries.markdown").to_string())
            })
        );
        assert_eq!(
            find_insert_text("/blog/tags/one", &items),
            Some(&CompletionItem {
                label: "Tag: One".into(),
                insert_text: Some("/blog/tags/one".into()),
                filter_text: Some("/blog/tags/one|One".into()),
                kind: CompletionItemKind::Folder,
                path: None
            })
        );
        assert_eq!(
            find_insert_text("/404", &items),
            Some(&CompletionItem {
                label: "404".into(),
                insert_text: Some("/404".into()),
                filter_text: Some("/404|404".into()),
                kind: CompletionItemKind::File,
                path: Some(test_site.input_path("static/404.markdown").to_string())
            })
        );
        assert_eq!(
            find_insert_text("/projects", &items),
            Some(&CompletionItem {
                label: "Projects".into(),
                insert_text: Some("/projects".into()),
                filter_text: Some("/projects|Projects".into()),
                kind: CompletionItemKind::Constant,
                path: None
            })
        );

        let def_items = complete(
            "[tag]: /",
            0,
            6,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );
        assert_eq!(items, def_items);

        let heading_items = complete(
            "](/blog/2022/02/01/feb_post#",
            0,
            6,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );

        assert_eq!(heading_items.iter().count(), 1);

        assert_eq!(
            find_insert_text("heading-with-text", &heading_items),
            Some(&CompletionItem {
                label: "# heading with text".into(),
                insert_text: Some("heading-with-text".into()),
                filter_text: Some("heading with text".into()),
                kind: CompletionItemKind::Class,
                path: None
            })
        );

        let def_heading_items = complete(
            "[tag]: /blog/2022/02/01/feb_post#",
            0,
            6,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );
        assert_eq!(heading_items, def_heading_items);

        Ok(())
    }

    #[test]
    fn test_header_ref_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let items = complete(
            "](#",
            0,
            6,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );

        assert_eq!(items.iter().count(), 2);

        assert_eq!(
            find_insert_text("Second-level-header", &items),
            Some(&CompletionItem {
                label: "## Second level header".into(),
                insert_text: Some("Second-level-header".into()),
                filter_text: Some("Second level header".into()),
                kind: CompletionItemKind::Class,
                path: None
            })
        );

        let def_items = complete(
            "[tag]: #",
            0,
            6,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );
        assert_eq!(items, def_items);

        Ok(())
    }

    #[test]
    fn test_full_tag_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let items = complete(
            "[some text][",
            0,
            6,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );

        assert_eq!(items.iter().count(), 1);

        assert_eq!(
            find_insert_text("tag1", &items),
            Some(&CompletionItem {
                label: "tag1".into(),
                insert_text: Some("tag1".into()),
                filter_text: Some("/uses|tag1".into()),
                kind: CompletionItemKind::Reference,
                path: None
            })
        );

        Ok(())
    }

    #[test]
    fn test_short_tag_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let items = complete(
            // Not the first in line, to only complete link def tags.
            "x [",
            0,
            6,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );

        assert_eq!(items.iter().count(), 1);

        assert_eq!(
            find_insert_text("tag1", &items),
            Some(&CompletionItem {
                label: "tag1".into(),
                insert_text: Some("tag1".into()),
                filter_text: Some("/uses|tag1".into()),
                kind: CompletionItemKind::Reference,
                path: None
            })
        );

        // First in line, so we should complete broken link tags as well.
        let items = complete("[", 0, 6, "posts/2022-01-31-test_post.dj", &test_site.site);

        assert_eq!(items.iter().count(), 2);

        assert_eq!(
            find_label("broken_tag", &items),
            Some(&CompletionItem {
                label: "broken_tag".into(),
                insert_text: None,
                filter_text: None,
                kind: CompletionItemKind::Field,
                path: None
            })
        );

        Ok(())
    }

    #[test]
    fn test_frontmatter_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let tags = complete(
            "tags = ",
            0,
            0,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );

        assert_eq!(tags.iter().count(), 3);

        assert_eq!(
            find_insert_text("/blog/tags/one", &tags),
            Some(&CompletionItem {
                label: "Tag: One".into(),
                insert_text: Some("/blog/tags/one".into()),
                filter_text: Some("/blog/tags/one|One".into()),
                kind: CompletionItemKind::Folder,
                path: None
            })
        );

        let series = complete(
            "series = ",
            0,
            0,
            "posts/2022-01-31-test_post.dj",
            &test_site.site,
        );

        assert_eq!(series.iter().count(), 1);

        assert_eq!(
            find_insert_text("/series/myseries", &series),
            Some(&CompletionItem {
                label: "Series: My series".into(),
                insert_text: Some("/series/myseries".into()),
                filter_text: Some("/series/myseries|My series".into()),
                kind: CompletionItemKind::Module,
                path: Some(test_site.input_path("series/myseries.markdown").to_string())
            })
        );

        Ok(())
    }
}
