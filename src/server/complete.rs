use super::messages::CompletionItem;
use crate::paths::AbsPath;
use crate::site::Site;
use crate::site_url::SiteUrl;
use lazy_static::lazy_static;
use regex::Regex;
use serde_repr::*;

pub fn complete(
    cursor_before_line: &str,
    _col: usize,
    row: usize,
    path: &str,
    site: &Site,
) -> Vec<CompletionItem> {
    let lookup = match site.content.find_post_lookup(&path) {
        Some(x) => x,
        None => return vec![],
    };

    if lookup.in_frontmatter(row) {
        // -- Expand tags in frontmatter if line starts with `tags = `
        // if string.match(cursor_line, "^tags = ")
        if FRONTMATTER_TAG.is_match(cursor_before_line) {
            return tags_completions(site);
        }

        // -- Expand series in frontmatter if line starts with `series = `
        // if string.match(cursor_line, "^series = ")
        if FRONTMATTER_SERIES.is_match(cursor_before_line) {
            return series_completions(site);
        }
    } else {
        // -- Expand images separately because I only ever use it in a -- `![](/url)`
        // -- context and not mixing with other urls gives a more pleasant experience.
        // string.match(cursor_before_line, "!%[%]%([^%)]*$")
        // if IMG_LINK.is_match(cursor_before_line) {}

        // -- Expand inline links, e.g. `[txt](/`
        if INLINE_REL_LINK.is_match(cursor_before_line) {
            // FIXME should match a heading in the referenced post
            return url_completions(site);
        }

        // -- Expanding headings in inline links, e.g. `[txt](#`
        if INLINE_HEADER_REF.is_match(cursor_before_line) {
            return heading_completions(path, site);
        }

        // -- Expand links in link ref definitions, e.g. `[label]: /`
        if LINK_DEF_REL_LINK.is_match(cursor_before_line) {
            // FIXME should match a heading in the referenced post
            return url_completions(site);
        }

        // -- Expanding headings in ref defs, e.g. `[label]: #`
        if LINK_DEF_HEADER_REF.is_match(cursor_before_line) {
            return heading_completions(path, site);
        }

        // -- Expand url definition tags in `[text][tag]`
        // if string.match(cursor_before_line, "%[[^%]]+%]%[[^%]]*$")
        if FULL_LINK_TAG.is_match(cursor_before_line) {
            return link_tag_completions(path, site);
        }

        // Expand url definition tags in `[tag][]`, simplified to after a `[`
        // If first thing in a line, it could be a link def `[tag]: `
        // where we should complete broken link tags as well.
        if let Some(open_bracket) = OPEN_BRACKET.find(cursor_before_line) {
            let mut res = link_tag_completions(path, site);

            if open_bracket.start() == 0 {
                append_broken_link_completions(path, site, &mut res);
            }

            return res;
        }
    }

    vec![]
}

lazy_static! {
    static ref IMG_LINK: Regex = Regex::new(r"!\[\]\([^\)]*$").unwrap();
    static ref INLINE_REL_LINK: Regex = Regex::new(r"]\(/[^)]*$").unwrap();
    static ref INLINE_HEADER_REF: Regex = Regex::new(r"]\(#[^)]*$").unwrap();
    static ref LINK_DEF_REL_LINK: Regex = Regex::new(r"^\[.+\]:\s+/").unwrap();
    static ref LINK_DEF_HEADER_REF: Regex = Regex::new(r"^\[.+\]:\s+#").unwrap();
    static ref FULL_LINK_TAG: Regex = Regex::new(r"\[[^\]]+\]\[[^\]]*$").unwrap();
    static ref FRONTMATTER_TAG: Regex = Regex::new(r"^tags(:| =) ").unwrap();
    static ref FRONTMATTER_SERIES: Regex = Regex::new(r"^series(:| =) ").unwrap();
    static ref OPEN_BRACKET: Regex = Regex::new(r"\[[^\]]*$").unwrap();
    static ref OPEN_BRACKET_FIRST: Regex = Regex::new(r"^\[[^\]]*$").unwrap();
}

fn url_completions(site: &Site) -> Vec<CompletionItem> {
    let mut res = Vec::new();

    for item in site.content.posts.values() {
        res.push(
            CompletionItemBuilder::from_url(&item.title, &item.url, CompletionItemKind::File)
                .with_path(&item.path)
                .into(),
        );
    }

    for item in site.content.standalones.iter() {
        res.push(
            CompletionItemBuilder::from_url(&item.title, &item.url, CompletionItemKind::File)
                .with_path(&item.path)
                .into(),
        );
    }

    append_series(site, &mut res);
    append_tags(site, &mut res);

    res.push(
        CompletionItemBuilder::from_url(
            &site.content.projects.title,
            &site.content.projects.url,
            CompletionItemKind::Constant,
        )
        .into(),
    );

    res
}

fn heading_completions(path: &str, site: &Site) -> Vec<CompletionItem> {
    let lookup = match site.content.find_post_lookup(path) {
        Some(x) => x,
        None => return vec![],
    };

    lookup
        .headings
        .values()
        .map(|heading| {
            CompletionItemBuilder {
                // FIXME should display number of `#`?
                label: heading.content.clone(),
                insert_text: Some(heading.id.clone()),
                filter_text: Some(heading.content.clone()),
                kind: CompletionItemKind::Class,
                ..Default::default()
            }
            .into()
        })
        .collect()
}

fn link_tag_completions(path: &str, site: &Site) -> Vec<CompletionItem> {
    let lookup = match site.content.find_post_lookup(path) {
        Some(x) => x,
        None => return vec![],
    };

    lookup
        .link_defs
        .values()
        .map(|link| {
            CompletionItemBuilder {
                label: link.label.clone(),
                insert_text: Some(link.label.clone()),
                kind: CompletionItemKind::Reference,
                ..Default::default()
            }
            .add_filter(&link.url)
            .add_filter(&link.label)
            .into()
        })
        .collect()
}

fn append_broken_link_completions(path: &str, site: &Site, res: &mut Vec<CompletionItem>) {
    let lookup = match site.content.find_post_lookup(path) {
        Some(x) => x,
        None => return,
    };

    for link in lookup.broken_links.iter() {
        res.push(
            CompletionItemBuilder {
                label: link.tag.clone(),
                kind: CompletionItemKind::Field,
                ..Default::default()
            }
            .into(),
        );
    }
}

fn tags_completions(site: &Site) -> Vec<CompletionItem> {
    let mut res = Vec::new();
    append_tags(site, &mut res);
    res
}

fn append_tags(site: &Site, res: &mut Vec<CompletionItem>) {
    for item in site.lookup.tags.keys() {
        res.push(
            CompletionItemBuilder::from_url(&item.name, &item.url, CompletionItemKind::Folder)
                .with_label_tag("Tag")
                .into(),
        );
    }
}

fn series_completions(site: &Site) -> Vec<CompletionItem> {
    let mut res = Vec::new();
    append_series(site, &mut res);
    res
}

fn append_series(site: &Site, res: &mut Vec<CompletionItem>) {
    for item in site.content.series.values() {
        res.push(
            CompletionItemBuilder::from_url(&item.title, &item.url, CompletionItemKind::Module)
                .with_path(&item.path)
                .with_label_tag("Series")
                .into(),
        );
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

pub struct CompletionItemBuilder {
    pub label: String,
    pub kind: CompletionItemKind,
    pub label_tag: Option<String>,
    pub filter_text: Option<String>,
    pub insert_text: Option<String>,
    pub path: Option<AbsPath>,
}

impl CompletionItemBuilder {
    pub fn from_url(label: &str, url: &SiteUrl, kind: CompletionItemKind) -> Self {
        let href = url.href();
        Self {
            label: label.to_string(),
            insert_text: Some(href.to_string()),
            kind,
            ..Default::default()
        }
        .add_filter(href.as_ref())
        .add_filter(label)
    }

    pub fn with_label_tag(mut self, tag: &str) -> Self {
        self.label_tag = Some(tag.into());
        self
    }

    pub fn with_path(mut self, path: &AbsPath) -> Self {
        self.path = Some(path.to_owned());
        self
    }

    pub fn add_filter(mut self, txt: &str) -> Self {
        if let Some(mut s) = self.filter_text {
            s.push('|');
            s.push_str(txt);
            self.filter_text = Some(s);
        } else {
            self.filter_text = Some(txt.to_string());
        }
        self
    }
}

impl Default for CompletionItemBuilder {
    fn default() -> Self {
        CompletionItemBuilder {
            label: "".into(),
            kind: CompletionItemKind::Text,
            label_tag: None,
            filter_text: None,
            insert_text: None,
            path: None,
        }
    }
}

impl Into<CompletionItem> for CompletionItemBuilder {
    fn into(self) -> CompletionItem {
        let label = if let Some(tag) = self.label_tag {
            format!("{}: {}", tag, self.label)
        } else {
            self.label
        };

        CompletionItem {
            label,
            insert_text: self.insert_text,
            filter_text: self.filter_text,
            kind: self.kind,
            path: self.path.map(|path| path.to_string()),
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
                label: "Second level header".into(),
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
    fn test_frontmatter_completions() -> Result<()> {
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
