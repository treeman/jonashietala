use crate::content::SeriesItem;
use crate::content::StandaloneItem;
use crate::content::Tag;
use crate::content::{
    BrokenLinkInfo, ConstantInfo, ContentInfo, DivClassInfo, HeadingContext, HeadingInfo, ImgInfo,
    LinkDefInfo, PostInfo, SeriesInfo, StandaloneInfo, SymbolInfo, TagInfo,
};
use crate::content::{PostItem, PostRef};
use crate::markup::markup_lookup::{Element, Img, ImgRef, Link, LinkRef};
use crate::markup::MarkupLookup;
use crate::paths::AbsPath;
use crate::paths::RelPath;
use crate::site::Site;
use crate::site_url::SiteUrl;
use camino::Utf8PathBuf;
use eyre::Result;
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use serde::Serialize;
use serde_repr::Serialize_repr;
use std::fmt::Display;
use std::time::SystemTime;

pub fn complete(
    cursor_before_line: &str,
    _col: usize,
    row: usize,
    path: &str,
    site: &Site,
) -> Vec<CompletionItem> {
    let lookup = match site.find_lookup_by_path(&path.into()) {
        Some(x) => x,
        None => return vec![],
    };

    if lookup.in_frontmatter(row) {
        // Expand tags in frontmatter if line starts with `tags = `
        if FRONTMATTER_TAG.is_match(cursor_before_line) {
            return tags_completions(site);
        }

        // Expand series in frontmatter if line starts with `series = `
        if FRONTMATTER_SERIES.is_match(cursor_before_line) {
            return series_completions(site);
        }
    } else {
        // Expand images separately because I only ever use it in a `![](/url)`
        // context and not mixing with other urls gives a more pleasant experience.
        if IMG_LINK.is_match(cursor_before_line) {
            return img_completions(site);
        }

        // Expand inline links, e.g. `[txt](/`
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

        // Expand div classes
        if AFTER_DIV_MARKER.is_match(cursor_before_line) {
            return div_class_completions();
        }

        // Expand symbols
        if AFTER_SYMBOL.is_match(cursor_before_line) {
            return symbol_completions();
        }
    }

    vec![]
}

lazy_static! {
    static ref IMG_LINK: Regex = Regex::new(r"!\[[^]]*\]\([^)]*$").unwrap();
    static ref INLINE_REL_LINK: Regex = Regex::new(r"]\((/[^)]*)$").unwrap();
    static ref INLINE_HEADER_REF: Regex = Regex::new(r"]\(#[^)]*$").unwrap();
    static ref LINK_DEF_REL_LINK: Regex = Regex::new(r"^\[.+\]:\s+(/.*)$").unwrap();
    static ref LINK_DEF_HEADER_REF: Regex = Regex::new(r"^\[.+\]:\s+#").unwrap();
    static ref FULL_LINK_TAG: Regex = Regex::new(r"\[[^\]]+\]\[[^\]]*$").unwrap();
    static ref FRONTMATTER_TAG: Regex = Regex::new(r"^tags(:| =) ").unwrap();
    static ref FRONTMATTER_SERIES: Regex = Regex::new(r"^series(:| =) ").unwrap();
    static ref OPEN_BRACKET: Regex = Regex::new(r"\[[^\]]*$").unwrap();
    static ref OPEN_BRACKET_FIRST: Regex = Regex::new(r"^\[[^\]]*$").unwrap();
    static ref AFTER_DIV_MARKER: Regex = Regex::new(r":{3,}\s+\w*$").unwrap();
    static ref AFTER_SYMBOL: Regex = Regex::new(r":\w*").unwrap();
}

fn img_completions(site: &Site) -> Vec<CompletionItem> {
    site.list_imgs()
        .map(|e| {
            CompletionItemBuilder::new_img(
                e.path.rel_path,
                e.meta.modified().expect("Modified should be available"),
            )
            .expect("Completion builder failed")
            .into()
        })
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

    append_series(CompletionType::Url, site, &mut res);
    append_tags(CompletionType::Url, site, &mut res);

    res.push(
        CompletionItemBuilder::new_constant(
            &site.content.projects.title,
            &site.content.projects.url,
        )
        .into(),
    );

    res
}

enum HeadingSource<'a> {
    SameFile,
    OtherFile { url: &'a str, path: &'a AbsPath },
}

fn heading_completions(lookup: &MarkupLookup, source: HeadingSource) -> Vec<CompletionItem> {
    lookup
        .headings
        .values()
        .map(|hs| {
            let h = &hs[0];
            let start_row = h.range.start.row;
            let end_row = h.range.end.row;

            let context = match source {
                HeadingSource::SameFile => HeadingContext::SameFile { start_row, end_row },
                HeadingSource::OtherFile { path, url } => HeadingContext::OtherFile {
                    path: path.to_string(),
                    url: url.to_string(),
                    start_row,
                    end_row,
                },
            };
            CompletionItemBuilder::Heading(HeadingInfo::from_heading(&h.heading, context)).into()
        })
        .collect()
}

fn split_heading_completions(caps: Captures<'_>, site: &Site) -> Option<Vec<CompletionItem>> {
    if let Some((url, _head)) = caps[1].split_once('#') {
        if let Some(post) = site.content.find_post_by_url(url) {
            if let Some(ref lookup) = post.markup_lookup {
                return Some(heading_completions(
                    lookup,
                    HeadingSource::OtherFile {
                        path: &post.path,
                        url,
                    },
                ));
            }
        }
    }
    None
}

fn link_tag_completions(lookup: &MarkupLookup) -> Vec<CompletionItem> {
    lookup
        .link_defs
        .values()
        .map(|defs| {
            let def = &defs[0];
            let start_row = def.range.start.row;
            let end_row = def.range.end.row;
            CompletionItemBuilder::LinkDefInfo(LinkDefInfo::from_link_def(
                &def.link_def,
                start_row,
                end_row,
            ))
            .into()
        })
        .collect()
}

fn append_broken_link_completions(lookup: &MarkupLookup, res: &mut Vec<CompletionItem>) {
    for (_, e) in lookup.char_pos_to_element.iter() {
        match &e.element {
            Element::Link(Link {
                link_ref: LinkRef::Unresolved { tag },
            })
            | Element::Img(Img {
                link_ref: ImgRef::Unresolved { tag },
            }) => {
                res.push(
                    CompletionItemBuilder::BrokenLink(BrokenLinkInfo::from_link(
                        tag,
                        e.range.start.row,
                    ))
                    .into(),
                );
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompletionType {
    Url,
    Id,
}

fn tags_completions(site: &Site) -> Vec<CompletionItem> {
    let mut res = Vec::new();
    append_tags(CompletionType::Id, site, &mut res);
    res
}

fn append_tags(t: CompletionType, site: &Site, res: &mut Vec<CompletionItem>) {
    for (tag, posts) in site.lookup.tags.iter() {
        res.push(CompletionItemBuilder::new_tag(t, tag, posts, site).into());
    }
}

fn series_completions(site: &Site) -> Vec<CompletionItem> {
    let mut res = Vec::new();
    append_series(CompletionType::Id, site, &mut res);
    res
}

fn append_series(t: CompletionType, site: &Site, res: &mut Vec<CompletionItem>) {
    for item in site.content.series.values() {
        res.push(CompletionItemBuilder::new_series(t, item, site).into());
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum DivClass {
    Flex,
    Gallery,
    Epigraph,
    Note,
    Tip,
    Warn,
    Important,
    Update,
    ListGreek,
    ListDash,
    ListPlus,
}

impl DivClass {
    #[allow(dead_code)]
    pub fn new(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "flex" => Some(Self::Flex),
            "gallery" => Some(Self::Gallery),
            "epigraph" => Some(Self::Epigraph),
            "note" => Some(Self::Note),
            "tip" => Some(Self::Tip),
            "warn" | "warning" => Some(Self::Warn),
            "important" => Some(Self::Important),
            "update" => Some(Self::Update),
            "greek" => Some(Self::ListGreek),
            "dash" => Some(Self::ListDash),
            "plus" => Some(Self::ListPlus),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Flex => "flex",
            Self::Gallery => "gallery",
            Self::Epigraph => "epigraph",
            Self::Note => "note",
            Self::Tip => "tip",
            Self::Warn => "warn",
            Self::Important => "important",
            Self::Update => "update",
            Self::ListGreek => "greek",
            Self::ListDash => "dash",
            Self::ListPlus => "plus",
        }
    }
}

impl Display for DivClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

fn div_class_completions() -> Vec<CompletionItem> {
    [
        DivClass::Flex,
        DivClass::Gallery,
        DivClass::Epigraph,
        DivClass::Note,
        DivClass::Tip,
        DivClass::Warn,
        DivClass::Important,
        DivClass::Update,
        DivClass::ListGreek,
        DivClass::ListDash,
        DivClass::ListPlus,
    ]
    .into_iter()
    .map(|e| CompletionItemBuilder::DivClass(e).into())
    .collect()
}

#[derive(Debug, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Symbol {
    TableOfContent,
}

impl Symbol {
    #[allow(dead_code)]
    pub fn new(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "table-of-content" => Some(Self::TableOfContent),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::TableOfContent => "table-of-content",
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

fn symbol_completions() -> Vec<CompletionItem> {
    [Symbol::TableOfContent]
        .into_iter()
        .map(|e| CompletionItemBuilder::Symbol(e).into())
        .collect()
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

#[derive(Debug, Serialize, PartialEq, Eq, Default)]
#[serde(rename_all = "camelCase")]
pub struct CompletionItem {
    // These are used by cmp.
    pub label: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub insert_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filter_text: Option<String>,
    pub kind: CompletionItemKind,

    // Blog specific metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(flatten)]
    pub info: Option<ContentInfo>,
}

pub enum CompletionItemBuilder {
    Post(PostInfo),
    Standalone(StandaloneInfo),
    Constant(ConstantInfo),
    Img(ImgInfo),
    Series(CompletionType, SeriesInfo),
    Tag(CompletionType, TagInfo),
    BrokenLink(BrokenLinkInfo),
    Heading(HeadingInfo),
    LinkDefInfo(LinkDefInfo),
    DivClass(DivClass),
    Symbol(Symbol),
}

impl CompletionItemBuilder {
    pub fn new_img(path: RelPath, t: SystemTime) -> Result<Self> {
        Ok(Self::Img(ImgInfo {
            url: Utf8PathBuf::from("/images/").join(path.0).to_string(),
            modified: t.duration_since(SystemTime::UNIX_EPOCH)?.as_secs(),
        }))
    }

    pub fn new_post(item: &PostItem) -> Self {
        Self::Post(item.into())
    }

    pub fn new_standalone(item: &StandaloneItem) -> Self {
        Self::Standalone(item.into())
    }

    pub fn new_constant(title: &str, url: &SiteUrl) -> Self {
        Self::Constant(ConstantInfo {
            title: title.to_string(),
            url: url.href().to_string(),
        })
    }

    pub fn new_series(t: CompletionType, item: &SeriesItem, site: &Site) -> Self {
        Self::Series(t, SeriesInfo::from(item, site))
    }

    pub fn new_tag(t: CompletionType, tag: &Tag, posts: &[PostRef], site: &Site) -> Self {
        Self::Tag(t, TagInfo::from_tag(tag, posts, site))
    }
}

impl From<CompletionItemBuilder> for CompletionItem {
    fn from(value: CompletionItemBuilder) -> CompletionItem {
        match value {
            CompletionItemBuilder::Img(info) => CompletionItem {
                label: info.url.clone(),
                kind: CompletionItemKind::File,
                info: Some(ContentInfo::Img(info)),
                ..Default::default()
            },
            CompletionItemBuilder::Post(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.title.as_str()].join("|")),
                label: info.title.clone(),
                insert_text: Some(info.url.clone()),
                kind: CompletionItemKind::File,
                info: Some(ContentInfo::Post(info)),
            },
            CompletionItemBuilder::Standalone(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.title.as_str()].join("|")),
                label: info.title.clone(),
                insert_text: Some(info.url.clone()),
                kind: CompletionItemKind::File,
                info: Some(ContentInfo::Standalone(info)),
            },
            CompletionItemBuilder::Constant(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.title.as_str()].join("|")),
                label: info.title.clone(),
                insert_text: Some(info.url.clone()),
                kind: CompletionItemKind::Constant,
                info: Some(ContentInfo::Constant(info)),
            },
            CompletionItemBuilder::Series(t, info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.title.as_str()].join("|")),
                label: info.title.clone(),
                insert_text: match t {
                    CompletionType::Url => Some(info.url.clone()),
                    CompletionType::Id => Some(info.id.clone()),
                },
                kind: CompletionItemKind::Module,
                info: Some(ContentInfo::Series(info)),
            },
            CompletionItemBuilder::Tag(t, info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.name.as_str()].join("|")),
                label: info.name.clone(),
                insert_text: match t {
                    CompletionType::Url => Some(info.url.clone()),
                    CompletionType::Id => Some(info.name.clone()),
                },
                kind: CompletionItemKind::Folder,
                info: Some(ContentInfo::Tag(info)),
            },
            CompletionItemBuilder::Heading(info) => CompletionItem {
                label: format!("{} {}", "#".repeat(info.level.into()), info.content),
                filter_text: Some(info.content.clone()),
                insert_text: Some(info.id.clone()),
                kind: CompletionItemKind::Class,
                info: Some(ContentInfo::Heading(info)),
            },
            CompletionItemBuilder::LinkDefInfo(info) => CompletionItem {
                filter_text: Some([info.url.as_str(), info.label.as_str()].join("|")),
                label: info.label.clone(),
                insert_text: Some(info.label.clone()),
                info: Some(ContentInfo::LinkDef(info)),
                kind: CompletionItemKind::Reference,
            },
            CompletionItemBuilder::BrokenLink(info) => CompletionItem {
                label: info.tag.clone(),
                info: Some(ContentInfo::BrokenLink(info)),
                kind: CompletionItemKind::Field,
                ..Default::default()
            },
            CompletionItemBuilder::DivClass(class) => CompletionItem {
                label: class.to_string(),
                info: Some(ContentInfo::DivClass(DivClassInfo {
                    name: class.as_str(),
                })),
                kind: CompletionItemKind::Keyword,
                ..Default::default()
            },
            CompletionItemBuilder::Symbol(sym) => CompletionItem {
                label: sym.to_string(),
                info: Some(ContentInfo::Symbol(SymbolInfo { sym: sym.as_str() })),
                kind: CompletionItemKind::Keyword,
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
        }
        .build()?;

        let items = complete(
            "](/",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );

        assert_eq!(
            find_insert_text("/blog/2022/02/01/feb_post", &items),
            Some(&CompletionItem {
                label: "Feb post 1".into(),
                insert_text: Some("/blog/2022/02/01/feb_post".into()),
                filter_text: Some("/blog/2022/02/01/feb_post|Feb post 1".into()),
                kind: CompletionItemKind::File,
                info: Some(ContentInfo::Post(PostInfo {
                    title: "Feb post 1".into(),
                    path: test_site
                        .input_path("posts/2022-02-01-feb_post.dj")
                        .to_string(),
                    created: "2022-02-01".to_string(),
                    url: "/blog/2022/02/01/feb_post".to_string(),
                    tags: vec!["One".to_string()],
                    series: Some("myseries".to_string())
                }))
            })
        );

        let myseries =
            find_insert_text("/series/myseries", &items).expect("Should find `myseries`");
        assert_eq!(myseries.label, "My series");
        assert_eq!(myseries.insert_text, Some("/series/myseries".to_string()));
        assert_eq!(
            myseries.filter_text,
            Some("/series/myseries|My series".into())
        );
        let series_info = if let Some(ContentInfo::Series(ref x)) = myseries.info {
            x
        } else {
            panic!("Wrong series info");
        };
        assert_eq!(series_info.id, "myseries");
        assert_eq!(series_info.title, "My series");
        assert_eq!(series_info.url, "/series/myseries");
        assert_eq!(
            series_info.path,
            test_site.input_path("series/myseries.dj").as_str()
        );
        assert_eq!(series_info.posts.len(), 2);

        let one = find_insert_text("/blog/tags/one", &items).expect("Should find tag `One`");
        assert_eq!(one.label, "One");
        assert_eq!(one.insert_text, Some("/blog/tags/one".to_string()));
        assert_eq!(one.filter_text, Some("/blog/tags/one|One".into()));
        let one_info = if let Some(ContentInfo::Tag(ref tag)) = one.info {
            tag
        } else {
            panic!("Wrong tag info");
        };
        assert_eq!(one_info.id, "One");
        assert_eq!(one_info.name, "One");
        assert_eq!(one_info.url, "/blog/tags/one");
        assert_eq!(one_info.posts.len(), 3);

        // dbg!(&items);

        assert_eq!(
            find_insert_text("/404", &items),
            Some(&CompletionItem {
                label: "404".into(),
                insert_text: Some("/404".into()),
                filter_text: Some("/404|404".into()),
                kind: CompletionItemKind::File,
                info: Some(ContentInfo::Standalone(StandaloneInfo {
                    title: "404".into(),
                    url: "/404".into(),
                    path: test_site.input_path("standalone/404.markdown").to_string(),
                    is_draft: false
                }))
            })
        );

        assert_eq!(
            find_insert_text("/projects", &items),
            Some(&CompletionItem {
                label: "Projects".into(),
                insert_text: Some("/projects".into()),
                filter_text: Some("/projects|Projects".into()),
                kind: CompletionItemKind::Constant,
                info: Some(ContentInfo::Constant(ConstantInfo {
                    title: "Projects".into(),
                    url: "/projects".into(),
                }))
            })
        );

        let def_items = complete(
            "[tag]: /",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );
        assert_eq!(items, def_items);

        let heading_items = complete(
            "](/blog/2022/02/01/feb_post#",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );

        assert_eq!(heading_items.len(), 1);

        assert_eq!(
            find_insert_text("heading-with-text", &heading_items),
            Some(&CompletionItem {
                label: "# heading with text".into(),
                insert_text: Some("heading-with-text".into()),
                filter_text: Some("heading with text".into()),
                kind: CompletionItemKind::Class,
                info: Some(ContentInfo::Heading(HeadingInfo {
                    id: "heading-with-text".into(),
                    content: "heading with text".into(),
                    level: 1,
                    context: HeadingContext::OtherFile {
                        path: test_site
                            .input_path("posts/2022-02-01-feb_post.dj")
                            .to_string(),
                        url: "/blog/2022/02/01/feb_post".into(),
                        start_row: 16,
                        end_row: 17
                    }
                }))
            })
        );

        let def_heading_items = complete(
            "[tag]: /blog/2022/02/01/feb_post#",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );
        assert_eq!(heading_items, def_heading_items);

        Ok(())
    }

    #[test]
    fn test_header_ref_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let items = complete(
            "](#",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );

        assert_eq!(items.len(), 3);

        assert_eq!(
            find_insert_text("Regular-heading", &items),
            Some(&CompletionItem {
                label: "## Regular heading".into(),
                insert_text: Some("Regular-heading".into()),
                filter_text: Some("Regular heading".into()),
                kind: CompletionItemKind::Class,
                info: Some(ContentInfo::Heading(HeadingInfo {
                    id: "Regular-heading".into(),
                    content: "Regular heading".into(),
                    level: 2,
                    context: HeadingContext::SameFile {
                        start_row: 56,
                        end_row: 57
                    }
                }))
            })
        );

        let def_items = complete(
            "[tag]: #",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );
        assert_eq!(items, def_items);

        Ok(())
    }

    #[test]
    fn test_full_link_tag_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let items = complete(
            "[some text][",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );

        assert_eq!(items.len(), 2);

        assert_eq!(
            find_insert_text("tag1", &items),
            Some(&CompletionItem {
                label: "tag1".into(),
                insert_text: Some("tag1".to_string()),
                filter_text: Some("/404|tag1".into()),
                kind: CompletionItemKind::Reference,
                info: Some(ContentInfo::LinkDef(LinkDefInfo {
                    label: "tag1".into(),
                    url: "/404".into(),
                    start_row: 35,
                    end_row: 36
                }))
            })
        );

        Ok(())
    }

    #[test]
    fn test_short_link_tag_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let items = complete(
            // Not the first in line, to only complete link def tags.
            "x [",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );

        assert_eq!(items.len(), 2);

        assert_eq!(
            find_insert_text("tag1", &items),
            Some(&CompletionItem {
                label: "tag1".into(),
                insert_text: Some("tag1".into()),
                filter_text: Some("/404|tag1".into()),
                kind: CompletionItemKind::Reference,
                info: Some(ContentInfo::LinkDef(LinkDefInfo {
                    label: "tag1".into(),
                    url: "/404".into(),
                    start_row: 35,
                    end_row: 36
                }))
            })
        );

        // First in line, so we should complete broken link tags as well.
        let items = complete(
            "[",
            0,
            7,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );

        assert_eq!(items.len(), 3);

        assert_eq!(
            find_label("broken_tag", &items),
            Some(&CompletionItem {
                label: "broken_tag".into(),
                insert_text: None,
                filter_text: None,
                kind: CompletionItemKind::Field,
                info: Some(ContentInfo::BrokenLink(BrokenLinkInfo {
                    tag: "broken_tag".into(),
                    row: 33
                }))
            })
        );

        Ok(())
    }

    #[test]
    fn test_frontmatter_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let tags = complete(
            "tags = ",
            0,
            0,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );

        assert_eq!(tags.len(), 3);

        let one = find_insert_text("One", &tags).expect("Should find tag `One`");
        assert_eq!(one.label, "One");
        assert_eq!(one.insert_text, Some("One".to_string()));
        assert_eq!(one.filter_text, Some("/blog/tags/one|One".into()));
        let one_info = if let Some(ContentInfo::Tag(ref tag)) = one.info {
            tag
        } else {
            panic!("Wrong tag info");
        };
        assert_eq!(one_info.id, "One");
        assert_eq!(one_info.name, "One");
        assert_eq!(one_info.url, "/blog/tags/one");
        assert_eq!(one_info.posts.len(), 3);

        let series = complete(
            "series = ",
            0,
            0,
            test_site
                .input_path("posts/2022-01-31-test_post.dj")
                .as_str(),
            &test_site.site,
        );

        assert_eq!(series.len(), 1);

        let myseries = find_insert_text("myseries", &series).expect("Should find `myseries`");
        assert_eq!(myseries.label, "My series");
        assert_eq!(myseries.insert_text, Some("myseries".to_string()));
        assert_eq!(
            myseries.filter_text,
            Some("/series/myseries|My series".into())
        );
        let series_info = if let Some(ContentInfo::Series(ref x)) = myseries.info {
            x
        } else {
            panic!("Wrong series info");
        };
        assert_eq!(series_info.id, "myseries");
        assert_eq!(series_info.title, "My series");
        assert_eq!(series_info.url, "/series/myseries");
        assert_eq!(
            series_info.path,
            test_site.input_path("series/myseries.dj").as_str()
        );
        assert_eq!(series_info.posts.len(), 2);

        Ok(())
    }
}
