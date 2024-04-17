use super::messages::{CompletionItem, CompletionItemBuilder, CompletionItemKind};
use crate::site::Site;
use lazy_static::lazy_static;
use regex::Regex;

pub fn complete(
    cursor_before_line: &str,
    _col: usize,
    _row: usize,
    _path: &str,
    site: &Site,
) -> Vec<CompletionItem> {
    // let lookup = match site.content.find_post_lookup(&path) {
    //     Some(x) => x,
    //     None => return vec![],
    // };

    // -- Expand images separately because I only ever use it in a -- `![](/url)`
    // -- context and not mixing with other urls gives a more pleasant experience.
    // string.match(cursor_before_line, "!%[%]%([^%)]*$")
    // if IMG_LINK.is_match(cursor_before_line) {}

    // -- Expand inline links, e.g. `[txt](/`
    if INLINE_REL_LINK.is_match(cursor_before_line) {
        return url_completions(site);
    }

    // -- Expanding headings in inline links, e.g. `[txt](#`
    // string.match(cursor_before_line, "%]%(#[^%)]*$")
    // if INLINE_HEADER_REF.is_match(cursor_before_line) {}

    // -- Expand links in link ref definitions, e.g. `[label]: `
    // string.match(cursor_before_line, "^%[.+%]:%s+")
    // if LINK_DEF_REL_LINK.is_match(cursor_before_line) {}

    // -- Expanding links in ref defs, e.g. `[label]: #`
    // string.match(cursor_before_line, "^%[.+%]:%s+#")
    // if LINK_DEF_HEADER_REF.is_match(cursor_before_line) {}

    // -- Expand url definition tags in `[text][tag]`
    // if string.match(cursor_before_line, "%[[^%]]+%]%[[^%]]*$")
    // if FULL_LINK_TAG.is_match(cursor_before_line) {}

    // -- Expand url definition tags in `[tag][]`, simplified to after a `[`
    // -- If at the beginning of the line, we should complete broken link tags to..
    // if string.match(cursor_before_line, "%[[^%]]*$")
    // if COLLAPSED_LINK_TAG.is_match(cursor_before_line) {}

    // if lookup.in_frontmatter(row) {
    //     // -- Expand tags in frontmatter if line starts with `tags = `
    //     // if string.match(cursor_line, "^tags = ")
    //     if FRONTMATTER_TAG.is_match(cursor_before_line) {}

    //     // -- Expand series in frontmatter if line starts with `series = `
    //     // if string.match(cursor_line, "^series = ")
    //     if FRONTMATTER_SERIES.is_match(cursor_before_line) {}
    // }

    vec![]
}

lazy_static! {
    static ref IMG_LINK: Regex = Regex::new(r"").unwrap();
    static ref INLINE_REL_LINK: Regex = Regex::new(r"]\(/[^)]*$").unwrap();
    static ref INLINE_HEADER_REF: Regex = Regex::new(r"").unwrap();
    static ref LINK_DEF_REL_LINK: Regex = Regex::new(r"").unwrap();
    static ref LINK_DEF_HEADER_REF: Regex = Regex::new(r"").unwrap();
    static ref FULL_LINK_TAG: Regex = Regex::new(r"").unwrap();
    static ref COLLAPSED_LINK_TAG: Regex = Regex::new(r"").unwrap();
    static ref FRONTMATTER_TAG: Regex = Regex::new(r"").unwrap();
    static ref FRONTMATTER_SERIES: Regex = Regex::new(r"").unwrap();
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

    for item in site.content.series.values() {
        res.push(
            CompletionItemBuilder::from_url(&item.title, &item.url, CompletionItemKind::Module)
                .with_path(&item.path)
                .with_label_tag("Series")
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

    for item in site.lookup.tags.keys() {
        res.push(
            CompletionItemBuilder::from_url(&item.name, &item.url, CompletionItemKind::Folder)
                .with_label_tag("Tag")
                .into(),
        );
    }

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

    #[test]
    fn test_completion() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let items = complete("](/", 0, 0, "", &test_site.site);

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

        Ok(())
    }
}
