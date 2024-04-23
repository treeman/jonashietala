use crate::markup::markup_lookup::{ElementInfo, Img, ImgRef, Link, LinkDef, LinkRef, NeovimRange};
use crate::paths::AbsPath;
use crate::{markup::MarkupLookup, site::Site};
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum GotoDefRes {
    OtherFile { path: AbsPath },
    SameFile { row: usize, col: usize },
}

impl From<NeovimRange> for GotoDefRes {
    fn from(r: NeovimRange) -> Self {
        GotoDefRes::SameFile {
            row: r.start.row,
            col: r.start.col,
        }
    }
}

pub fn goto_def(linenum: usize, column: usize, path: &str, site: &Site) -> Option<GotoDefRes> {
    let lookup = site.content.find_post_lookup_by_file_name(&path)?;

    match lookup.element_at(linenum, column)? {
        ElementInfo::Link(Link {
            link_ref: LinkRef::Inline(url),
            ..
        }) => goto_url(url, lookup, site),
        ElementInfo::Link(Link {
            link_ref: LinkRef::Reference { label, url },
            ..
        }) => {
            if let Some(defs) = lookup.link_defs.get(label) {
                let def = &defs[0];
                Some(def.range.into())
            } else {
                // May happen for short heading links
                goto_url(url, lookup, site)
            }
        }
        ElementInfo::Link(Link {
            link_ref: LinkRef::AutoLink(url),
            ..
        }) => goto_url(url, lookup, site),
        ElementInfo::Link(_) => None,
        ElementInfo::Img(Img {
            link_ref: ImgRef::Reference { label, .. },
            ..
        }) => {
            if let Some(defs) = lookup.link_defs.get(label) {
                let def = &defs[0];
                Some(def.range.into())
            } else {
                None
            }
        }
        ElementInfo::Img(_) => None,
        ElementInfo::LinkDef(LinkDef { url, .. }) => goto_url(url, lookup, site),
        ElementInfo::Heading(_) => None,
    }
}

lazy_static! {
    static ref HASH_REF: Regex = Regex::new(r"^#(.+)$").unwrap();
}

fn goto_url(url: &str, lookup: &MarkupLookup, site: &Site) -> Option<GotoDefRes> {
    let hash_ref = HASH_REF.captures(url.trim());
    if let Some(hash_match) = hash_ref {
        if let Some(hs) = lookup.headings.get(&hash_match[1]) {
            let heading = &hs[0];
            return Some(heading.range.into());
        }
    }

    if let Some(post) = site.content.find_post_by_url(url) {
        return Some(GotoDefRes::OtherFile {
            path: post.path.clone().into(),
        });
    }

    for hs in lookup.headings.values() {
        for heading in hs.iter() {
            if heading.content == url {
                return Some(heading.range.into());
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    use eyre::Result;

    #[test]
    fn test_goto_link_def() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: true,
        }
        .build()?;

        let feb_post = "posts/2022-02-01-feb_post.dj";

        let feb_post2 = "posts/2022-02-02-feb_post2.dj";
        let abs_feb_post2 = test_site.input_path(feb_post2);

        // Goto link def from full reference link
        assert_eq!(
            goto_def(9, 16, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 13, col: 0 })
        );

        // Goto link def from collapsed reference
        assert_eq!(
            goto_def(11, 2, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 13, col: 0 })
        );

        // Goto link def from collapsed reference
        assert_eq!(
            goto_def(11, 2, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 13, col: 0 })
        );

        // Goto other post from link def
        assert_eq!(
            goto_def(13, 8, feb_post, &test_site.site),
            Some(GotoDefRes::OtherFile {
                path: abs_feb_post2.clone()
            })
        );

        // Goto other post from inline link
        assert_eq!(
            goto_def(15, 5, feb_post, &test_site.site),
            Some(GotoDefRes::OtherFile {
                path: abs_feb_post2.clone()
            })
        );

        // Goto explicit heading from inline link
        assert_eq!(
            goto_def(21, 16, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 17, col: 0 })
        );

        // Goto heading from link def
        assert_eq!(
            goto_def(23, 7, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 17, col: 0 })
        );

        // Goto short heading
        assert_eq!(
            goto_def(19, 3, feb_post, &test_site.site),
            Some(GotoDefRes::SameFile { row: 17, col: 0 })
        );

        // Above markup, inside frontmatter
        assert_eq!(goto_def(1, 0, feb_post, &test_site.site), None);

        // Past eof
        assert_eq!(goto_def(99999999, 0, feb_post, &test_site.site), None);

        // Past column
        assert_eq!(goto_def(11, 9999999, feb_post, &test_site.site), None);

        Ok(())
    }
}
