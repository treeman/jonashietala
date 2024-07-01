use crate::markup::markup_lookup::{Element, Img, ImgRef, Link, LinkDef, LinkRef, PosRange};
use crate::paths::AbsPath;
use crate::{markup::MarkupLookup, site::Site};
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum GotoDefRes {
    OtherFile { path: AbsPath },
    SameFile { row: usize, col: usize },
}

impl From<PosRange> for GotoDefRes {
    fn from(r: PosRange) -> Self {
        GotoDefRes::SameFile {
            row: r.start.row,
            col: r.start.col,
        }
    }
}

pub fn goto_def(linenum: usize, column: usize, path: &str, site: &Site) -> Option<GotoDefRes> {
    let lookup = site.find_lookup_by_path(&path.into())?;

    match &lookup.element_at(linenum, column)?.element {
        Element::Link(Link {
            link_ref: LinkRef::Inline { url },
        }) => goto_url(url, lookup, site),
        Element::Link(Link {
            link_ref: LinkRef::Reference { label, url },
        }) => {
            if let Some(defs) = lookup.link_defs.get(label) {
                let def = &defs[0];
                Some(def.range.into())
            } else {
                // May happen for short heading links
                goto_url(url, lookup, site)
            }
        }
        Element::Link(Link {
            link_ref: LinkRef::AutoLink { url },
            ..
        }) => goto_url(url, lookup, site),
        Element::Link(_) => None,
        Element::Img(Img {
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
        Element::Img(_) => None,
        Element::LinkDef(LinkDef { url, .. }) => goto_url(url, lookup, site),
        Element::Heading(_) => None,
        Element::Todo(_) => None,
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
            path: post.path.clone(),
        });
    }

    for hs in lookup.headings.values() {
        for h in hs.iter() {
            if h.heading.content == url {
                return Some(h.range.into());
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
        }
        .build()?;

        let feb_post = test_site.input_path("posts/2022-02-01-feb_post.dj");
        let feb_post2 = test_site.input_path("posts/2022-02-02-feb_post2.dj");

        // Goto link def from full reference link
        assert_eq!(
            goto_def(8, 16, feb_post.as_str(), &test_site.site),
            Some(GotoDefRes::SameFile { row: 12, col: 0 })
        );

        // Goto link def from collapsed reference
        assert_eq!(
            goto_def(10, 2, feb_post.as_str(), &test_site.site),
            Some(GotoDefRes::SameFile { row: 12, col: 0 })
        );

        // Goto link def from collapsed reference
        assert_eq!(
            goto_def(10, 2, feb_post.as_str(), &test_site.site),
            Some(GotoDefRes::SameFile { row: 12, col: 0 })
        );

        // Goto other post from link def
        assert_eq!(
            goto_def(12, 8, feb_post.as_str(), &test_site.site),
            Some(GotoDefRes::OtherFile {
                path: feb_post2.clone()
            })
        );

        // Goto other post from inline link
        assert_eq!(
            goto_def(14, 5, feb_post.as_str(), &test_site.site),
            Some(GotoDefRes::OtherFile {
                path: feb_post2.clone()
            })
        );

        // Goto explicit heading from inline link
        assert_eq!(
            goto_def(20, 16, feb_post.as_str(), &test_site.site),
            Some(GotoDefRes::SameFile { row: 16, col: 0 })
        );

        // Goto heading from link def
        assert_eq!(
            goto_def(22, 7, feb_post.as_str(), &test_site.site),
            Some(GotoDefRes::SameFile { row: 16, col: 0 })
        );

        // Goto short heading
        assert_eq!(
            goto_def(18, 3, feb_post.as_str(), &test_site.site),
            Some(GotoDefRes::SameFile { row: 16, col: 0 })
        );

        // Above markup, inside frontmatter
        assert_eq!(goto_def(1, 0, feb_post.as_str(), &test_site.site), None);

        // Past eof
        assert_eq!(
            goto_def(99999999, 0, feb_post.as_str(), &test_site.site),
            None
        );

        // Past column
        assert_eq!(
            goto_def(11, 9999999, feb_post.as_str(), &test_site.site),
            None
        );

        Ok(())
    }

    #[test]
    fn test_goto_in_series() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let path = test_site.input_path("series/myseries.dj");

        assert_eq!(
            goto_def(6, 10, path.as_str(), &test_site.site),
            Some(GotoDefRes::SameFile { row: 8, col: 0 })
        );

        Ok(())
    }
}
