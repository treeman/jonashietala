use atom_syndication::Content;
use atom_syndication::Entry;
use atom_syndication::Feed;
use atom_syndication::Link;
use atom_syndication::Person;
use chrono::DateTime;
use chrono::FixedOffset;
use chrono::NaiveDateTime;
use eyre::Result;
use std::borrow::Cow;
use std::fs::File;
use tracing::debug;

use crate::content::PostItem;
use crate::item::Item;
use crate::item::RenderContext;
use crate::markup::ParseContext;
use crate::site::BASE_SITE_URL;
use crate::site_url::SiteUrl;

#[derive(Debug)]
pub struct SiteFeed;

impl From<&PostItem> for Entry {
    fn from(post: &PostItem) -> Self {
        Self {
            title: post.title.clone().into(),
            id: entity_id(&post.url),
            updated: fixed_date_time(post.updated),
            published: Some(fixed_date_time(post.created)),
            links: vec![Link {
                href: post.url.href().to_string(),
                rel: "alternate".to_string(),
                ..Default::default()
            }],
            content: Some(Content {
                base: Some(BASE_SITE_URL.to_string()),
                content_type: Some("html".to_string()),
                value: Some(
                    post.markup
                        .parse_feed(ParseContext::default())
                        .expect("Should be able to generate feed markup")
                        .0,
                ),
                ..Default::default()
            }),
            ..Default::default()
        }
    }
}

fn fixed_date_time(dt: NaiveDateTime) -> DateTime<FixedOffset> {
    DateTime::from_naive_utc_and_offset(dt, FixedOffset::east_opt(0).unwrap())
}

// We've used "http://{path}index.html" as the id, since they shouldn't change let's continue with it
fn entity_id(site_url: &SiteUrl) -> String {
    let mut res = site_url.url.clone();
    res.set_scheme("http").unwrap();
    let mut res = String::from(res);
    res = res.replacen("http://www.", "http://", 1);
    if res.ends_with('/') {
        res.pop();
    }
    res.push_str("/index.html");
    res
}

impl Item for SiteFeed {
    fn render(&self, ctx: &RenderContext) -> Result<()> {
        let output_file = ctx.output_dir.join("feed.xml");
        debug!("Writing {output_file}");

        let file = File::create(output_file)?;

        let feed = Feed {
            title: "Jonas Hietala".into(),
            id: "http://www.jonashietala.se/feed.xml".to_string(),
            links: vec![
                Link {
                    href: BASE_SITE_URL.join("feed.xml").unwrap().to_string(),
                    rel: "self".to_string(),
                    ..Default::default()
                },
                Link {
                    href: BASE_SITE_URL.to_string(),
                    rel: "alternate".to_string(),
                    ..Default::default()
                },
            ],
            authors: vec![Person {
                name: "Jonas Hietala".to_string(),
                email: Some("mail@jonashietala.se".to_string()),
                uri: Some(BASE_SITE_URL.to_string()),
            }],
            base: Some(BASE_SITE_URL.to_string()),
            updated: ctx
                .content
                .posts
                .values()
                .max_by_key(|post| post.updated)
                .map(|post| fixed_date_time(post.updated))
                .unwrap(),
            entries: ctx.content.posts.values().map(Entry::from).collect(),
            ..Default::default()
        };

        feed.write_to(file)?;

        Ok(())
    }

    fn id(&self) -> Cow<str> {
        "feed".into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;
    use sxd_document::{dom::Document, parser};
    use sxd_xpath::{context::Context, Factory, Value};

    #[test]
    fn test_entity_id() {
        assert_eq!(
            entity_id(&SiteUrl::parse("/blog/2022/01/31/test_post").unwrap()),
            "http://jonashietala.se/blog/2022/01/31/test_post/index.html"
        );

        assert_eq!(
            entity_id(&SiteUrl::parse("/blog/2022/01/31/test_post/").unwrap()),
            "http://jonashietala.se/blog/2022/01/31/test_post/index.html"
        );
    }

    #[test]
    fn test_feed() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
        }
        .build()?;

        let feed = test_site.output_content("feed.xml")?;
        let package = parser::parse(&feed)?;
        let document = package.as_document();

        let feed_id = collect_string(&document, "/a:feed/a:id")?;

        // Not sure if we messed up one time, but at the moment this is the id we're using,
        // so let's continue with that...
        assert_eq!(feed_id, "http://www.jonashietala.se/feed.xml");

        let links = collect_strings(&document, "/a:feed/a:link/@href")?;
        assert_eq!(
            links,
            vec![
                "https://www.jonashietala.se/",
                "https://www.jonashietala.se/feed.xml",
            ]
        );

        // I can't seem to extract the xml:base property. Sigh
        // let entry_content_base =
        //     collect_string(&document, "/a:feed/a:entry/a:content/*[name()='xml:base']")?;
        // assert_eq!(entry_content_base, "https://www.jonashietala.se/");

        let entry_ids = collect_strings(&document, "/a:feed/a:entry/a:id")?;

        // Entry ids should be of the form http:// because the ids should never change!
        assert_eq!(
            entry_ids,
            vec![
                "http://jonashietala.se/blog/2022/01/31/test_post/index.html",
                "http://jonashietala.se/blog/2022/02/01/feb_post/index.html",
                "http://jonashietala.se/blog/2022/02/02/feb_post2/index.html",
                "http://jonashietala.se/blog/2022/02/02/feb_post_dupe/index.html"
            ]
        );

        Ok(())
    }

    fn context<'a>() -> Context<'a> {
        // Aah, the beauty that is xml namespaces.
        let mut context = Context::new();
        context.set_namespace("a", "http://www.w3.org/2005/Atom");
        context
    }

    fn collect_string(document: &Document, path: &str) -> Result<String> {
        let xpath = Factory::new().build(path).unwrap().unwrap();
        let res = xpath.evaluate(&context(), document.root())?;
        Ok(res.string())
    }

    fn collect_strings(document: &Document, path: &str) -> Result<Vec<String>> {
        let xpath = Factory::new().build(path).unwrap().unwrap();
        let values = xpath.evaluate(&context(), document.root())?;

        let mut res = Vec::new();
        if let Value::Nodeset(nodes) = values {
            for x in nodes {
                res.push(x.string_value());
            }
        }
        res.sort();
        Ok(res)
    }
}
