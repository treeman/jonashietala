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
use crate::markdown::markdown_to_html_feed;
use crate::site::BASE_SITE_URL;
use crate::site_url::SiteUrl;

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
                value: Some(markdown_to_html_feed(&post.raw_content)),
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
            title: "Jonas Hietala: ...".into(),
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
}
