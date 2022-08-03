use serde::Serialize;
use std::collections::BTreeMap;
use tera::Context;

use crate::{
    content::{SeriesContext, SeriesItem, SeriesRef},
    item::{RenderContext, TeraItem},
    site_url::SiteUrl,
};

#[derive(Debug)]
pub struct SeriesArchiveItem {
    pub url: SiteUrl,
    pub series: Vec<SeriesRef>,
}

impl SeriesArchiveItem {
    pub fn new(series: &BTreeMap<SeriesRef, SeriesItem>) -> Self {
        Self {
            url: SiteUrl::parse("/series").unwrap(),
            series: series.keys().map(Clone::clone).collect(),
        }
    }
}

impl TeraItem for SeriesArchiveItem {
    fn context(&self, ctx: &RenderContext) -> Context {
        Context::from_serialize(SeriesArchiveContext {
            title: "All series",
            series: self
                .series
                .iter()
                .map(|series| SeriesContext::from_ref(series, ctx))
                .collect(),
        })
        .unwrap()
    }

    fn template(&self) -> &str {
        "series_archive.html"
    }

    fn url(&self) -> &SiteUrl {
        &self.url
    }
}

#[derive(Debug, Clone, Serialize)]
struct SeriesArchiveContext<'a> {
    title: &'static str,
    series: Vec<SeriesContext<'a>>,
}

// impl<'a> SeriesArchiveContext<'a> {
//     fn from_ref(series_ref: &SeriesRef, ctx: &'a RenderContext) -> Self {
//         let series = ctx
//             .content
//             .get_series(series_ref)
//             .as_ref()
//             .expect("Should have series");
//
//         SeriesArchiveContext { title: series.title, series: series.posts.
//         }
//     }
