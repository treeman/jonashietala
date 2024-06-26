use crate::paths::AbsPath;
use serde::Serialize;
use std::collections::BTreeMap;
use tera::Context;

use crate::{
    content::{SeriesContext, SeriesItem, SeriesRef},
    context::RenderContext,
    item::TeraItem,
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

    fn tera_url(&self) -> &SiteUrl {
        &self.url
    }

    fn tera_source_file(&self) -> Option<&AbsPath> {
        None
    }
}

#[derive(Debug, Clone, Serialize)]
struct SeriesArchiveContext<'a> {
    title: &'static str,
    series: Vec<SeriesContext<'a>>,
}
