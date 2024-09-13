use crate::context::RenderContext;
use crate::item::Item;
use crate::paths::AbsPath;
use crate::site_url::SiteUrl;
use crate::util;
use eyre::{eyre, Result};
use lazy_static::lazy_static;

lazy_static! {
    static ref SASS_URL: SiteUrl = SiteUrl::parse("css/main.css").unwrap();
    static ref JS_URL: SiteUrl = SiteUrl::parse("js/main.js").unwrap();
}

#[derive(Debug)]
pub struct SassItem;

impl Item for SassItem {
    fn render(&self, ctx: &RenderContext) -> Result<()> {
        let sass = grass::from_path("css/main.scss", &grass::Options::default())
            .map_err(|err| eyre!("Sass error: {}", err))?;
        let output_file = ctx.output_dir.join("css/main.css");
        util::write_to_file(output_file, sass)
    }

    fn url(&self) -> &SiteUrl {
        &SASS_URL
    }

    fn source_file(&self) -> Option<&AbsPath> {
        None
    }
}

#[derive(Debug)]
pub struct JsItem;

impl Item for JsItem {
    fn render(&self, ctx: &RenderContext) -> Result<()> {
        util::copy_file("js/main.js", ctx.output_dir.join("js/main.js").as_str())
    }

    fn url(&self) -> &SiteUrl {
        &SASS_URL
    }

    fn source_file(&self) -> Option<&AbsPath> {
        None
    }
}
