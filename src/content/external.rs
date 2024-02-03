use std::borrow::Cow;

use crate::item::Item;
use crate::item::RenderContext;
use crate::util;
use eyre::{eyre, Result};

pub struct SassItem;

impl Item for SassItem {
    fn render(&self, ctx: &RenderContext) -> Result<()> {
        let sass = grass::from_path("css/main.scss", &grass::Options::default())
            .map_err(|err| eyre!("Sass error: {}", err))?;
        let output_file = ctx.output_dir.join("css/main.css");
        util::write_to_file(&output_file, &sass)
    }

    fn id(&self) -> Cow<str> {
        "sass".into()
    }
}

pub struct JsItem;

impl Item for JsItem {
    fn render(&self, ctx: &RenderContext) -> Result<()> {
        util::copy_file("js/main.js".into(), &ctx.output_dir.join("js/main.js"))
    }

    fn id(&self) -> Cow<str> {
        "js".into()
    }
}
