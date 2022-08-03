use std::borrow::Cow;

use crate::item::Item;
use crate::item::RenderContext;
use crate::util;
use eyre::{eyre, Result};

pub struct Sass;

impl Item for Sass {
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
