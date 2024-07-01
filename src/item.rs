use crate::context::RenderContext;
use crate::paths::AbsPath;
use crate::util;
use crate::{content, site_url::SiteUrl};
use camino::{Utf8Path, Utf8PathBuf};
use eyre::Result;
use std::borrow::Cow;
use std::fmt::Debug;
use std::io::Write;
use tera::Context;
use tracing::debug;

pub trait Item: Send + Sync + Debug {
    fn render(&self, ctx: &RenderContext) -> Result<()>;

    fn url(&self) -> &SiteUrl;

    fn source_file(&self) -> Option<&AbsPath>;

    fn id(&self) -> Cow<str> {
        self.url().href()
    }
}

#[allow(dead_code)]
pub trait TeraItem {
    fn context(&self, ctx: &RenderContext) -> Context;

    fn template(&self) -> &str;

    // Just here to allow us to only implement TeraItem and rely
    // on blanket implementation for Item.
    fn tera_url(&self) -> &SiteUrl;
    fn tera_source_file(&self) -> Option<&AbsPath>;

    fn output_file(&self, output_dir: &Utf8Path) -> Utf8PathBuf {
        self.tera_url().output_file(output_dir)
    }

    fn render_to_string(&self, ctx: &RenderContext) -> Result<String> {
        let mut buf = Vec::new();
        self.render_to(ctx, &mut buf)?;
        Ok(String::from_utf8(buf)?)
    }

    fn render_to_file(&self, ctx: &RenderContext, file: &Utf8Path) -> Result<()> {
        debug!("Rendering {file}");
        self.render_to(ctx, util::create_file(file)?)
    }

    fn render_to(&self, ctx: &RenderContext, write: impl Write) -> Result<()> {
        let mut context = ctx.parent_context.clone();
        context.extend(self.context(ctx));
        if !context.contains_key("url") {
            context.insert("url", self.tera_url().href().to_string().as_str());
        }
        content::add_nav_highlight(&mut context);
        ctx.tera.render_to(self.template(), &context, write)?;
        Ok(())
    }
}

impl<T: TeraItem + Send + Sync + Debug> Item for T {
    fn render(&self, ctx: &RenderContext) -> Result<()> {
        let output_file = self.tera_url().output_file(ctx.output_dir);
        self.render_to_file(ctx, &output_file)
    }

    fn url(&self) -> &SiteUrl {
        self.tera_url()
    }

    fn source_file(&self) -> Option<&AbsPath> {
        self.tera_source_file()
    }
}
