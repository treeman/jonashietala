use crate::{content, site::SiteContent, site_url::SiteUrl};
use camino::{Utf8Path, Utf8PathBuf};
use eyre::Result;
use std::borrow::Cow;
use std::fs;
use std::fs::File;
use std::io::Write;
use tera::{Context, Tera};
use tracing::debug;

pub trait Item: Send + Sync {
    fn render(&self, ctx: &RenderContext) -> Result<()>;

    fn id(&self) -> Cow<str>;
}

pub struct RenderContext<'a> {
    pub output_dir: &'a Utf8Path,
    pub parent_context: &'a Context,
    pub content: &'a SiteContent,
    pub tera: &'a Tera,
}

pub trait TeraItem {
    fn context(&self, ctx: &RenderContext) -> Context;

    fn template(&self) -> &str;

    // Should url and output file exist for Item instead?
    fn url(&self) -> &SiteUrl;

    fn output_file(&self, output_dir: &Utf8Path) -> Utf8PathBuf {
        self.url().output_file(output_dir)
    }

    fn render_to_string(&self, ctx: &RenderContext) -> Result<String> {
        let mut buf = Vec::new();
        self.render_to(ctx, &mut buf)?;
        Ok(String::from_utf8(buf)?)
    }

    fn render_to_file(&self, ctx: &RenderContext, file: &Utf8Path) -> Result<()> {
        debug!("Rendering {file}");
        let dir = file.parent().expect("Should have a parent dir");
        fs::create_dir_all(dir)?;
        let file = File::create(file)?;
        self.render_to(ctx, file)
    }

    fn render_to(&self, ctx: &RenderContext, write: impl Write) -> Result<()> {
        let mut context = ctx.parent_context.clone();
        context.extend(self.context(ctx));
        if !context.contains_key("url") {
            context.insert("url", self.url().href().to_string().as_str());
        }
        content::add_nav_highlight(&mut context);
        ctx.tera.render_to(self.template(), &context, write)?;
        Ok(())
    }
}

impl<T: TeraItem + Send + Sync> Item for T {
    fn render(&self, ctx: &RenderContext) -> Result<()> {
        let output_file = self.url().output_file(ctx.output_dir);
        self.render_to_file(ctx, &output_file)
    }

    fn id(&self) -> Cow<str> {
        self.url().href()
    }
}
