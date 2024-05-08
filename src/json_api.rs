use crate::paths::AbsPath;
use crate::server::messages::PostInfo;
use crate::{Site, SiteOptions};
use eyre::Result;

pub async fn list_posts(dir: &AbsPath) -> Result<()> {
    list_posts_or_drafts(dir, false).await
}

pub async fn list_drafts(dir: &AbsPath) -> Result<()> {
    list_posts_or_drafts(dir, true).await
}

async fn list_posts_or_drafts(dir: &AbsPath, drafts: bool) -> Result<()> {
    let site = load_site(dir, drafts)?;
    let posts: Vec<_> = site
        .content
        .posts
        .values()
        .filter(|post| post.is_draft == drafts)
        .map(PostInfo::from)
        .collect();
    let json = serde_json::to_string(&posts)?;
    println!("{}", json);
    Ok(())
}

fn load_site(dir: &AbsPath, include_drafts: bool) -> Result<Site> {
    let site = Site::load_content(SiteOptions {
        output_dir: "".into(),
        input_dir: dir.clone(),
        clear_output_dir: false,
        include_drafts,
        generate_feed: false,
        include_js: false,
        generate_markup_lookup: false,
    })?;
    Ok(site)
}
