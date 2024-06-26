use crate::content::load_partial_posts;
use crate::paths::AbsPath;
use crate::server::complete::PostInfo;
use eyre::Result;

pub async fn list_posts(dir: &AbsPath) -> Result<()> {
    list_posts_or_drafts(dir, dir.join("posts")).await
}

pub async fn list_drafts(dir: &AbsPath) -> Result<()> {
    list_posts_or_drafts(dir, dir.join("drafts")).await
}

async fn list_posts_or_drafts(base: &AbsPath, dir: AbsPath) -> Result<()> {
    let posts: Vec<_> = load_partial_posts(base, &dir)?
        .into_iter()
        .map(PostInfo::from)
        .collect();
    let json = serde_json::to_string(&posts)?;
    println!("{}", json);
    Ok(())
}
