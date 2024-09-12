use crate::content::{load_partial_posts, PartialPostItem, PartialStandaloneItem};
use crate::markup;
use crate::paths::AbsPath;
use crate::server::complete::PostInfo;
use eyre::Result;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

pub async fn list_posts(dir: &AbsPath) -> Result<()> {
    display_posts_or_drafts(dir, dir.join("posts")).await
}

pub async fn list_drafts(dir: &AbsPath) -> Result<()> {
    display_posts_or_drafts(dir, dir.join("drafts")).await
}

async fn display_posts_or_drafts(base: &AbsPath, dir: AbsPath) -> Result<()> {
    let posts: Vec<_> = load_partial_posts(base, &dir)?
        .into_iter()
        .map(PostInfo::from)
        .collect();
    let json = serde_json::to_string(&posts)?;
    println!("{}", json);
    Ok(())
}

pub async fn list_markup_content(base: &AbsPath) -> Result<()> {
    let posts = markup::find_markup_files(base, &[base.join("posts"), base.join("drafts")])
        .par_iter()
        .map(PartialPostItem::try_from)
        .collect::<Result<Vec<_>>>()?;
    // .map(PostInfo::from)
    // .map(ContentInfo::from);

    let standalones = markup::find_markup_files(base, &[base.join("standalone")])
        .par_iter()
        .map(PartialStandaloneItem::try_from)
        .collect::<Result<Vec<_>>>()?;
    // dbg!(standalones);

    let series = markup::find_markup_files(base, &[base.join("series")]);

    // let games
    // let projects

    // for path in find_markup_files(&context.opts.input_dir, &[dir]).into_iter() {
    //     let item = StandaloneItem::from_file(&path, context)?;
    //     if !item.is_draft || context.opts.include_drafts {
    //         res.insert(item);
    //     }
    // }
    Ok(())
}
