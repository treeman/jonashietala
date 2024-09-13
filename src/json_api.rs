use crate::content::ContentInfo;
use crate::markup;
use crate::paths::AbsPath;
use crate::paths::FilePath;
use eyre::Result;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

pub async fn list_markup_content(base: &AbsPath) -> Result<()> {
    let mut all_files = markup::find_markup_files(
        base,
        &[
            base.join("posts"),
            base.join("drafts"),
            base.join("projects"),
            base.join("standalone"),
            base.join("series"),
        ],
    );
    all_files.push(FilePath::from_path(base, base.join("projects.dj"))?);

    let content = all_files
        .par_iter()
        .map(ContentInfo::try_from)
        .collect::<Result<Vec<ContentInfo>>>()?;

    println!("{}", serde_json::to_string(&content)?);
    Ok(())
}
