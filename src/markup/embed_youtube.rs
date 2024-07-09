use crate::util;
use camino::{Utf8Path, Utf8PathBuf};
use eyre::Result;
use lazy_static::lazy_static;
use regex::Regex;
use tracing::info;

pub fn embed_yt(line: &str, embed_thumbnail: bool) -> Result<Option<String>> {
    if embed_thumbnail {
        embed_yt_thumbnail(line)
    } else {
        Ok(linkify_yt_link(line))
    }
}

pub fn linkify_yt_link(line: &str) -> Option<String> {
    let caps = match RE.captures(line) {
        Some(captures) => captures,
        _ => return None,
    };
    let video_ref = &caps[1];
    let yt_url = format!("https://www.youtube.com/watch?v={video_ref}");

    let html = format!(r#"<a href="{yt_url}">{yt_url}</a>"#);
    Some(html)
}

pub fn embed_yt_thumbnail(line: &str) -> Result<Option<String>> {
    let caps = match RE.captures(line) {
        Some(captures) => captures,
        _ => return Ok(None),
    };
    let video_ref = &caps[1];

    let img_path = Utf8PathBuf::from(format!("images/yt-thumbnails/{video_ref}.jpg"));
    download_thumbnail(&img_path, video_ref)?;

    let yt_url = format!("https://www.youtube.com/watch?v={video_ref}");

    let html = format!(
        r##"<div class="yt-wrapper" id="{video_ref}">
      <div class="yt-container">
        <a href="{yt_url}">
          <img src="/{img_path}" />
          <span class="yt-overlay">
            <span>Click to view on YouTube</span>
          </span>
<svg height="100%" version="1.1" viewBox="0 0 68 48" width="100%" class="play-button"><path class="background" d="M66.52,7.74c-0.78-2.93-2.49-5.41-5.42-6.19C55.79,.13,34,0,34,0S12.21,.13,6.9,1.55 C3.97,2.33,2.27,4.81,1.48,7.74C0.06,13.05,0,24,0,24s0.06,10.95,1.48,16.26c0.78,2.93,2.49,5.41,5.42,6.19 C12.21,47.87,34,48,34,48s21.79-0.13,27.1-1.55c2.93-0.78,4.64-3.26,5.42-6.19C67.94,34.95,68,24,68,24S67.94,13.05,66.52,7.74z"></path><path class="arrow" d="M 45,24 27,14 27,34"></path></svg>
        </a>
      </div>
    </div>"##
    );

    Ok(Some(html))
}

fn download_thumbnail(dest: &Utf8Path, video_ref: &str) -> Result<()> {
    if dest.exists() {
        return Ok(());
    }

    info!("Generating yt thumbnail: {video_ref}");

    let img_url = format!("http://img.youtube.com/vi/{video_ref}/hqdefault.jpg");
    let resp = reqwest::blocking::get(img_url)?;
    let body = resp.bytes()?;
    util::write_to_file(dest, body.as_ref())?;

    Ok(())
}

lazy_static! {
    static ref RE: Regex =
        Regex::new(r"^https?://www\.youtube\.com/watch\?v=([A-Za-z0-9_-]+)$").unwrap();
}
