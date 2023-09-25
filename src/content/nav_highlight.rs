use tera::{Context, Value};

/// Add navigation highlight tag depending on the current context
pub fn add_nav_highlight(ctx: &mut Context) {
    ctx.insert("nav_highlight", decide_highlight(ctx));
}

fn decide_highlight(ctx: &Context) -> &'static str {
    match ctx.get("series") {
        Some(Value::Null) => {}
        Some(_) => return "series",
        _ => {}
    }

    if let Some(Value::String(url)) = ctx.get("url") {
        if url == "/" {
            // Homepage, what should we highlight?
            return "projects";
        } else if url.starts_with("/series") {
            return "series";
        } else if url.starts_with("/blog") {
            return "archive";
        } else if url.starts_with("/drafts") {
            return "drafts";
        } else if url.starts_with("/gpg") {
            return "contact";
        } else if url.starts_with("/projects") {
            return "projects";
        }
    }

    // Dunno, default to projects?
    return "projects";
}
