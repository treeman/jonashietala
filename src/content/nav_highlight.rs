use tera::{Context, Value};

/// Add navigation highlight tag depending on the current context
pub fn add_nav_highlight(ctx: &mut Context) {
    ctx.insert("nav_highlight", decide_highlight(ctx));
}

fn decide_highlight(ctx: &Context) -> &'static str {
    let url = ctx.get("url");

    if let Some(Value::String(url)) = url {
        if url == "/" {
            return "";
        }
    }

    match ctx.get("series") {
        Some(Value::Null) => {}
        Some(_) => return "series",
        _ => {}
    }

    if let Some(Value::String(url)) = url {
        if url.starts_with("/series") {
            return "series";
        } else if url.starts_with("/about") {
            return "about";
        } else if url.starts_with("/blog") || url.starts_with("/favorite") {
            return "archive";
        } else if url.starts_with("/drafts") {
            return "drafts";
        } else if url.starts_with("/gpg") {
            return "contact";
        } else if url.starts_with("/projects") {
            return "projects";
        }
    }

    ""
}
