use crate::markup::{markdown_to_html, strip_one_paragraph};
use pulldown_cmark::escape;
use std::collections::BTreeMap;

pub struct Figure<'a> {
    pub imgs: Vec<Img>,
    pub caption: Option<String>,
    pub class: Option<&'a str>,
    pub link: bool,
}

impl<'a> Figure<'a> {
    pub fn push_html(&self, s: &mut String) {
        push_open_tag(s, "figure", self.class);
        s.push('\n');
        for img in self.imgs.iter() {
            if self.link {
                s.push_str(r#"<a href=""#);
                s.push_str(&img.src);
                s.push_str(r#"">"#);
            }
            img.push_html(s);
            if self.link {
                s.push_str("</a>");
            }
            s.push('\n');
        }

        if let Some(caption) = &self.caption {
            let caption = strip_one_paragraph(markdown_to_html(caption).html.0.into());
            s.push_str(&format!("<figcaption>{caption}</figcaption>\n"));
        }
        s.push_str("</figure>");
    }
}

pub struct Img {
    pub src: String,
    pub title: Option<String>,
    pub width: Option<String>,
    pub height: Option<String>,
}

impl Img {
    pub fn new(src: &str) -> Self {
        Self {
            src: src.into(),
            title: None,
            width: None,
            height: None,
        }
    }

    pub fn push_html(&self, s: &mut String) {
        let mut attrs = BTreeMap::new();
        attrs.insert("src", to_escape_href(&self.src));
        if let Some(title) = &self.title {
            attrs.insert("title", title.to_string());
        }
        if let Some(width) = &self.width {
            attrs.insert("width", width.to_string());
        }
        if let Some(height) = &self.height {
            attrs.insert("height", height.to_string());
        }
        push_tag(s, "img", attrs)
    }
}

pub fn push_tag<S>(s: &mut String, tag: &str, args: BTreeMap<&str, S>)
where
    S: AsRef<str>,
{
    s.push('<');
    s.push_str(tag);
    for (key, val) in args {
        let val_s = val.as_ref();
        if val_s.is_empty() {
            continue;
        }
        s.push(' ');
        s.push_str(key);
        s.push_str(r#"=""#);
        s.push_str(val.as_ref());
        s.push('"');
    }
    s.push_str(" />");
}

pub fn push_open_tag<S>(s: &mut String, tag: &str, class: Option<S>)
where
    S: AsRef<str>,
{
    s.push('<');
    s.push_str(tag);
    if let Some(class) = class {
        s.push_str(r#" class=""#);
        s.push_str(class.as_ref());
        s.push('"');
    }
    s.push('>');
}

pub fn to_escape_href(s: &str) -> String {
    let mut res = String::new();
    escape::escape_href(&mut res, s).unwrap();
    res
}
