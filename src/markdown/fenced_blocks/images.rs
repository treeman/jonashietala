use crate::markdown::attrs::parse_ending_attrs;
use crate::markdown::html::{Figure, Img};

pub fn parse_flex(content: &str) -> String {
    let (imgs, caption) = split_imgs_caption(content);

    let class = match imgs.len() {
        1 => None,
        2 => Some("flex-50"),
        3 => Some("flex-33"),
        4 => Some("flex-25"),
        5 => Some("flex-20"),
        x => panic!("Too many images {x} in `Flex`, try `Gallery` instead"),
    };

    let mut res = String::new();
    Figure {
        imgs,
        caption,
        class,
        link: true,
    }
    .push_html(&mut res);
    res
}

pub fn parse_figure(content: &str) -> String {
    let (imgs, caption) = split_imgs_caption(content);
    let mut res = String::new();
    Figure {
        imgs,
        caption,
        class: None,
        link: false,
    }
    .push_html(&mut res);
    res
}

pub fn parse_gallery(content: &str) -> String {
    let (imgs, caption) = split_imgs_caption(content);
    let mut res = String::new();
    Figure {
        imgs,
        caption,
        class: Some("gallery"),
        link: true,
    }
    .push_html(&mut res);
    res
}

fn split_imgs_caption(content: &str) -> (Vec<Img>, Option<String>) {
    let mut in_caption = false;
    let (caption, imgs): (Vec<_>, Vec<_>) = content.trim().lines().partition(|line| {
        if line.is_empty() {
            in_caption = true;
        }
        in_caption
    });

    let caption = if caption.is_empty() {
        None
    } else {
        Some(caption.join("\n"))
    };

    let imgs: Vec<_> = imgs.into_iter().map(parse_img).collect();

    (imgs, caption)
}

fn parse_img(s: &str) -> Img {
    let attrs = parse_ending_attrs(s).expect("Failed to parse img");
    if let Some((src, attrs)) = attrs {
        Img {
            src: src.into(),
            title: attrs.key_value.get("title").map(String::to_string),
            width: attrs.key_value.get("width").map(String::to_string),
            height: attrs.key_value.get("height").map(String::to_string),
        }
    } else {
        Img::new(s)
    }
}
