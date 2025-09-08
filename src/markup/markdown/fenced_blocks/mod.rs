mod images;

use lazy_static::lazy_static;
use regex::{Captures, Regex};
use std::borrow::Cow;

lazy_static! {
    static ref BLOCK: Regex = Regex::new(
        r#"(?xsm)
        ^
        # Opening :::
        :{3}
        \s+
        # Parsing id type
        (?P<id>\w+)
        \s*
        $

        # Content inside
        (?P<content>.+?)

        # Ending :::
        ^:::$
        "#
    )
    .unwrap();
}

pub fn parse_fenced_blocks(s: &str) -> Cow<'_, str> {
    BLOCK.replace_all(s, |caps: &Captures| -> String {
        parse_block(
            caps.name("id").unwrap().as_str(),
            caps.name("content").unwrap().as_str(),
        )
    })
}

fn parse_block(id: &str, content: &str) -> String {
    let t = BlockType::new(id);
    t.parse(content)
}

enum BlockType {
    Flex,
    Figure,
    Gallery,
}

impl BlockType {
    fn new(id: &str) -> Self {
        match id {
            "Flex" => BlockType::Flex,
            "Figure" => BlockType::Figure,
            "Gallery" => BlockType::Gallery,
            _ => panic!("Unknown block type `{id}`"),
        }
    }

    fn parse(&self, content: &str) -> String {
        match self {
            BlockType::Flex => images::parse_flex(content),
            BlockType::Figure => images::parse_figure(content),
            BlockType::Gallery => images::parse_gallery(content),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_flex_simple() {
        let s = r#"
::: Flex
/images/img1.jpg
/images/img2.jpg
:::"#;

        assert_eq!(
            parse_fenced_blocks(s),
            r#"
<figure class="flex-50">
<a href="/images/img1.jpg"><img src="/images/img1.jpg" /></a>
<a href="/images/img2.jpg"><img src="/images/img2.jpg" /></a>
</figure>"#
        );
    }

    #[test]
    fn test_parse_flex_attrs() {
        let s = r#"
::: Flex
/images/img1.jpg { height=100 }
/images/img2.jpg { height=100 }
:::"#;

        assert_eq!(
            parse_fenced_blocks(s),
            r#"
<figure class="flex-50">
<a href="/images/img1.jpg"><img height="100" src="/images/img1.jpg" /></a>
<a href="/images/img2.jpg"><img height="100" src="/images/img2.jpg" /></a>
</figure>"#
        );
    }

    #[test]
    fn test_parse_flex_title() {
        let s = r#"
::: Flex
/images/img1.jpg
/images/img2.jpg

My *title*
:::"#;

        assert_eq!(
            parse_fenced_blocks(s),
            r#"
<figure class="flex-50">
<a href="/images/img1.jpg"><img src="/images/img1.jpg" /></a>
<a href="/images/img2.jpg"><img src="/images/img2.jpg" /></a>
<figcaption>My <em>title</em></figcaption>
</figure>"#
        );
    }

    #[test]
    fn test_parse_gallery() {
        let s = r#"
::: Gallery
/images/img1.jpg
/images/img2.jpg
:::"#;

        assert_eq!(
            parse_fenced_blocks(s),
            r#"
<figure class="gallery">
<a href="/images/img1.jpg"><img src="/images/img1.jpg" /></a>
<a href="/images/img2.jpg"><img src="/images/img2.jpg" /></a>
</figure>"#
        );
    }
}
