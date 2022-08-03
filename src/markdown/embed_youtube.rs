use lazy_static::lazy_static;
use pulldown_cmark::{Event, Tag};
use regex::Regex;
use std::iter::Peekable;

pub struct EmbedYoutube<'a, I: Iterator<Item = Event<'a>>> {
    parent: Peekable<I>,
}

impl<'a, I: Iterator<Item = Event<'a>>> EmbedYoutube<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.peekable(),
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for EmbedYoutube<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = match self.parent.next()? {
            start @ Event::Start(Tag::Paragraph) => start,
            other => return Some(other),
        };

        let text = match self.parent.peek()? {
            Event::Text(ref text) => text,
            _ => return Some(start),
        };

        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"^https?://www\.youtube\.com/watch\?v=([A-Za-z0-9_-]+)$").unwrap();
        }

        let captures = match RE.captures(text) {
            Some(captures) => captures,
            _ => return Some(start),
        };
        let video_ref = &captures[1];
        let embedded = format!(
            r#"
<div class="video-wrapper">
  <div class="video-container">
    <iframe src="//www.youtube.com/embed/{video_ref}" frameborder="0" allowfullscreen="1">
    </iframe>
  </div>
</div>"#
        );

        // Consume the text and ending paragraph, which we should replace with the embedded video.
        self.parent.next(); // Skip the Text
        if self.parent.next() != Some(Event::End(Tag::Paragraph)) {
            panic!("Bare Youtube link not ending with a paragraph");
        }

        Some(Event::Html(embedded.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pulldown_cmark::{html, Options, Parser};

    fn convert(s: &str) -> String {
        let parser = Parser::new_ext(s, Options::all());
        let transformed = EmbedYoutube::new(parser);
        let mut body = String::new();
        html::push_html(&mut body, transformed);
        body
    }

    #[test]
    fn test_embed_youtube() {
        let s = "Before

http://www.youtube.com/watch?v=eoKDyhxCVm0

After";
        assert_eq!(
            convert(s).trim_end(),
            r#"<p>Before</p>

<div class="video-wrapper">
  <div class="video-container">
    <iframe src="//www.youtube.com/embed/eoKDyhxCVm0" frameborder="0" allowfullscreen="1">
    </iframe>
  </div>
</div>
<p>After</p>"#
        );
    }
}
