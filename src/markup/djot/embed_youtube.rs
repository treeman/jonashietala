use jotdown::{Attributes, Container, Event};
use lazy_static::lazy_static;
use regex::Regex;
use std::iter::Peekable;

pub struct EmbedYoutube<'a, I: Iterator<Item = Event<'a>>> {
    parent: Peekable<I>,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> EmbedYoutube<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.peekable(),
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for EmbedYoutube<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let start = match self.parent.next()? {
            start @ Event::Start(Container::Paragraph, _) => start,
            other => return Some(other),
        };

        let text = match self.parent.peek()? {
            Event::Str(ref text) => text,
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
        if self.parent.next() != Some(Event::End(Container::Paragraph)) {
            panic!("Bare Youtube link not ending with a paragraph");
        }

        let html = Container::RawBlock { format: "html" };
        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str(embedded.into()));
        Some(Event::Start(html, Attributes::new()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<String> {
        let parser = Parser::new(s);
        let transformed = EmbedYoutube::new(parser);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_embed_youtube() -> Result<()> {
        let s = "Before

http://www.youtube.com/watch?v=eoKDyhxCVm0

After";
        assert_eq!(
            convert(s)?.trim_end(),
            r#"<p>Before</p>

<div class="video-wrapper">
  <div class="video-container">
    <iframe src="//www.youtube.com/embed/eoKDyhxCVm0" frameborder="0" allowfullscreen="1">
    </iframe>
  </div>
</div>
<p>After</p>"#
        );

        Ok(())
    }
}
