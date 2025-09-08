use crate::markup::embed_youtube::embed_yt;
use jotdown::{Attributes, Container, Event};
use std::iter::Peekable;

pub struct EmbedYoutube<'a, I: Iterator<Item = Event<'a>>> {
    parent: Peekable<I>,
    event_queue: Vec<Event<'a>>,
    embed_thumbnail: bool,
}

impl<'a, I: Iterator<Item = Event<'a>>> EmbedYoutube<'a, I> {
    pub fn new(parent: I, embed_thumbnail: bool) -> Self {
        Self {
            parent: parent.peekable(),
            event_queue: vec![],
            embed_thumbnail,
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
            Event::Str(text) => text,
            _ => return Some(start),
        };

        let embedded = match embed_yt(text.as_ref(), self.embed_thumbnail)
            .expect("Failed to process youtube link")
        {
            Some(x) => x,
            None => return Some(start),
        };

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
    use jotdown::{Parser, Render, html};

    fn convert(s: &str, embed_thumbnail: bool) -> Result<String> {
        let parser = Parser::new(s);
        let transformed = EmbedYoutube::new(parser, embed_thumbnail);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_embed_youtube_thumbnail() -> Result<()> {
        let s = "Before

http://www.youtube.com/watch?v=eoKDyhxCVm0

After";
        assert_eq!(
            convert(s, true)?.trim_end(),
            "<p>Before</p>\n<div class=\"yt-wrapper\" id=\"eoKDyhxCVm0\">\n      <div class=\"yt-container\">\n        <a href=\"https://www.youtube.com/watch?v=eoKDyhxCVm0\">\n          <img src=\"/images/yt-thumbnails/eoKDyhxCVm0.jpg\" />\n          <span class=\"yt-overlay\">\n            <span>Click to view on YouTube</span>\n          </span>\n<svg height=\"100%\" version=\"1.1\" viewBox=\"0 0 68 48\" width=\"100%\" class=\"play-button\"><path class=\"background\" d=\"M66.52,7.74c-0.78-2.93-2.49-5.41-5.42-6.19C55.79,.13,34,0,34,0S12.21,.13,6.9,1.55 C3.97,2.33,2.27,4.81,1.48,7.74C0.06,13.05,0,24,0,24s0.06,10.95,1.48,16.26c0.78,2.93,2.49,5.41,5.42,6.19 C12.21,47.87,34,48,34,48s21.79-0.13,27.1-1.55c2.93-0.78,4.64-3.26,5.42-6.19C67.94,34.95,68,24,68,24S67.94,13.05,66.52,7.74z\"></path><path class=\"arrow\" d=\"M 45,24 27,14 27,34\"></path></svg>\n        </a>\n      </div>\n    </div>\n<p>After</p>"
        );

        Ok(())
    }

    #[test]
    fn test_embed_youtube_link() -> Result<()> {
        let s = "Before

http://www.youtube.com/watch?v=eoKDyhxCVm0

After";
        assert_eq!(
            convert(s, false)?.trim_end(),
            "<p>Before</p>\n<a href=\"https://www.youtube.com/watch?v=eoKDyhxCVm0\">https://www.youtube.com/watch?v=eoKDyhxCVm0</a>\n<p>After</p>"
        );

        Ok(())
    }
}
