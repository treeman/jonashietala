use itertools::{Itertools, MultiPeek};
use jotdown::{Attributes, Container, Event};

pub struct AutoFigures<'a, I: Iterator<Item = Event<'a>>> {
    parent: MultiPeek<I>,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> AutoFigures<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.multipeek(),
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for AutoFigures<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        // Should only transform a Start(Paragraph), Start(Img), ..., End(Img), End(Paragraph)
        let start = match self.parent.next()? {
            start @ Event::Start(Container::Paragraph, _) => start,
            other => return Some(other),
        };

        match self.parent.peek()? {
            Event::Start(Container::Image(_, _), _) => {}
            _ => return Some(start),
        };

        loop {
            if let Event::End(Container::Image(_, _)) = self.parent.peek()? {
                break;
            }
        }

        match self.parent.peek()? {
            Event::End(Container::Paragraph) => {}
            _ => return Some(start),
        }

        // Now we can eat it all up.
        // The starting paragraph has already been discarded.
        let (img, img_attrs) = if let Event::Start(img, attrs) = self.parent.next()? {
            (img, attrs)
        } else {
            panic!("Should have a start image tag");
        };

        // Caption comes before image tag end.
        let mut caption_events = Vec::new();
        loop {
            match self.parent.next()? {
                Event::End(Container::Image(_, _)) => break,
                event => caption_events.push(event),
            }
        }
        let has_caption = !caption_events.is_empty();

        // Eat until the ending paragraph
        if !matches!(self.parent.next()?, Event::End(Container::Paragraph)) {
            panic!("Should have an end paragraph tag");
        };

        // Producing output is a bit verbose as we need to add a start/end event
        // for every html tag we have.
        // But keep using original djot events to allow proper link tag expansion.
        let html = Container::RawBlock { format: "html" };
        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str("</figure>".into()));
        if has_caption {
            self.event_queue.push(Event::Str("</figcaption>".into()));
        }
        self.event_queue
            .push(Event::Start(html.clone(), Attributes::new()));

        for x in caption_events.into_iter().rev() {
            self.event_queue.push(x);
        }

        if has_caption {
            self.event_queue.push(Event::End(html.clone()));
            self.event_queue.push(Event::Str("<figcaption>".into()));
            self.event_queue
                .push(Event::Start(html.clone(), Attributes::new()));
        }

        self.event_queue.push(Event::End(img.clone()));
        self.event_queue.push(Event::Start(img.clone(), img_attrs));

        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str("<figure>".into()));
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
        let transformed = AutoFigures::new(parser);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_auto_figures() -> Result<()> {
        let s = "![](/images/img.png)";
        assert_eq!(
            convert(s)?,
            r#"<figure><img alt="" src="/images/img.png">
</figure>
"#
        );

        Ok(())
    }

    #[test]
    fn test_auto_figures_title() -> Result<()> {
        let s = "![My *title*](/images/img.png)";
        assert_eq!(
            convert(s)?,
            r#"<figure><img alt="" src="/images/img.png">
<figcaption>My <strong>title</strong>
</figcaption></figure>
"#
        );

        Ok(())
    }
}
