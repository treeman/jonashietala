use jotdown::{Attributes, Container, Event};

pub struct QuoteTransforms<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> QuoteTransforms<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent,
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for QuoteTransforms<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let author = match self.parent.next()? {
            Event::Start(Container::Blockquote, attrs) => {
                if let Some(author) = attrs.get("author") {
                    author.to_string()
                } else {
                    return Some(Event::Start(Container::Blockquote, attrs));
                }
            }
            other => return Some(other),
        };

        let mut events = Vec::new();
        loop {
            match self.parent.next()? {
                // Yeah, don't support nesting for now.
                Event::End(Container::Blockquote) => {
                    break;
                }
                other => events.push(other),
            }
        }

        let html = Container::RawBlock { format: "html" };
        self.event_queue.push(Event::End(Container::Blockquote));
        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str(
            format!(
                r#"<footer><span class="author">{}</span></footer>"#,
                html_escape::encode_text(&author),
            )
            .into(),
        ));
        self.event_queue.push(Event::Start(html, Attributes::new()));
        for x in events.into_iter().rev() {
            self.event_queue.push(x);
        }
        Some(Event::Start(Container::Blockquote, Attributes::new()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<String> {
        let parser = Parser::new(s);
        let transformed = QuoteTransforms::new(parser);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_quote_src() -> Result<()> {
        let s = r#"
{author="John > Jane"}
> Text here
"#;
        assert_eq!(
            convert(s)?,
            r#"
<blockquote>
<p>Text here</p>
<footer><span class="author">John &gt; Jane</span></footer>
</blockquote>
"#
        );

        Ok(())
    }
}
