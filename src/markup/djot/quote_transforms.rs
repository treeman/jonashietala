use jotdown::{Attributes, Container, Event};
use lazy_static::lazy_static;
use regex::Regex;

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

        let start = match self.parent.next()? {
            start @ Event::Start(Container::Blockquote, _) => start,
            other => return Some(other),
        };

        lazy_static! {
            static ref AUTHOR: Regex = Regex::new(r"^\^\s(.+)").unwrap();
        }
        let html = Container::RawBlock { format: "html" };

        let mut events = Vec::new();
        let mut author = Vec::new();
        let mut in_author = false;

        loop {
            match self.parent.next()? {
                Event::End(Container::Blockquote) => break,
                Event::Str(s) => match AUTHOR.captures(&s) {
                    Some(captures) => {
                        in_author = true;
                        author.push(Event::Str(captures[1].to_owned().into()));
                    }
                    _ => {
                        let e = Event::Str(s);
                        if in_author {
                            author.push(e);
                        } else {
                            events.push(e);
                        }
                    }
                },
                other @ Event::End(Container::Paragraph) => {
                    if in_author {
                        in_author = false;
                    }
                    events.push(other);
                }
                other => {
                    if in_author {
                        author.push(other);
                    } else {
                        events.push(other);
                    }
                }
            }
        }

        self.event_queue.push(Event::End(Container::Blockquote));

        self.event_queue.push(Event::End(html.clone()));
        self.event_queue
            .push(Event::Str(r#"</span></footer>"#.into()));
        self.event_queue
            .push(Event::Start(html.clone(), Attributes::new()));

        for x in author.into_iter().rev() {
            self.event_queue.push(x);
        }

        self.event_queue.push(Event::End(html.clone()));
        self.event_queue
            .push(Event::Str(r#"<footer><span class="author">"#.into()));
        self.event_queue
            .push(Event::Start(html.clone(), Attributes::new()));

        for x in events.into_iter().rev() {
            self.event_queue.push(x);
        }
        Some(start)
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
> Text here
> ^ John > Jane
"#;
        assert_eq!(
            convert(s)?,
            r#"
<blockquote>
<p>Text here
</p>
<footer><span class="author">John &gt; Jane
</span></footer>
</blockquote>
"#
        );

        Ok(())
    }

    #[test]
    fn test_quote_src_with_link() -> Result<()> {
        let s = r#"
> Text here
> ^ Memoires [John Doe](#my-link), by AI
"#;
        assert_eq!(
            convert(s)?,
            r##"
<blockquote>
<p>Text here
</p>
<footer><span class="author">Memoires <a href="#my-link">John Doe</a>, by AI
</span></footer>
</blockquote>
"##
        );

        Ok(())
    }
}
