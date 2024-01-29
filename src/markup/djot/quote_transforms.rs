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
        let mut attributes = Vec::new();

        loop {
            match self.parent.next()? {
                Event::End(Container::Blockquote) => break,
                Event::Str(s) => match AUTHOR.captures(&s) {
                    Some(captures) => {
                        attributes.push(Event::Start(html.clone(), Attributes::new()));
                        attributes.push(Event::Str(r#"<footer><span class="author">"#.into()));
                        attributes.push(Event::End(html.clone()));

                        attributes.push(Event::Str(captures[1].to_owned().into()));

                        attributes.push(Event::Start(html.clone(), Attributes::new()));
                        attributes.push(Event::Str(r#"</span></footer>"#.into()));
                        attributes.push(Event::End(html.clone()));
                    }
                    _ => events.push(Event::Str(s)),
                },
                other => events.push(other),
            }
        }

        self.event_queue.push(Event::End(Container::Blockquote));
        for x in attributes.into_iter().rev() {
            self.event_queue.push(x);
        }
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
        let parser = Parser::new(s).map(|x| dbg!(x));
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
}
