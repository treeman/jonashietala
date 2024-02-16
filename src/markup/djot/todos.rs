use jotdown::{Attributes, Container, Event};
use std::iter::Peekable;

use crate::markup::ParseContext;

pub struct TransformTodoComments<'a, I: Iterator<Item = Event<'a>>> {
    parent: Peekable<I>,
    event_queue: Vec<Event<'a>>,
    context: ParseContext<'a>,
}

impl<'a, I: Iterator<Item = Event<'a>>> TransformTodoComments<'a, I> {
    pub fn new(parent: I, context: ParseContext<'a>) -> Self {
        Self {
            parent: parent.peekable(),
            event_queue: vec![],
            context,
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for TransformTodoComments<'a, I> {
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

        let text = text.to_owned();

        let class = if text.starts_with("TODO") {
            "todo"
        } else if text.starts_with("FIXME") {
            "fixme"
        } else if text.starts_with("NOTE") {
            "note"
        } else {
            return Some(start);
        };

        // Consume the text and ending paragraph, which we should replace with the div.
        self.parent.next(); // Skip the Text
        if self.parent.next() != Some(Event::End(Container::Paragraph)) {
            panic!("Bare {text} not ending with a paragraph");
        }

        self.context.log_todo_comment(&text);

        let html = Container::RawBlock { format: "html" };
        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str(
            format!(r#"<div><span class="{class}">{text}</span></div>"#).into(),
        ));
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
        let transformed = TransformTodoComments::new(parser, ParseContext::default());
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_convert_todo() -> Result<()> {
        let s = "Before

TODO

After";
        assert_eq!(
            convert(s)?.trim_end(),
            r#"<p>Before</p>

<div class="todo">
    TODO
</div>
<p>After</p>"#
        );

        Ok(())
    }
}
