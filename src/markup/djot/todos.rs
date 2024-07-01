use crate::markup::markup_lookup::{Element, MarkupLookup, TodoTag};
use jotdown::{Attributes, Container, Event};
use std::cell::RefCell;
use std::iter::Peekable;
use std::ops::Range;
use std::rc::Rc;

use crate::markup::ParseContext;

pub struct TransformTodoComments<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> {
    parent: Peekable<I>,
    lookup: Rc<RefCell<MarkupLookup>>,
    event_queue: Vec<(Event<'a>, Range<usize>)>,
    context: ParseContext<'a>,
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> TransformTodoComments<'a, I> {
    pub fn new(parent: I, context: ParseContext<'a>, lookup: Rc<RefCell<MarkupLookup>>) -> Self {
        Self {
            parent: parent.peekable(),
            lookup,
            event_queue: vec![],
            context,
        }
    }
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> Iterator for TransformTodoComments<'a, I> {
    type Item = (Event<'a>, Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let start = match self.parent.next()? {
            start @ (Event::Start(Container::Paragraph, _), _) => start,
            other => return Some(other),
        };

        let (text, text_range) = match self.parent.peek()? {
            (Event::Str(ref text), range) => (text.to_string(), range.clone()),
            _ => return Some(start),
        };

        let (todo, todo_len) = if let Some(t) = TodoTag::from_beginning_of_line(&text) {
            t
        } else {
            return Some(start);
        };

        self.parent.next(); // Skip the Text

        // Consume everything until paragraph end, so we can replay it later.
        let mut following = Vec::new();
        let end;
        loop {
            match self.parent.next().expect("Early end") {
                (Event::End(Container::Paragraph), range) => {
                    end = range.end;
                    break;
                }
                x => following.push(x),
            }
        }

        self.context.log_todo_comment(&text);

        self.lookup.borrow_mut().insert_element(
            Element::Todo(todo.clone()),
            text_range.start..text_range.start + todo_len,
        );

        let html = Container::RawBlock { format: "html" };

        let end_range = end..end;
        let start_range = text_range.start..text_range.start;

        self.event_queue
            .push((Event::End(Container::Paragraph), end..end));

        self.event_queue
            .push((Event::End(html.clone()), end_range.clone()));
        self.event_queue
            .push((Event::Str("</span>".into()), end_range.clone()));
        self.event_queue
            .push((Event::Start(html.clone(), Attributes::new()), end_range));

        for x in following.into_iter().rev() {
            self.event_queue.push(x);
        }
        self.event_queue.push((Event::Str(text.into()), text_range));

        self.event_queue
            .push((Event::End(html.clone()), start_range.clone()));
        self.event_queue.push((
            Event::Str(format!(r#"<span class="{}">"#, todo.class()).into()),
            start_range.clone(),
        ));
        self.event_queue
            .push((Event::Start(html, Attributes::new()), start_range));
        Some(start)
    }
}

#[cfg(test)]
mod tests {
    use super::super::drop_offset::DropOffset;
    use super::*;
    use crate::markup::markup_lookup::{Element, ElementLookup, PosRange, TodoTag};
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<(String, MarkupLookup)> {
        let lookup = Rc::new(RefCell::new(MarkupLookup::new(s, 0)));
        let parser = Parser::new(s).into_offset_iter();
        let transformed =
            TransformTodoComments::new(parser, ParseContext::default(), lookup.clone());
        let transformed = DropOffset::new(transformed);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok((body, Rc::try_unwrap(lookup).unwrap().into_inner()))
    }

    #[test]
    fn test_convert_todo() -> Result<()> {
        let s = "Before

TODO some [text](/) end

After";
        assert_eq!(
            convert(s)?.0.trim_end(),
            r#"<p>Before</p>
<p>
<span class="todo">TODO some <a href="/">text</a> end
</span></p>
<p>After</p>"#
        );

        Ok(())
    }

    #[test]
    fn test_register_todo_lookup() -> Result<()> {
        let s = "TODO todo

WIP wip [x](link)

NOTE note

INFO info

XXX xxx

FIXME fixme
";

        let lookup = convert(s)?.1;

        assert_eq!(
            lookup.element_at(0, 0),
            Some(&ElementLookup {
                element: Element::Todo(TodoTag::Todo),
                range: PosRange::new((0, 0), (0, 4)),
                char_range: 0..4,
            }),
        );
        assert_eq!(
            lookup.element_at(2, 0),
            Some(&ElementLookup {
                element: Element::Todo(TodoTag::Todo),
                range: PosRange::new((2, 0), (2, 3)),
                char_range: 11..14,
            }),
        );
        assert_eq!(
            lookup.element_at(4, 0),
            Some(&ElementLookup {
                element: Element::Todo(TodoTag::Note),
                range: PosRange::new((4, 0), (4, 4)),
                char_range: 30..34,
            }),
        );
        assert_eq!(
            lookup.element_at(6, 0),
            Some(&ElementLookup {
                element: Element::Todo(TodoTag::Note),
                range: PosRange::new((6, 0), (6, 4)),
                char_range: 41..45,
            }),
        );
        assert_eq!(
            lookup.element_at(8, 0),
            Some(&ElementLookup {
                element: Element::Todo(TodoTag::Note),
                range: PosRange::new((8, 0), (8, 3)),
                char_range: 52..55,
            }),
        );
        assert_eq!(
            lookup.element_at(10, 0),
            Some(&ElementLookup {
                element: Element::Todo(TodoTag::Fixme),
                range: PosRange::new((10, 0), (10, 5)),
                char_range: 61..66,
            }),
        );

        Ok(())
    }
}
