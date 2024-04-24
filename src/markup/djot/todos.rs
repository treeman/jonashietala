use crate::markup::markup_lookup::{Element, MarkupLookup, TodoType, Todoish};
use jotdown::{Attributes, Container, Event};
use std::cell::RefCell;
use std::iter::Peekable;
use std::ops::Range;
use std::rc::Rc;

use crate::markup::ParseContext;

pub struct TransformTodoComments<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> {
    parent: Peekable<I>,
    lookup: Option<Rc<RefCell<MarkupLookup>>>,
    event_queue: Vec<(Event<'a>, Range<usize>)>,
    context: ParseContext<'a>,
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> TransformTodoComments<'a, I> {
    pub fn new(
        parent: I,
        context: ParseContext<'a>,
        lookup: Option<Rc<RefCell<MarkupLookup>>>,
    ) -> Self {
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

        let (todo, rest) = if let Some(t) = TodoType::from_beginning_of_line(&text) {
            t
        } else {
            return Some(start);
        };
        dbg!(&todo);
        dbg!(&rest);

        self.context.log_todo_comment(&text);

        let todo_range = text_range.start..todo.class().len();

        if let Some(ref lookup) = self.lookup {
            lookup.borrow_mut().insert_element(
                Element::Todoish(Todoish { t: todo.clone() }),
                todo_range.clone(),
            );
        }

        self.parent.next(); // Skip the Text
        self.event_queue
            .push((Event::Str(rest.to_owned().into()), text_range)); // FIXME adjust range
        self.event_queue.push(start); // FIXME adjust range
        let html = Container::RawBlock { format: "html" };
        self.event_queue
            .push((Event::End(html.clone()), todo_range.end..todo_range.end));
        self.event_queue.push((
            Event::Str(
                format!(r#"<div><span class="{}">{text}</span></div>"#, todo.class()).into(),
            ),
            todo_range.clone(),
        ));
        Some((
            Event::Start(html, Attributes::new()),
            todo_range.start..todo_range.start,
        ))

        // Consume the text and ending paragraph, which we should replace with the div.
        // self.parent.next(); // Skip the Text
        // let end_range = if let Some((Event::End(Container::Paragraph), range)) = self.parent.next()
        // {
        //     range.clone()
        // } else {
        //     panic!("Bare {text} not ending with a paragraph");
        // };

        // if let Some(ref lookup) = self.lookup {
        //     lookup.borrow_mut().insert_element(
        //         Element::Todoish(Todoish { t: t.clone() }),
        //         text_range.start..t.class().len(),
        //     );
        // }

        // self.context.log_todo_comment(&text);

        // let html = Container::RawBlock { format: "html" };
        // self.event_queue
        //     .push((Event::End(html.clone()), end_range.clone()));
        // self.event_queue.push((
        //     Event::Str(format!(r#"<div><span class="{}">{text}</span></div>"#, t.class()).into()),
        //     end_range.clone(),
        // ));
        // Some((Event::Start(html, Attributes::new()), end_range.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::super::drop_offset::DropOffset;
    use super::*;
    use crate::markup::markup_lookup::{Element, ElementLookup, PosRange, TodoType, Todoish};
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<(String, MarkupLookup)> {
        let lookup = Rc::new(RefCell::new(MarkupLookup::new(s, 0)));
        let parser = Parser::new(s).into_offset_iter();
        let transformed =
            TransformTodoComments::new(parser, ParseContext::default(), Some(lookup.clone()));
        let transformed = DropOffset::new(transformed);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok((body, Rc::try_unwrap(lookup).unwrap().into_inner()))
    }

    #[test]
    fn test_convert_todo() -> Result<()> {
        let s = "Before

TODO some text

After";
        assert_eq!(
            convert(s)?.0.trim_end(),
            r#"<p>Before</p>
<div><span class="todo">TODO some text</span></div>
<p>After</p>"#
        );

        Ok(())
    }

    #[test]
    fn test_register_todo_lookup() -> Result<()> {
        let s = "TODO todo

WIP wip

NOTE note

INFO info

XXX xxx

FIXME fixme
";

        let lookup = convert(s)?.1;

        assert_eq!(
            lookup.element_at(0, 0),
            Some(&ElementLookup {
                element: Element::Todoish(Todoish { t: TodoType::Todo }),
                range: PosRange::new((0, 0), (0, 3)),
                char_range: 0..3,
            }),
        );
        assert_eq!(
            lookup.element_at(2, 0),
            Some(&ElementLookup {
                element: Element::Todoish(Todoish { t: TodoType::Todo }),
                range: PosRange::new((1, 0), (1, 4)),
                char_range: 0..3,
            }),
        );
        assert_eq!(
            lookup.element_at(4, 0),
            Some(&ElementLookup {
                element: Element::Todoish(Todoish { t: TodoType::Note }),
                range: PosRange::new((1, 0), (1, 4)),
                char_range: 0..3,
            }),
        );
        assert_eq!(
            lookup.element_at(6, 0),
            Some(&ElementLookup {
                element: Element::Todoish(Todoish { t: TodoType::Note }),
                range: PosRange::new((1, 0), (1, 4)),
                char_range: 0..3,
            }),
        );
        assert_eq!(
            lookup.element_at(8, 0),
            Some(&ElementLookup {
                element: Element::Todoish(Todoish { t: TodoType::Note }),
                range: PosRange::new((1, 0), (1, 4)),
                char_range: 0..3,
            }),
        );
        assert_eq!(
            lookup.element_at(10, 0),
            Some(&ElementLookup {
                element: Element::Todoish(Todoish { t: TodoType::Fixme }),
                range: PosRange::new((1, 0), (1, 4)),
                char_range: 0..3,
            }),
        );

        Ok(())
    }
}
