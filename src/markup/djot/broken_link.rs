use jotdown::{Container, Event, LinkType, SpanLinkType};

use crate::markup::ParseContext;

pub struct BrokenLink<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
    context: ParseContext<'a>,
}

impl<'a, I: Iterator<Item = Event<'a>>> BrokenLink<'a, I> {
    pub fn new(parent: I, context: ParseContext<'a>) -> Self {
        Self { parent, context }
    }
}

// FIXME move to lookup register?
impl<'a, I: Iterator<Item = Event<'a>>> Iterator for BrokenLink<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.parent.next()?;

        match &next {
            Event::Start(Container::Link(target, LinkType::Span(SpanLinkType::Unresolved)), _) => {
                self.context.log_broken_link(&target);
            }
            Event::Start(Container::Image(target, SpanLinkType::Unresolved), _) => {
                self.context.log_broken_link(&target);
            }
            _ => {}
        }

        Some(next)
    }
}
