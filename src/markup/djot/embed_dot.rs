use crate::markup::dot::generate_dot;
use crate::paths::RelPath;
use camino::Utf8PathBuf;
use jotdown::{Attributes, Container, Event, SpanLinkType};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use tracing::warn;

pub struct EmbedDot<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
    event_queue: Vec<Event<'a>>,
    embedded_files: Rc<RefCell<HashSet<RelPath>>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> EmbedDot<'a, I> {
    pub fn new(parent: I, embedded_files: Rc<RefCell<HashSet<RelPath>>>) -> Self {
        Self {
            parent,
            event_queue: vec![],
            embedded_files,
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for EmbedDot<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let (src, link_type, attrs) = match self.parent.next()? {
            Event::Start(Container::Image(src, link_type), attrs) => {
                if should_embed(&src, &link_type) {
                    (src, link_type, attrs)
                } else {
                    return Some(Event::Start(Container::Image(src, link_type), attrs));
                }
            }
            other => return Some(other),
        };

        let next = self.parent.next();
        if !matches!(next, Some(Event::End(Container::Image(_, _)))) {
            warn!("Expected image to end but got: {next:?}");
        }

        // Images typically include a leading `/` so strip it.
        let rel_src = src
            .strip_prefix('/')
            .map(String::from)
            .unwrap_or_else(|| src.to_string());

        let svg = match generate_dot(&RelPath(rel_src.clone().into())) {
            Ok(x) => x,
            Err(err) => {
                warn!("Couldn't convert dot `{src}`: {err}");
                return Some(Event::Start(Container::Image(src, link_type), attrs));
            }
        };

        self.embedded_files
            .borrow_mut()
            .insert(RelPath(Utf8PathBuf::from(rel_src.to_string())));

        let html = Container::RawBlock { format: "html" };
        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str(svg.into()));
        Some(Event::Start(html, Attributes::new()))
    }
}

fn should_embed(src: &str, link_type: &SpanLinkType) -> bool {
    if !src.ends_with(".dot") {
        return false;
    }

    if *link_type == SpanLinkType::Unresolved {
        return false;
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<String> {
        let parser = Parser::new(s);
        let embedded_files = Rc::new(RefCell::new(HashSet::new()));
        let transformed = EmbedDot::new(parser, embedded_files);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_embed_dot() -> Result<()> {
        let s = "![](/graphviz/test.dot)";
        assert_eq!(convert(s)?, r#""#);
        Ok(())
    }
}
