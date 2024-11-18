use camino::Utf8PathBuf;
use jotdown::{Attributes, Container, Event, SpanLinkType};
use std::fs;
use tracing::warn;

pub struct EmbedSvg<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> EmbedSvg<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent,
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for EmbedSvg<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let src = match self.parent.next()? {
            Event::Start(Container::Image(src, link_type), attrs) => {
                if should_embed(&src, &link_type, &attrs) {
                    src
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
        let src = src
            .strip_prefix('/')
            .map(String::from)
            .unwrap_or_else(|| src.to_string());

        let path = Utf8PathBuf::from(src);
        let embedded = fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("Should be able to read file `{path}"));

        let html = Container::RawBlock { format: "html" };
        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str(embedded.into()));
        Some(Event::Start(html, Attributes::new()))
    }
}

fn should_embed(src: &str, link_type: &SpanLinkType, attrs: &Attributes<'_>) -> bool {
    if !src.ends_with(".svg") {
        return false;
    }

    if *link_type == SpanLinkType::Unresolved {
        return false;
    }

    if let Some(embed) = attrs.get("embed").map(|x| x.to_string()) {
        return embed == "true";
    }

    false
}
