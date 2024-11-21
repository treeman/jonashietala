use camino::Utf8PathBuf;
use jotdown::{Attributes, Container, Event, SpanLinkType};
use std::collections::HashSet;
use std::fs;
use tracing::warn;

use crate::paths::RelPath;

pub struct EmbedSvg<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
    event_queue: Vec<Event<'a>>,
    embedded_files: &'a mut HashSet<RelPath>,
}

impl<'a, I: Iterator<Item = Event<'a>>> EmbedSvg<'a, I> {
    pub fn new(parent: I, embedded_files: &'a mut HashSet<RelPath>) -> Self {
        Self {
            parent,
            event_queue: vec![],
            embedded_files,
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for EmbedSvg<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let (src, link_type, attrs) = match self.parent.next()? {
            Event::Start(Container::Image(src, link_type), attrs) => {
                if should_embed(&src, &link_type, &attrs) {
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

        self.embedded_files
            .insert(RelPath(Utf8PathBuf::from(rel_src.to_string())));

        let path = Utf8PathBuf::from(rel_src);
        match fs::read_to_string(&path) {
            Ok(embedded) => {
                let html = Container::RawBlock { format: "html" };
                self.event_queue.push(Event::End(html.clone()));
                self.event_queue.push(Event::Str(embedded.into()));
                Some(Event::Start(html, Attributes::new()))
            }
            Err(err) => {
                warn!("Couldn't embed image `{src}`: {err}");
                Some(Event::Start(Container::Image(src, link_type), attrs))
            }
        }
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
