use crate::util;
use pulldown_cmark::{html::push_html, Event, HeadingLevel, Tag, TagEnd};
use std::fmt::Write;

pub struct TransformHeaders<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
}

impl<'a, I: Iterator<Item = Event<'a>>> TransformHeaders<'a, I> {
    pub fn new(parent: I) -> Self {
        Self { parent }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for TransformHeaders<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (level, id, classes) = match self.parent.next()? {
            Event::Start(Tag::Heading {
                level, id, classes, ..
            }) => (level, id, classes),
            other => return Some(other),
        };

        let mut events = Vec::new();
        loop {
            match self.parent.next()? {
                Event::End(TagEnd::Heading(_)) => break,
                event => events.push(event),
            }
        }

        let mut inner_html = String::new();
        push_html(&mut inner_html, events.iter().cloned());

        let id = match id {
            Some(id) => id.to_string(),
            None => util::to_id(&util::html_text(&inner_html)),
        };

        let demoted_level = level.demote();

        let mut res = String::new();
        write!(res, r#"<{demoted_level} id="{id}""#).unwrap();
        if !classes.is_empty() {
            res.push_str(r#" class=""#);
            res.push_str(&classes.join(" "));
            res.push('"');
        }
        res.push('>');
        write!(res, r##"<a class="heading-ref" href="#{id}">"##).unwrap();
        res.push_str(&inner_html);
        res.push_str("</a>");
        write!(res, "</{demoted_level}>").unwrap();

        Some(Event::Html(res.into()))
    }
}

trait DemoteHeader {
    fn demote(self) -> HeadingLevel;
}

impl DemoteHeader for HeadingLevel {
    fn demote(self) -> HeadingLevel {
        match self {
            HeadingLevel::H1 => HeadingLevel::H2,
            HeadingLevel::H2 => HeadingLevel::H3,
            HeadingLevel::H3 => HeadingLevel::H4,
            HeadingLevel::H4 => HeadingLevel::H5,
            HeadingLevel::H5 => HeadingLevel::H6,
            HeadingLevel::H6 => panic!("Cannot demote level 6 header"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pulldown_cmark::{html, Options, Parser};

    fn convert(s: &str) -> String {
        let parser = Parser::new_ext(s, Options::all());
        let transformed = TransformHeaders::new(parser);
        let mut body = String::new();
        html::push_html(&mut body, transformed);
        body
    }

    #[test]
    fn test_transform_headers() {
        let s = "# Header 1";
        assert_eq!(
            convert(s),
            r##"<h2 id="header-1"><a class="heading-ref" href="#header-1">Header 1</a></h2>"##
        );

        let s = "## Header 2";
        assert_eq!(
            convert(s),
            r##"<h3 id="header-2"><a class="heading-ref" href="#header-2">Header 2</a></h3>"##
        );

        let s = "## With [link](#x)";
        assert_eq!(
            convert(s),
            r##"<h3 id="with-link"><a class="heading-ref" href="#with-link">With <a href="#x">link</a></a></h3>"##
        );

        let s = "## Manual { #my-id }";
        assert_eq!(
            convert(s),
            r##"<h3 id="my-id"><a class="heading-ref" href="#my-id">Manual</a></h3>"##
        );

        let s = "# Head { #id .class }";
        assert_eq!(
            convert(s),
            r##"<h2 id="id" class="class"><a class="heading-ref" href="#id">Head</a></h2>"##
        );
    }
}
