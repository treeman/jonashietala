use crate::markup::markup_lookup::{ElementInfo, Heading, Link, LinkDef, LinkRef};
use crate::markup::MarkupLookup;
use jotdown::{Container, Event, LinkType, SpanLinkType};
use std::ops::Range;

pub struct LookupRegister<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> {
    parent: I,
    lookup: Option<&'a mut MarkupLookup>,
    event_stack: Vec<ElementInfo>,
    src: &'a str,
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> LookupRegister<'a, I> {
    pub fn new(parent: I, src: &'a str, lookup: Option<&'a mut MarkupLookup>) -> Self {
        Self {
            parent,
            lookup,
            event_stack: Vec::new(),
            src,
        }
    }
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> Iterator for LookupRegister<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.parent.next()?;

        let lookup = match &mut self.lookup {
            Some(lookup) => lookup,
            None => return Some(next.0),
        };

        match &next {
            (Event::Start(Container::Heading { id, .. }, _), range) => {
                self.event_stack.push(ElementInfo::Heading(Heading {
                    range: range.clone(),
                    id: id.to_string(),
                    content: "".into(),
                }))
            }
            (Event::End(Container::Heading { .. }), range) => {
                if let Some(ElementInfo::Heading(mut heading)) = self.event_stack.pop() {
                    // Content is between start and end.
                    heading.content = self.src[heading.range.end..range.start].trim().to_owned();

                    heading.range.end = range.end;
                    lookup.insert_heading(heading);
                }
            }
            (Event::Start(Container::Link(tag, link_type), _), range) => match link_type {
                LinkType::Span(SpanLinkType::Inline) => {
                    self.event_stack.push(ElementInfo::Link(Link {
                        link_ref: LinkRef::Inline(tag.to_string()),
                        range: range.clone(),
                    }));
                }
                LinkType::Span(SpanLinkType::Reference) => {
                    self.event_stack.push(ElementInfo::Link(Link {
                        // NOTE that "tag" here references the expanded url,
                        // not the actual label...
                        link_ref: LinkRef::Reference {
                            label: "".to_string(),
                            url: tag.to_string(),
                        },
                        range: range.clone(),
                    }));
                }
                LinkType::Span(SpanLinkType::Unresolved) => {
                    self.event_stack.push(ElementInfo::Link(Link {
                        link_ref: LinkRef::Unresolved(tag.to_string()),
                        range: range.clone(),
                    }));
                }
                LinkType::Email => {
                    self.event_stack.push(ElementInfo::Link(Link {
                        link_ref: LinkRef::Email(tag.to_string()),
                        range: range.clone(),
                    }));
                }
                LinkType::AutoLink => {
                    self.event_stack.push(ElementInfo::Link(Link {
                        link_ref: LinkRef::AutoLink(tag.to_string()),
                        range: range.clone(),
                    }));
                }
            },
            (Event::End(Container::Link(_, _)), range) => {
                if let Some(ElementInfo::Link(mut link)) = self.event_stack.pop() {
                    // Workaround for reference not containing the label
                    if let LinkRef::Reference { url, .. } = link.link_ref {
                        let mut label = self.src[range.start + 2..range.end - 1].to_owned();
                        if label.is_empty() {
                            // If it's empty then we have a compact link reference like [tag][].
                            // The tag is exists between the start and end tags.
                            label = self.src[link.range.end..range.start].to_owned();
                        }
                        link.link_ref = LinkRef::Reference { label, url };
                    }

                    link.range.end = range.end;
                    lookup.insert_link(link);
                }
            }
            (Event::Start(Container::LinkDefinition { label }, _), range) => {
                self.event_stack.push(ElementInfo::LinkDef(LinkDef {
                    label: label.to_string(),
                    range: range.clone(),
                    url: "".into(),
                }))
            }
            (Event::End(Container::LinkDefinition { .. }), range) => {
                if let Some(ElementInfo::LinkDef(mut link_def)) = self.event_stack.pop() {
                    // Url is between start and end.
                    link_def.url = self.src[link_def.range.end..range.start].trim().to_owned();

                    link_def.range.end = range.end;
                    lookup.insert_link_def(link_def);
                }
            }
            _ => {}
        }
        return Some(next.0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markup::markup_lookup::BrokenLink;
    use jotdown::Parser;

    fn gen(s: &str) -> MarkupLookup {
        let mut lookup = MarkupLookup::new(s, 0);
        let parser = Parser::new(s).into_offset_iter();
        let transformed = LookupRegister::new(parser, s, Some(&mut lookup));
        for _ in transformed {}
        return lookup;
    }

    #[test]
    fn test_heading_lookup() {
        let lookup = gen("# h1 (x)

text");
        let heading = Heading {
            id: "h1-x".into(),
            content: "h1 (x)".into(),
            range: 0..9,
        };

        assert_eq!(lookup.headings.get("h1-x"), Some(&heading.clone()));
        assert_eq!(lookup.at_pos(1), Some(&ElementInfo::Heading(heading)));
    }

    #[test]
    fn test_inline_link_lookup() {
        let lookup = gen("before [text here](/url) after");

        let element = ElementInfo::Link(Link {
            link_ref: LinkRef::Inline("/url".into()),
            range: 7..24,
        });
        assert_eq!(lookup.at_pos(6), None);
        assert_eq!(lookup.at_pos(7), Some(&element));
        assert_eq!(lookup.at_pos(23), Some(&element));
        assert_eq!(lookup.at_pos(24), None);
    }

    #[test]
    fn test_unresolved_link_lookup() {
        let lookup = gen("before [text here][tag] after");

        let tag = "tag";
        let range = 7..23;

        assert_eq!(
            lookup.broken_links,
            vec![BrokenLink {
                tag: tag.into(),
                range: range.clone()
            }]
        );
        assert_eq!(
            lookup.at_pos(7),
            Some(&ElementInfo::Link(Link {
                link_ref: LinkRef::Unresolved(tag.into()),
                range: range.clone(),
            }))
        );
    }

    #[test]
    fn test_explicit_ref_link_lookup() {
        let lookup = gen("[text here][tag]

[tag]: /url");

        assert_eq!(
            lookup.at_pos(7),
            Some(&ElementInfo::Link(Link {
                link_ref: LinkRef::Reference {
                    label: "tag".into(),
                    url: "/url".into(),
                },
                range: 0..16,
            }))
        );

        let link_def = LinkDef {
            label: "tag".into(),
            range: 18..29,
            url: "/url".into(),
        };
        assert_eq!(
            lookup.at_pos(20),
            Some(&ElementInfo::LinkDef(link_def.clone())),
        );
        assert_eq!(lookup.link_defs.get("tag"), Some(&link_def));
    }

    #[test]
    fn test_collapsed_ref_link_lookup() {
        let lookup = gen("[tag][]

[tag]: /url");

        assert_eq!(
            lookup.at_pos(2),
            Some(&ElementInfo::Link(Link {
                link_ref: LinkRef::Reference {
                    label: "tag".into(),
                    url: "/url".into(),
                },
                range: 0..7,
            }))
        );

        let link_def = LinkDef {
            label: "tag".into(),
            range: 9..20,
            url: "/url".into(),
        };
        assert_eq!(
            lookup.at_pos(9),
            Some(&ElementInfo::LinkDef(link_def.clone())),
        );
        assert_eq!(lookup.link_defs.get("tag"), Some(&link_def));
    }
}
