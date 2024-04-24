use crate::markup::markup_lookup::{
    Element, Heading, Img, ImgRef, Link, LinkDef, LinkRef, RawElementLookup,
};
use crate::markup::MarkupLookup;
use jotdown::{Container, Event, LinkType, SpanLinkType};
use std::cell::RefCell;
use std::ops::Range;
use std::rc::Rc;

pub struct LookupRegister<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> {
    parent: I,
    lookup: Option<Rc<RefCell<MarkupLookup>>>,
    event_stack: Vec<RawElementLookup>,
    src: &'a str,
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> LookupRegister<'a, I> {
    pub fn new(parent: I, src: &'a str, lookup: Option<Rc<RefCell<MarkupLookup>>>) -> Self {
        Self {
            parent,
            lookup,
            event_stack: Vec::new(),
            src,
        }
    }
}

impl<'a, I: Iterator<Item = (Event<'a>, Range<usize>)>> Iterator for LookupRegister<'a, I> {
    type Item = (Event<'a>, Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.parent.next()?;

        let mut lookup = match &self.lookup {
            Some(lookup) => lookup.borrow_mut(),
            None => return Some(next),
        };

        match &next {
            (Event::Start(Container::Heading { id, level, .. }, _), range) => {
                self.event_stack.push(RawElementLookup {
                    element: Element::Heading(Heading {
                        level: *level,
                        id: id.to_string(),
                        content: "".into(),
                    }),
                    char_range: range.clone(),
                })
            }
            (Event::End(Container::Heading { .. }), range) => {
                if let Some(RawElementLookup {
                    element: Element::Heading(mut heading),
                    mut char_range,
                }) = self.event_stack.pop()
                {
                    // Content is between start and end.
                    heading.content = self.src[char_range.end..range.start].trim().to_owned();

                    char_range.end = range.end;
                    lookup.insert_heading(heading, char_range);
                }
            }
            (Event::Start(Container::Image(tag, link_type), _), range) => match link_type {
                SpanLinkType::Inline => {
                    self.event_stack.push(RawElementLookup {
                        element: Element::Img(Img {
                            link_ref: ImgRef::Inline(tag.to_string()),
                        }),
                        char_range: range.clone(),
                    });
                }
                SpanLinkType::Reference => {
                    self.event_stack.push(RawElementLookup {
                        element: Element::Img(Img {
                            // NOTE that "tag" here references the expanded url,
                            // not the actual label...
                            link_ref: ImgRef::Reference {
                                label: "".to_string(),
                                url: tag.to_string(),
                            },
                        }),
                        char_range: range.clone(),
                    });
                }
                SpanLinkType::Unresolved => {
                    self.event_stack.push(RawElementLookup {
                        element: Element::Img(Img {
                            link_ref: ImgRef::Unresolved(tag.to_string()),
                        }),
                        char_range: range.clone(),
                    });
                }
            },
            (Event::End(Container::Image(_, _)), range) => {
                if let Some(RawElementLookup {
                    element: Element::Img(mut img),
                    mut char_range,
                }) = self.event_stack.pop()
                {
                    // Workaround for reference not containing the label
                    if let ImgRef::Reference { url, .. } = img.link_ref {
                        let mut label = self.src[range.start + 2..range.end - 1].to_owned();
                        if label.is_empty() {
                            // If it's empty then we have a compact link reference like [tag][].
                            // The tag is exists between the start and end tags.
                            label = self.src[char_range.end..range.start].to_owned();
                        }
                        img.link_ref = ImgRef::Reference { label, url };
                    }

                    char_range.end = range.end;
                    lookup.insert_img(img, char_range);
                }
            }
            (Event::Start(Container::Link(tag, link_type), _), range) => match link_type {
                LinkType::Span(SpanLinkType::Inline) => {
                    self.event_stack.push(RawElementLookup {
                        element: Element::Link(Link {
                            link_ref: LinkRef::Inline(tag.to_string()),
                        }),
                        char_range: range.clone(),
                    });
                }
                LinkType::Span(SpanLinkType::Reference) => {
                    self.event_stack.push(RawElementLookup {
                        element: Element::Link(Link {
                            // NOTE that "tag" here references the expanded url,
                            // not the actual label...
                            link_ref: LinkRef::Reference {
                                label: "".to_string(),
                                url: tag.to_string(),
                            },
                        }),
                        char_range: range.clone(),
                    });
                }
                LinkType::Span(SpanLinkType::Unresolved) => {
                    self.event_stack.push(RawElementLookup {
                        element: Element::Link(Link {
                            link_ref: LinkRef::Unresolved(tag.to_string()),
                        }),
                        char_range: range.clone(),
                    });
                }
                LinkType::Email => {
                    self.event_stack.push(RawElementLookup {
                        element: Element::Link(Link {
                            link_ref: LinkRef::Email(tag.to_string()),
                        }),
                        char_range: range.clone(),
                    });
                }
                LinkType::AutoLink => {
                    self.event_stack.push(RawElementLookup {
                        element: Element::Link(Link {
                            link_ref: LinkRef::AutoLink(tag.to_string()),
                        }),
                        char_range: range.clone(),
                    });
                }
            },
            (Event::End(Container::Link(_, _)), range) => {
                if let Some(RawElementLookup {
                    element: Element::Link(mut link),
                    mut char_range,
                }) = self.event_stack.pop()
                {
                    // Workaround for reference not containing the label
                    if let LinkRef::Reference { url, .. } = link.link_ref {
                        let mut label = self.src[range.start + 2..range.end - 1].to_owned();
                        if label.is_empty() {
                            // If it's empty then we have a compact link reference like [tag][].
                            // The tag is exists between the start and end tags.
                            label = self.src[char_range.end..range.start].to_owned();
                        }
                        link.link_ref = LinkRef::Reference { label, url };
                    }

                    char_range.end = range.end;
                    lookup.insert_link(link, char_range);
                }
            }
            (Event::Start(Container::LinkDefinition { label }, _), range) => {
                self.event_stack.push(RawElementLookup {
                    element: Element::LinkDef(LinkDef {
                        label: label.to_string(),
                        url: "".into(),
                    }),
                    char_range: range.clone(),
                })
            }
            (Event::End(Container::LinkDefinition { .. }), range) => {
                if let Some(RawElementLookup {
                    element: Element::LinkDef(mut link_def),
                    mut char_range,
                }) = self.event_stack.pop()
                {
                    // Url is between start and end.
                    link_def.url = self.src[char_range.end..range.start].trim().to_owned();

                    char_range.end = range.end;
                    lookup.insert_link_def(link_def, char_range);
                }
            }
            _ => {}
        }
        return Some(next);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markup::markup_lookup::{ElementLookup, HeadingLookup, LinkDefLookup, PosRange};
    use jotdown::Parser;

    fn gen(s: &str) -> MarkupLookup {
        let lookup = Rc::new(RefCell::new(MarkupLookup::new(s, 0)));
        let parser = Parser::new(s).into_offset_iter();
        let transformed = LookupRegister::new(parser, s, Some(lookup.clone()));
        for _ in transformed {}
        Rc::try_unwrap(lookup).unwrap().into_inner()
    }

    #[test]
    fn test_heading_lookup() {
        let lookup = gen("# h1 (x)

text");
        let heading = Heading {
            id: "h1-x".into(),
            level: 1,
            content: "h1 (x)".into(),
        };
        let range = PosRange::new((0, 0), (1, 0));
        let char_range = 0..9;

        assert_eq!(
            lookup.headings.get("h1-x"),
            Some(&vec![HeadingLookup {
                heading: heading.clone(),
                range,
                char_range: char_range.clone()
            }])
        );
        assert_eq!(
            lookup.at_pos(1),
            Some(&ElementLookup {
                element: Element::Heading(heading),
                range,
                char_range,
            })
        );
    }

    #[test]
    fn test_inline_link_lookup() {
        let lookup = gen("before [text here](/url) after");

        let element = ElementLookup {
            element: Element::Link(Link {
                link_ref: LinkRef::Inline("/url".into()),
            }),
            range: PosRange::new((0, 7), (0, 24)),
            char_range: 7..24,
        };
        assert_eq!(lookup.at_pos(6), None);
        assert_eq!(lookup.at_pos(7), Some(&element));
        assert_eq!(lookup.at_pos(23), Some(&element));
        assert_eq!(lookup.at_pos(24), None);
    }

    #[test]
    fn test_unresolved_link_lookup() {
        let lookup = gen("before [text here][tag] after");

        assert_eq!(
            lookup.at_pos(7),
            Some(&ElementLookup {
                element: Element::Link(Link {
                    link_ref: LinkRef::Unresolved("tag".into()),
                }),
                range: PosRange::new((0, 7), (0, 23)),
                char_range: 7..23,
            })
        );
    }

    #[test]
    fn test_explicit_ref_link_lookup() {
        let lookup = gen("[text here][tag]

[tag]: /url");

        assert_eq!(
            lookup.at_pos(7),
            Some(&ElementLookup {
                element: Element::Link(Link {
                    link_ref: LinkRef::Reference {
                        label: "tag".into(),
                        url: "/url".into(),
                    },
                }),
                range: PosRange::new((0, 0), (0, 16)),
                char_range: 0..16,
            })
        );

        let link_def = LinkDef {
            label: "tag".into(),
            url: "/url".into(),
        };
        let range = PosRange::new((2, 0), (2, 11));
        let char_range = 18..29;
        assert_eq!(
            lookup.at_pos(20),
            Some(&ElementLookup {
                element: Element::LinkDef(link_def.clone()),
                range,
                char_range: char_range.clone()
            }),
        );
        assert_eq!(
            lookup.link_defs.get("tag"),
            Some(&vec![LinkDefLookup {
                link_def,
                range,
                char_range,
            }])
        );
    }

    #[test]
    fn test_collapsed_ref_link_lookup() {
        let lookup = gen("[tag][]

[tag]: /url");

        assert_eq!(
            lookup.at_pos(2),
            Some(&ElementLookup {
                element: Element::Link(Link {
                    link_ref: LinkRef::Reference {
                        label: "tag".into(),
                        url: "/url".into(),
                    },
                }),
                range: PosRange::new((0, 0), (0, 7)),
                char_range: 0..7,
            })
        );

        let link_def = LinkDef {
            label: "tag".into(),
            url: "/url".into(),
        };
        let range = PosRange::new((2, 0), (2, 11));
        let char_range = 9..20;
        assert_eq!(
            lookup.at_pos(9),
            Some(&ElementLookup {
                element: Element::LinkDef(link_def.clone()),
                range,
                char_range: char_range.clone()
            }),
        );
        assert_eq!(
            lookup.link_defs.get("tag"),
            Some(&vec![LinkDefLookup {
                link_def,
                range,
                char_range,
            }])
        );
    }

    #[test]
    fn test_inline_img_lookup() {
        let lookup = gen("![img text](/img.png)");

        assert_eq!(
            lookup.at_pos(0),
            Some(&ElementLookup {
                element: Element::Img(Img {
                    link_ref: ImgRef::Inline("/img.png".into()),
                }),
                range: PosRange::new((0, 0), (0, 21)),
                char_range: 0..21,
            }),
        );
    }

    #[test]
    fn test_unresolved_img_lookup() {
        let lookup = gen("![text here][tag]");

        assert_eq!(
            lookup.at_pos(0),
            Some(&ElementLookup {
                element: Element::Img(Img {
                    link_ref: ImgRef::Unresolved("tag".into()),
                }),
                range: PosRange::new((0, 0), (0, 17)),
                char_range: 0..17,
            }),
        );
    }

    #[test]
    fn test_explicit_ref_img_lookup() {
        let lookup = gen("![text here][tag]

[tag]: /img.png");

        assert_eq!(
            lookup.at_pos(0),
            Some(&ElementLookup {
                element: Element::Img(Img {
                    link_ref: ImgRef::Reference {
                        label: "tag".into(),
                        url: "/img.png".into(),
                    },
                }),
                range: PosRange::new((0, 0), (0, 17)),
                char_range: 0..17,
            }),
        );
    }
}
