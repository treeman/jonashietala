use jotdown::{Attributes, Container, Event, LinkType, SpanLinkType};

pub struct TransformHeaders<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> TransformHeaders<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent,
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for TransformHeaders<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let (level, has_section, id, attrs) = match self.parent.next()? {
            Event::Start(
                Container::Heading {
                    level,
                    has_section,
                    id,
                },
                attrs,
            ) => (level, has_section, id, attrs),
            other => return Some(other),
        };

        let mut events = Vec::new();
        loop {
            match self.parent.next()? {
                Event::End(Container::Heading { .. }) => break,
                event => events.push(event),
            }
        }

        // let id: Cow<_> = util::to_id(&id).into();
        let level = level + 1;

        let heading = Container::Heading {
            level,
            has_section,
            id: id.clone(),
        };
        let link = Container::Link(
            format!("#{id}").into(),
            LinkType::Span(SpanLinkType::Inline),
        );
        let mut link_attrs = Attributes::new();
        link_attrs.insert("class", "heading-ref".into());

        self.event_queue.push(Event::End(heading.clone()));
        self.event_queue.push(Event::End(link.clone()));
        for event in events.into_iter().rev() {
            self.event_queue.push(event);
        }

        self.event_queue.push(Event::Start(link, link_attrs));
        Some(Event::Start(heading, attrs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<String> {
        let parser = Parser::new(s);
        let transformed = TransformHeaders::new(parser);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_transform_headers() -> Result<()> {
        let s = "# Header 1";
        assert_eq!(
            convert(s)?,
            r##"<section id="Header-1">
<h2><a href="#Header-1" class="heading-ref">Header 1</a></h2>
</section>
"##
        );

        let s = "## Header 2";
        assert_eq!(
            convert(s)?,
            r##"<section id="Header-2">
<h3><a href="#Header-2" class="heading-ref">Header 2</a></h3>
</section>
"##
        );

        let s = "## With [link](#x)";
        assert_eq!(
            convert(s)?,
            r##"<section id="With-link">
<h3><a href="#With-link" class="heading-ref">With <a href="#x">link</a></a></h3>
</section>
"##
        );

        let s = "{ #my-id }\n## Manual";
        assert_eq!(
            convert(s)?,
            r##"<section id="my-id">
<h3><a href="#my-id" class="heading-ref">Manual</a></h3>
</section>
"##
        );

        let s = "{ #my-id .class}\n# Head";
        assert_eq!(
            convert(s)?,
            r##"<section id="my-id" class="class">
<h2><a href="#my-id" class="heading-ref">Head</a></h2>
</section>
"##
        );

        Ok(())
    }
}
