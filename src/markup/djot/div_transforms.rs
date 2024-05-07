use jotdown::{Attributes, Container, Event, LinkType, SpanLinkType};

pub struct DivTransforms<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> DivTransforms<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent,
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for DivTransforms<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let (transformer, class) = match self.parent.next()? {
            Event::Start(Container::Div { class }, attrs) => match TransformType::parse(class) {
                Some(h) => (h, class),
                _ => return Some(Event::Start(Container::Div { class }, attrs)),
            },
            other => return Some(other),
        };

        let mut events = Vec::new();
        loop {
            match self.parent.next()? {
                Event::End(Container::Div { class: end_class }) if end_class == class => break,
                other => events.push(other),
            }
        }

        for x in transformer.transform(events).into_iter().rev() {
            self.event_queue.push(x);
        }
        self.event_queue.pop()
    }
}

#[derive(Debug)]
enum TransformType {
    Note,
    Tip,
    Warn,
    Flex,
    Figure,
    Gallery,
}

impl TransformType {
    fn parse(id: &str) -> Option<Self> {
        match id.to_lowercase().as_str() {
            "note" => Some(Self::Note),
            "tip" => Some(Self::Tip),
            "warn" => Some(Self::Warn),
            "flex" => Some(Self::Flex),
            "figure" => Some(Self::Figure),
            "gallery" => Some(Self::Gallery),
            _ => None,
        }
    }

    fn transform<'a>(self, content: Vec<Event<'a>>) -> Vec<Event<'a>> {
        match self {
            Self::Note => wrap_content(content.into_iter(), "aside", Some("note")),
            Self::Tip => wrap_content(content.into_iter(), "aside", Some("tip")),
            Self::Warn => wrap_content(content.into_iter(), "aside", Some("warn")),
            Self::Flex => parse_flex(content.into_iter()),
            Self::Figure => wrap_images(content.into_iter(), "figure", None, false),
            Self::Gallery => wrap_images(content.into_iter(), "figure", Some("gallery"), true),
        }
    }
}

fn wrap_content<'a, I: Iterator<Item = Event<'a>>>(
    content: I,
    container: &str,
    class: Option<&str>,
) -> Vec<Event<'a>> {
    let mut res = Vec::new();
    let html = Container::RawBlock { format: "html" };

    res.push(Event::Start(html.clone(), Attributes::new()));
    match class {
        Some(class) => res.push(Event::Str(
            format!(r#"<{container} class="{class}">"#).into(),
        )),
        None => res.push(Event::Str(format!("<{container}>").into())),
    }
    res.push(Event::End(html.clone()));

    for x in content {
        res.push(x);
    }

    res.push(Event::Start(html.clone(), Attributes::new()));
    res.push(Event::Str(format!("</{container}>").into()));
    res.push(Event::End(html));

    res
}

fn wrap_images<'a, I>(
    content: I,
    container: &str,
    class: Option<&str>,
    insert_links: bool,
) -> Vec<Event<'a>>
where
    I: Iterator<Item = Event<'a>>,
{
    let (mut imgs, caption, _count) = split_images(content, insert_links);

    let mut inner = Vec::new();
    inner.append(&mut imgs);

    if !caption.is_empty() {
        inner.append(&mut wrap_content(caption.into_iter(), "figcaption", None));
    }

    wrap_content(inner.into_iter(), container, class)
}

fn parse_flex<'a, I>(content: I) -> Vec<Event<'a>>
where
    I: Iterator<Item = Event<'a>>,
{
    let (mut imgs, caption, img_count) = split_images(content, true);

    let class = match img_count {
        1 => None,
        2 => Some("flex-50"),
        3 => Some("flex-33"),
        4 => Some("flex-25"),
        5 => Some("flex-20"),
        x => panic!("Too many images {x} in `Flex`, try `Gallery` instead"),
    };

    let mut inner = Vec::new();
    inner.append(&mut imgs);

    if !caption.is_empty() {
        inner.append(&mut wrap_content(caption.into_iter(), "figcaption", None));
    }

    wrap_content(inner.into_iter(), "figure", class)
}

fn split_images<'a, I>(
    mut content: I,
    insert_links: bool,
) -> (Vec<Event<'a>>, Vec<Event<'a>>, usize)
where
    I: Iterator<Item = Event<'a>>,
{
    let mut img_count = 0;
    let mut imgs = Vec::new();
    while let Some(e) = content.next() {
        match e {
            Event::Str(href) => {
                img_count += 1;
                imgs.push(Event::Softbreak);

                if insert_links {
                    imgs.push(Event::Start(
                        Container::Link(href.to_owned(), LinkType::Span(SpanLinkType::Inline)),
                        Attributes::new(),
                    ));
                }
                let img = Container::Image(href.to_owned(), SpanLinkType::Inline);
                imgs.push(Event::Start(img.clone(), Attributes::new()));
                imgs.push(Event::End(img.clone()));

                if insert_links {
                    imgs.push(Event::End(Container::Link(
                        href.to_owned(),
                        LinkType::Span(SpanLinkType::Inline),
                    )));
                }
            }
            ref start @ Event::Start(Container::Image(ref href, _), _) => {
                img_count += 1;
                imgs.push(Event::Softbreak);
                if insert_links {
                    imgs.push(Event::Start(
                        Container::Link(href.to_owned(), LinkType::Span(SpanLinkType::Inline)),
                        Attributes::new(),
                    ));
                }
                imgs.push(start.clone());
            }
            ref end @ Event::End(Container::Image(ref href, _)) => {
                imgs.push(end.clone());
                if insert_links {
                    imgs.push(Event::End(Container::Link(
                        href.to_owned(),
                        LinkType::Span(SpanLinkType::Inline),
                    )));
                }
            }
            Event::End(Container::Paragraph) => break,
            _ => {}
        }
    }

    (imgs, content.collect(), img_count)
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<String> {
        let parser = Parser::new(s);
        let transformed = DivTransforms::new(parser);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_parse_note() -> Result<()> {
        let s = "::: note
Text here
:::";
        assert_eq!(
            convert(s)?,
            r#"<aside class="note">
<p>Text here</p>
</aside>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_tip() -> Result<()> {
        let s = "::: tip
Text here
:::";
        assert_eq!(
            convert(s)?,
            r#"<aside class="tip">
<p>Text here</p>
</aside>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_warn() -> Result<()> {
        let s = "::: warn
Text here
:::";
        assert_eq!(
            convert(s)?,
            r#"<aside class="warn">
<p>Text here</p>
</aside>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_gallery() -> Result<()> {
        let s = r#"
::: Gallery
/images/img1.jpg
/images/img2.jpg
:::"#;

        assert_eq!(
            convert(s)?,
            r#"
<figure class="gallery">
<a href="/images/img1.jpg"><img alt="" src="/images/img1.jpg"></a>
<a href="/images/img2.jpg"><img alt="" src="/images/img2.jpg"></a>
</figure>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_gallery_imgs() -> Result<()> {
        let s = r#"
::: Gallery
![](/images/img1.jpg)
![](/images/img2.jpg)
:::"#;

        assert_eq!(
            convert(s)?,
            r#"
<figure class="gallery">
<a href="/images/img1.jpg"><img alt="" src="/images/img1.jpg"></a>
<a href="/images/img2.jpg"><img alt="" src="/images/img2.jpg"></a>
</figure>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_gallery_title() -> Result<()> {
        let s = r#"
::: Gallery
/images/img1.jpg
/images/img2.jpg

My *title*
:::"#;

        assert_eq!(
            convert(s)?,
            r#"
<figure class="gallery">
<a href="/images/img1.jpg"><img alt="" src="/images/img1.jpg"></a>
<a href="/images/img2.jpg"><img alt="" src="/images/img2.jpg"></a>
<figcaption>
<p>My <strong>title</strong></p>
</figcaption>
</figure>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_flex() -> Result<()> {
        let s = r#"
::: Flex
/images/img1.jpg
/images/img2.jpg
:::"#;

        assert_eq!(
            convert(s)?,
            r#"
<figure class="flex-50">
<a href="/images/img1.jpg"><img alt="" src="/images/img1.jpg"></a>
<a href="/images/img2.jpg"><img alt="" src="/images/img2.jpg"></a>
</figure>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_flex_attrs() -> Result<()> {
        let s = r#"
::: Flex
![](/images/img1.jpg){ height=100 }
![](/images/img2.jpg){ height=100 }
:::"#;

        assert_eq!(
            convert(s)?,
            r#"
<figure class="flex-50">
<a href="/images/img1.jpg"><img height="100" alt="" src="/images/img1.jpg"></a>
<a href="/images/img2.jpg"><img height="100" alt="" src="/images/img2.jpg"></a>
</figure>
"#
        );

        Ok(())
    }
}
