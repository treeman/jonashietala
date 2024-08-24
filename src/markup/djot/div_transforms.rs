use eyre::{eyre, Result};
use jotdown::{Attributes, Container, Event, LinkType, SpanLinkType};
use lazy_static::lazy_static;
use regex::Regex;
use tracing::{error, warn};

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

        let (transformer, class, attrs) = match self.parent.next()? {
            Event::Start(Container::Div { class }, attrs) => match TransformType::parse(class) {
                Some(h) => (h, class, attrs),
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

        for x in transformer.transform(events, &attrs).into_iter().rev() {
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
    Important,
    Update,
    Flex,
    Figure,
    Gallery,
    Timeline,
}

impl TransformType {
    fn parse(id: &str) -> Option<Self> {
        match id.to_lowercase().as_str() {
            "note" => Some(Self::Note),
            "tip" => Some(Self::Tip),
            "warn" | "warning" => Some(Self::Warn),
            "important" => Some(Self::Important),
            "update" => Some(Self::Update),
            "flex" => Some(Self::Flex),
            "figure" => Some(Self::Figure),
            "gallery" => Some(Self::Gallery),
            "timeline" => Some(Self::Timeline),
            _ => None,
        }
    }

    fn try_transform<'a>(
        self,
        content: Vec<Event<'a>>,
        attrs: &Attributes,
    ) -> Result<Vec<Event<'a>>> {
        match self {
            Self::Note => wrap_content(content.into_iter(), "aside", Some("note")),
            Self::Tip => wrap_content(content.into_iter(), "aside", Some("tip")),
            Self::Warn => wrap_content(content.into_iter(), "aside", Some("warn")),
            Self::Important => wrap_content(content.into_iter(), "aside", Some("important")),
            Self::Update => {
                let date = match attrs.get("date") {
                    Some(date) => format!(r#" <span class="date">{date}</span>"#),
                    None => "".into(),
                };

                let mut info = Vec::new();
                let html = Container::RawBlock { format: "html" };
                info.push(Event::Start(html.clone(), Attributes::new()));
                info.push(Event::Str(
                    format!(r#"<div class="info">Update{date}</div>"#).into(),
                ));
                info.push(Event::End(html.clone()));

                wrap_content(info.into_iter().chain(content), "aside", Some("update"))
            }
            Self::Flex => parse_flex(content.into_iter()),
            Self::Figure => wrap_images(content.into_iter(), "figure", None, false),
            Self::Gallery => wrap_images(content.into_iter(), "figure", Some("gallery"), true),
            Self::Timeline => convert_timeline(content.into_iter()),
        }
    }

    fn transform<'a>(self, content: Vec<Event<'a>>, attrs: &Attributes) -> Vec<Event<'a>> {
        match self.try_transform(content, attrs) {
            Ok(res) => res,
            Err(err) => {
                error!("{}", err);
                Vec::new()
            }
        }
    }
}

fn wrap_content<'a, I: Iterator<Item = Event<'a>>>(
    content: I,
    container: &str,
    class: Option<&str>,
) -> Result<Vec<Event<'a>>> {
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

    Ok(res)
}

fn wrap_images<'a, I>(
    content: I,
    container: &str,
    class: Option<&str>,
    insert_links: bool,
) -> Result<Vec<Event<'a>>>
where
    I: Iterator<Item = Event<'a>>,
{
    let (mut imgs, caption, _count) = split_images(content, insert_links);

    let mut inner = Vec::new();
    inner.append(&mut imgs);

    if !caption.is_empty() {
        inner.append(&mut wrap_content(caption.into_iter(), "figcaption", None)?);
    }

    wrap_content(inner.into_iter(), container, class)
}

fn parse_flex<'a, I>(content: I) -> Result<Vec<Event<'a>>>
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
        inner.append(&mut wrap_content(caption.into_iter(), "figcaption", None)?);
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
    for e in content.by_ref() {
        match e {
            Event::Str(href) => {
                img_count += 1;
                imgs.push(Event::Softbreak);

                if insert_links {
                    imgs.push(Event::Start(
                        Container::Link(href.clone(), LinkType::Span(SpanLinkType::Inline)),
                        Attributes::new(),
                    ));
                }
                let img = Container::Image(href.clone(), SpanLinkType::Inline);
                imgs.push(Event::Start(img.clone(), Attributes::new()));
                imgs.push(Event::End(img.clone()));

                if insert_links {
                    imgs.push(Event::End(Container::Link(
                        href.clone(),
                        LinkType::Span(SpanLinkType::Inline),
                    )));
                }
            }
            ref start @ Event::Start(Container::Image(ref href, _), _) => {
                img_count += 1;
                imgs.push(Event::Softbreak);
                if insert_links {
                    imgs.push(Event::Start(
                        Container::Link(href.clone(), LinkType::Span(SpanLinkType::Inline)),
                        Attributes::new(),
                    ));
                }
                imgs.push(start.clone());
            }
            ref end @ Event::End(Container::Image(ref href, _)) => {
                imgs.push(end.clone());
                if insert_links {
                    imgs.push(Event::End(Container::Link(
                        href.clone(),
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

lazy_static! {
    pub static ref TRAILING_CLASS: Regex = Regex::new(r"^(.+)\s\.(\S+)$").unwrap();
}

fn convert_timeline<'a, I>(mut content: I) -> Result<Vec<Event<'a>>>
where
    I: Iterator<Item = Event<'a>>,
{
    assert!(matches!(
        content.next(),
        Some(Event::Start(Container::DescriptionList, _))
    ));

    let mut res = Vec::new();

    let html = Container::RawBlock { format: "html" };

    res.push(Event::Start(html.clone(), Attributes::new()));
    res.push(Event::Str(
        r#"<div class="timeline"><div class="events">"#.into(),
    ));
    res.push(Event::End(html.clone()));

    while let Some(token) = content.next() {
        match token {
            Event::Start(Container::DescriptionTerm, _) => {}
            Event::End(Container::DescriptionList) => break,
            token => return Err(eyre!("Unknown token: {:?}", token)),
        };

        let (when, class) = match content.next() {
            Some(Event::Str(s)) => match TRAILING_CLASS.captures(s.as_ref()) {
                Some(captures) => (captures[1].to_string(), captures[2].to_string()),
                None => return Err(eyre!("Failed to match timeline heading: `{s}`")),
            },
            token => {
                return Err(eyre!("Expected string , got: {:?}", token));
            }
        };

        for token in content.by_ref() {
            if matches!(token, Event::Start(Container::DescriptionDetails, _)) {
                break;
            }
        }

        res.push(Event::Start(html.clone(), Attributes::new()));
        res.push(Event::Str(
            format!(
                r#"<div class="event {}">
      <svg class="marker" xmlns="http://www.w3.org/2000/svg" width="12" height="12">
        <circle cx="6" cy="6" r="6" />
      </svg>
      <div class="content">
        <time>{}</time>
        <div class="text">
        "#,
                class,
                html_escape::encode_text(&when)
            )
            .into(),
        ));
        res.push(Event::End(html.clone()));

        for token in content.by_ref() {
            match token {
                Event::End(Container::DescriptionDetails) => break,
                x => res.push(x),
            }
        }

        // Close `text`, `content` and `event`
        res.push(Event::Start(html.clone(), Attributes::new()));
        res.push(Event::Str("</div></div></div>".into()));
        res.push(Event::End(html.clone()));
    }

    // Close `events` and `timeline`
    res.push(Event::Start(html.clone(), Attributes::new()));
    res.push(Event::Str("</div></div>".into()));
    res.push(Event::End(html.clone()));

    Ok(res)
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
    fn test_parse_important() -> Result<()> {
        let s = "::: important
Text here
:::";
        assert_eq!(
            convert(s)?,
            r#"<aside class="important">
<p>Text here</p>
</aside>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_update() -> Result<()> {
        let s = r#"
::: update
Text here
:::"#;
        assert_eq!(
            convert(s)?,
            r#"
<aside class="update">
<div class="info">Update</div>
<p>Text here</p>
</aside>
"#
        );

        Ok(())
    }

    #[test]
    fn test_parse_update_date() -> Result<()> {
        let s = r#"
{date="2024-06-01"}
::: update
Text here
:::"#;
        assert_eq!(
            convert(s)?,
            r#"
<aside class="update">
<div class="info">Update <span class="date">2024-06-01</span></div>
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
