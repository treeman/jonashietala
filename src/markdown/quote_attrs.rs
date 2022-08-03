use crate::markdown::attrs::{parse_attrs, Attrs};
use crate::markdown::html::push_open_tag;
use itertools::{Itertools, MultiPeek};
use pulldown_cmark::{html::push_html, Event, Tag};

pub struct QuoteAttrs<'a, I: Iterator<Item = Event<'a>>> {
    parent: MultiPeek<I>,
}

impl<'a, I: Iterator<Item = Event<'a>>> QuoteAttrs<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.multipeek(),
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for QuoteAttrs<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.parent.next()? {
            Event::Start(Tag::BlockQuote) => {}
            other => return Some(other),
        };

        // > Text
        // {: notice}
        //
        // Will create events like:
        //   Start blockquote
        //   Start paragraph
        //   ...
        //   SoftBreak
        //   Text({: notice })
        //   End paragraph
        //   End blockquote
        //
        // The strategy here is to peek and collect everything until the end
        // and see if there are matching events.

        // Take everything until the end
        let mut events = Vec::new();
        loop {
            match self.parent.next()? {
                Event::End(Tag::BlockQuote) => break,
                event => events.push(event),
            }
        }

        let (events, attrs) = split_attrs(events);

        let res = match attrs.and_then(|x| x.parser) {
            Some(parser) => parser.transform(events),
            None => {
                let mut res = String::new();
                push_open_tag::<&str>(&mut res, "blockquote", None);
                push_html(&mut res, events.into_iter());
                res.push_str("</blockquote>");
                res
            }
        };

        Some(Event::Html(res.into()))
    }
}

fn split_attrs(mut events: Vec<Event>) -> (Vec<Event>, Option<Attrs>) {
    let mut it = events.iter().rev();

    if it.next() != Some(&Event::End(Tag::Paragraph)) {
        return (events, None);
    }

    let attrs = match it.next() {
        Some(Event::Text(s)) => match parse_attrs(s).expect("error parsing attr") {
            Some(attrs) => attrs,
            None => return (events, None),
        },
        _ => return (events, None),
    };

    if it.next() != Some(&Event::SoftBreak) {
        return (events, None);
    };

    // It's a match, now we can pop the soft break and the attribute text.
    events.pop();
    events.pop();
    // Replace the softbreak with an ending parapgrah
    events.pop();
    events.push(Event::End(Tag::Paragraph));

    (events, Some(attrs))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pulldown_cmark::{html, Options, Parser};

    fn convert(s: &str) -> String {
        let parser = Parser::new_ext(s, Options::all());
        let transformed = QuoteAttrs::new(parser);
        let mut body = String::new();
        html::push_html(&mut body, transformed);
        body
    }

    #[test]
    fn test_quote_notice() {
        // Start block
        // Start paragraph
        // ...
        // SoftBreak
        // {: notice}
        // End paragraph
        // End block
        let s = r"
> Text here
{ :notice }";
        assert_eq!(convert(s), r"<aside>Text here</aside>");
    }

    #[test]
    fn test_quote_skip() {
        let s = r"
> Text here

Other";
        assert_eq!(
            convert(s).trim_end(),
            "<blockquote><p>Text here</p>\n</blockquote>\n<p>Other</p>"
        );
    }
}
