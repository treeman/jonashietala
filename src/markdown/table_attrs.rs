use crate::markdown::attrs::{parse_attrs, Attrs};
use crate::markdown::html::push_open_tag;
use crate::markdown::pd_html::HtmlWriter;
use itertools::{Itertools, MultiPeek};
use pulldown_cmark::{Event, Tag};

pub struct TableAttrs<'a, I: Iterator<Item = Event<'a>>> {
    parent: MultiPeek<I>,
}

impl<'a, I: Iterator<Item = Event<'a>>> TableAttrs<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.multipeek(),
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for TableAttrs<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let alignment = match self.parent.next()? {
            Event::Start(Tag::Table(alignment)) => alignment,
            other => return Some(other),
        };

        // | One | Two
        // | --  | --
        // | 1   | 2
        // { .class }
        //
        // Will create events like:
        //   Start table
        //   Start table head
        //   ...
        //   End table head
        //   Start table row
        //   ..
        //   End table row
        //
        //   Start table row
        //      Start table cell
        //      Text({ .class })
        //      End table cell
        //
        //      N Start/End empty table cell
        //   End table row
        //   End table
        //
        // The strategy here is to peek and collect everything until the end
        // and see if there are matching events.

        // Take everything until the end
        let mut events = Vec::new();
        loop {
            match self.parent.next()? {
                end @ Event::End(Tag::Table(_)) => {
                    events.push(end);
                    break;
                }
                event => events.push(event),
            }
        }

        // Split out attrs, discarding the last row if applicable
        let (events, attrs) = split_attrs(events);

        let mut res = String::new();
        // Hack to render a table with classses, keeping the table alignment.
        if let Some(attrs) = attrs {
            attrs.push_open_tag(&mut res, "table");
        } else {
            push_open_tag::<&str>(&mut res, "table", None);
        }
        let mut writer = HtmlWriter::new(events.into_iter(), &mut res);
        writer.table_alignments = alignment.to_vec();
        writer.run().unwrap();
        Some(Event::Html(res.into()))
    }
}

fn split_attrs(mut events: Vec<Event>) -> (Vec<Event>, Option<Attrs>) {
    let mut it = events.iter().rev();

    if !matches!(it.next(), Some(Event::End(Tag::Table(_)))) {
        return (events, None);
    }
    if !matches!(it.next(), Some(Event::End(Tag::TableRow))) {
        return (events, None);
    }

    // Check all cells until we have a matching text with attributes
    let attrs = loop {
        match it.next() {
            Some(Event::Text(s)) => match parse_attrs(s).expect("error parsing attr") {
                Some(attrs) => break attrs,
                None => return (events, None),
            },
            Some(Event::Start(Tag::TableCell) | Event::End(Tag::TableCell)) => {}
            _ => return (events, None),
        }
    };
    // If we have a matching attr, table row should end
    if !matches!(it.next(), Some(Event::Start(Tag::TableCell))) {
        return (events, None);
    }
    if !matches!(it.next(), Some(Event::Start(Tag::TableRow))) {
        return (events, None);
    }

    let end = events.pop().unwrap();
    // It's a match, now we pop the last tablerow
    while !matches!(events.pop(), Some(Event::Start(Tag::TableRow))) {}
    // Add back table end
    events.push(end);

    (events, Some(attrs))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pulldown_cmark::{html, Options, Parser};

    fn convert(s: &str) -> String {
        let parser = Parser::new_ext(s, Options::all());
        let transformed = TableAttrs::new(parser);
        let mut body = String::new();
        html::push_html(&mut body, transformed);
        body
    }

    #[test]
    fn test_table_attrs() {
        let s = r"
| One | Two
| --  | --
| 1   | 2
{ .myclass }";
        assert_eq!(
            convert(s).trim_end(),
            r#"<table class="myclass"><thead><tr><th>One</th><th>Two</th></tr></thead><tbody>
<tr><td>1</td><td>2</td></tr>
</tbody></table>"#
        );
    }

    #[test]
    fn test_table() {
        let s = r"
| One | Two
| --  | --
| 1   | 2";
        assert_eq!(
            convert(s).trim_end(),
            r#"<table><thead><tr><th>One</th><th>Two</th></tr></thead><tbody>
<tr><td>1</td><td>2</td></tr>
</tbody></table>"#
        );
    }
}
