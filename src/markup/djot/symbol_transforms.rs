use crate::markup::graphs::PostStatsGraph;
use chrono::NaiveDate;
use eyre::Result;
use jotdown::{Attributes, Container, Event};
use serde::Serialize;
use std::iter::Peekable;
use tracing::error;

pub struct SymbolTransforms<'a, I: Iterator<Item = Event<'a>>> {
    parent: Peekable<I>,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> SymbolTransforms<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.peekable(),
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for SymbolTransforms<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        // If we try to add attributes to the symbol like this:
        //
        //    {before_date="2024-09-01"}
        //    :post-stats-graph:
        //
        // Then the attributes will be connected to the paragraph.
        //
        // Element attributes isn't supported by jotdown:
        //
        //    :post-stats-graph:{before_date="2024-09-01"}
        //
        // So retrieve the attributes from the paragraph.
        let attrs = match self.parent.next()? {
            Event::Start(Container::Paragraph, attrs) => attrs,
            event => return Some(event),
        };

        let transform = match self.parent.peek()? {
            Event::Symbol(sym) => match SymbolTransform::parse(sym.as_ref()) {
                Some(x) => x,
                None => return Some(Event::Start(Container::Paragraph, attrs)),
            },
            _ => return Some(Event::Start(Container::Paragraph, attrs)),
        };

        // Pop symbol
        self.parent.next();

        if !matches!(self.parent.next()?, Event::End(Container::Paragraph)) {
            panic!("Should have an end paragraph tag");
        };

        for x in transform.transform(&attrs).into_iter().rev() {
            self.event_queue.push(x);
        }
        self.event_queue.pop().or_else(|| self.parent.next())
    }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum SymbolTransform {
    PostStatsGraph,
}

impl SymbolTransform {
    pub fn parse(id: &str) -> Option<Self> {
        match id {
            "post-stats-graph" => Some(Self::PostStatsGraph),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::PostStatsGraph => "post-stats-graph",
        }
    }

    fn try_transform<'a>(self, attrs: &Attributes) -> Result<Vec<Event<'a>>> {
        match self {
            Self::PostStatsGraph => create_post_stats_graph(attrs),
        }
    }

    fn transform<'a>(self, attrs: &Attributes) -> Vec<Event<'a>> {
        match self.try_transform(attrs) {
            Ok(res) => res,
            Err(err) => {
                error!("symbol transform error: {err}");
                Vec::new()
            }
        }
    }
}

fn create_post_stats_graph<'a>(attrs: &Attributes) -> Result<Vec<Event<'a>>> {
    let before_date_filter = if let Some(date) = attrs.get_value("before_date") {
        Some(NaiveDate::parse_from_str(
            date.to_string().as_str(),
            "%Y-%m-%d",
        )?)
    } else {
        None
    };

    let graph = PostStatsGraph {
        before_date: before_date_filter,
        caption: attrs.get_value("caption").map(|x| x.to_string()),
    }
    .generate()?;

    let html = Container::RawBlock { format: "html" };

    Ok(vec![
        Event::Start(html.clone(), Attributes::new()),
        Event::Str(graph.into()),
        Event::End(html),
    ])
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use eyre::Result;
//     use jotdown::{html, Parser, Render};
//
//     fn convert(s: &str) -> Result<String> {
//         let parser = Parser::new(s);
//         let transformed = SymbolTransforms::new(parser);
//         let mut body = String::new();
//         html::Renderer::default().push(transformed, &mut body)?;
//         Ok(body)
//     }
//
//     #[test]
//     fn test_parse_stats_graph() -> Result<()> {
//         let s = r#"
// {before_date="2009-08-01"}
// :post-stats-graph:
// "#;
//         assert_eq!(
//             convert(s)?,
//             r#"<aside class="note">
// <p>Text here</p>
// </aside>
// "#
//         );
//
//         Ok(())
//     }
// }
