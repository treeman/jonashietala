use crate::content::CountedWordsPostItem;
use crate::markup::{self};
use crate::paths::AbsPath;
use chrono::{DateTime, Datelike, NaiveDate, NaiveDateTime, NaiveTime};
use eyre::Result;
use itertools::join;
use jotdown::{Attributes, Container, Event};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::cmp::{max, min};
use std::collections::HashSet;
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

#[derive(Debug)]
enum SymbolTransform {
    PostStatsGraph,
}

impl SymbolTransform {
    fn parse(id: &str) -> Option<Self> {
        match id {
            "post-stats-graph" => Some(Self::PostStatsGraph),
            _ => None,
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
                error!("{}", err);
                Vec::new()
            }
        }
    }
}

fn create_post_stats_graph<'a>(attrs: &Attributes) -> Result<Vec<Event<'a>>> {
    let base = AbsPath::current_dir().unwrap();
    let dir = base.join("posts");

    let before_date_filter = if let Some(date) = attrs.get("before_date") {
        Some(NaiveDate::parse_from_str(
            date.to_string().as_str(),
            "%Y-%m-%d",
        )?)
    } else {
        None
    };

    // Yeah, maybe we could refactor things so that we don't read the files again,
    // but that's a surprising amount of work and I don't feel that it's worth it at the moment.
    let posts = markup::find_markup_files(base, &[dir])
        .par_iter()
        .map(|path| CountedWordsPostItem::from_file(path.abs_path()))
        .filter(|file| {
            if let Ok(file) = file {
                if let Some(date) = before_date_filter {
                    file.created <= date
                } else {
                    true
                }
            } else {
                true
            }
        })
        .collect::<Result<Vec<_>>>()?;

    let plot_max_y = 6800;

    let mut groups = Groups::new();

    let mut first_date = NaiveDate::from_ymd_opt(3000, 1, 1).unwrap();
    let mut last_date = NaiveDate::from_ymd_opt(2000, 1, 1).unwrap();
    for post in posts.into_iter() {
        if post.word_count > plot_max_y {
            panic!("You madman, you've created a post that's too long! Please update the y-axis labels to be pretty again.");
        }

        first_date = min(first_date, post.created);
        last_date = max(last_date, post.created);

        groups.insert(post);
    }

    let viewbox_w = 800;
    let viewbox_h = 600;

    let label_margin_x = 50;
    let label_margin_y = 40;

    let padding = 30;

    let plot_w = viewbox_w - label_margin_x - padding * 2;
    let plot_h = viewbox_h - label_margin_y - padding * 2;

    let plot_left = label_margin_x + padding;
    let plot_right = viewbox_w - padding;
    let plot_top = padding;
    let plot_bottom = plot_h + padding;

    let word_count_to_y = |count| {
        // Transform to 0..plot_h
        // let remap = (count as f32 / plot_max_y as f32) as u32 * plot_h;
        let remap = ((count as f32 / plot_max_y as f32) * plot_h as f32) as u32;
        // Invert (y = 0 is at the top)
        let inverted = plot_h - remap;
        // Offset from top padding
        inverted + padding
    };

    let y_label_x = plot_left - 10;

    let y_ticks = join(
        [0, 1000, 2000, 3000, 4000, 5000, 6000]
            .into_iter()
            .map(|count| {
                let x = y_label_x;
                let y = word_count_to_y(count) + 4;
                format!(r#"<text x="{x}" y="{y}">{count}</text>"#)
            }),
        "\n",
    );

    let first_unix_time = first_date
        .and_time(NaiveTime::from_hms_opt(0, 0, 0).unwrap())
        .and_utc()
        .timestamp();
    let last_unix_time = last_date
        .and_time(NaiveTime::from_hms_opt(23, 59, 59).unwrap())
        .and_utc()
        .timestamp();
    let datetime_to_x = |date: &NaiveDateTime| {
        let unix_timestamp: i64 = date.and_utc().timestamp();
        // Transform to 0..plot_w
        let remap = (((last_unix_time - unix_timestamp) as f64
            / (last_unix_time - first_unix_time) as f64)
            * plot_w as f64) as u32;
        // Invert
        let inverted = plot_w - remap;
        // Offset
        inverted + padding + label_margin_x
    };
    let date_to_x =
        |date: &NaiveDate| datetime_to_x(&date.and_time(NaiveTime::from_hms_opt(0, 0, 0).unwrap()));

    // How many years should we display as labels?
    let x_tick_count = 7;

    let x_ticks = join(
        (0..x_tick_count)
            .map(|i| {
                // Interpolates and creates a date between the first and last post
                let t = (i as f32) / (x_tick_count as f32 - 1.0);
                let unix_time =
                    first_unix_time + (t * (last_unix_time - first_unix_time) as f32) as i64;
                DateTime::from_timestamp(unix_time, 0)
                    .expect("Failed to create date from unix time")
                    .naive_local()
                    .date()
            })
            .filter_map(|date| {
                if date < first_date {
                    return None;
                }

                let y = plot_bottom + 24;
                let x = date_to_x(&date);
                let year = date.year();
                Some(format!(r#"<text x="{x}" y="{y}">{year}</text>"#))
            }),
        "\n",
    );

    let point_radius = 3.5;

    let data_sets = join(
        groups.groups.into_iter().rev().map(|group| {
            let points = join(
                group.posts.into_iter().map(|post| {
                    let x = date_to_x(&post.created);
                    let y = word_count_to_y(post.word_count);
                    let val = post.word_count;
                    let title = format!("{}: {}", post.title, post.word_count);
                    let href = post.url.href();
                    format!(
                        r#"<a href="{href}">
<circle cx="{x}" cy="{y}" data-value="{val}" r="{point_radius}">
  <title>{title}</title>
</circle>
</a>"#
                    )
                }),
                "\n",
            );
            let label = if group.id == "games" {
                let x = plot_left;
                let y = plot_bottom + 45;
                let title = group.id;
                let box_x = x - 12;
                let box_y = y - 8;
                let box_w = 10;
                format!(
                    r#"<rect x="{box_x}" y="{box_y}" width="{box_w}" height="{box_w}"
                        class="data-set-label-box" />
<text x="{plot_left}" y="{y}" class="data-set-label">{title}</text>"#
                )
            } else {
                "".into()
            };
            let class = group.id;
            format!(
                r#"<g class="data-set {class}">
  {points}
  {label}
</g>"#
            )
        }),
        "\n",
    );

    let html = Container::RawBlock { format: "html" };
    let mut res = Vec::new();
    res.push(Event::Start(html.clone(), Attributes::new()));
    res.push(Event::Str(format!(
        r#"
<svg version="1.2" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" class="blog-stats-graph" role="img"
    viewBox="0 0 {viewbox_w} {viewbox_h}">
<g class="grid x-axis">
  <line x1="{plot_left}" x2="{plot_right}" y1="{plot_bottom}" y2="{plot_bottom}"></line>
</g>
<g class="grid y-grid">
  <line x1="{plot_left}" x2="{plot_left}" y1="{plot_top}" y2="{plot_bottom}"></line>
</g>
<g class="labels x-labels">
  {x_ticks}
</g>
<g class="labels y-labels">
  {y_ticks}
  <text x="{y_label_x}" y="{padding}" class="label-title">Words</text>
</g>
{data_sets}
</svg>
    "#
    ).into()));
    res.push(Event::End(html));

    Ok(res)
}

#[derive(Debug)]
struct Groups {
    groups: Vec<Group>,
}

impl Groups {
    fn new() -> Self {
        Self {
            groups: vec![
                Group::series("voron_trident", &["voron_trident"]),
                Group::series("t-34", &["t-34"]),
                Group::tagged("crypto", &["Why Cryptocurrencies?"]),
                Group::tagged("games", &["Experimental Gameplay Project", "Ludum Dare"]),
                Group::tagged("game_design_course", &["Game Design Course"]),
                Group::tagged("yearly_review", &["Yearly Review"]),
                Group::tagged(
                    "programming",
                    &[
                        "Rust",
                        "Neovim",
                        "Lua",
                        "Racket",
                        "CSS",
                        "Elixir",
                        "Gleam",
                        "Programming",
                    ],
                ),
                Group::tagged("life", &["Life", "School"]),
                Group::tagged("gaming", &["Gaming", "Netrunner"]),
                Group::tagged("linux", &["Linux", "Slackware", "Void Linux"]),
                Group::fallback("fallback"),
            ],
        }
    }

    fn insert(&mut self, post: CountedWordsPostItem) {
        for group in self.groups.iter_mut() {
            if group.accept(&post) {
                group.posts.push(post);
                return;
            }
        }
        panic!("No group fallback #{post:?}");
    }
}

#[derive(Default, Debug)]
struct Group {
    id: &'static str,
    series: HashSet<&'static str>,
    tags: HashSet<&'static str>,
    posts: Vec<CountedWordsPostItem>,
    fallback: bool,
}

impl Group {
    fn series(id: &'static str, series: &[&'static str]) -> Self {
        Self {
            id,
            series: series.iter().copied().collect(),
            ..Default::default()
        }
    }

    fn tagged(id: &'static str, tags: &[&'static str]) -> Self {
        Self {
            id,
            tags: tags.iter().copied().collect(),
            ..Default::default()
        }
    }

    fn fallback(id: &'static str) -> Self {
        Self {
            id,
            fallback: true,
            ..Default::default()
        }
    }

    fn accept(&self, post: &CountedWordsPostItem) -> bool {
        if let Some(id) = &post.series_id {
            dbg!(&id);
            if self.series.contains(id.as_str()) {
                return true;
            }
        }
        if !self.tags.is_empty() {
            for tag in &post.tags {
                if self.tags.contains(tag.id.as_str()) {
                    return true;
                }
            }
        }
        self.fallback
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<String> {
        let parser = Parser::new(s);
        let transformed = SymbolTransforms::new(parser);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body)
    }

    #[test]
    fn test_parse_stats_graph() -> Result<()> {
        let s = r#"
{before_date="2009-08-01"}
:post-stats-graph:
"#;
        assert_eq!(
            convert(s)?,
            r#"<aside class="note">
<p>Text here</p>
</aside>
"#
        );

        Ok(())
    }
}
