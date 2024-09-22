#![allow(unused_imports)]
#![allow(unused_variables)]
use crate::content::CountedWordsPostItem;
use crate::markup;
use crate::paths::AbsPath;
use charming::component::{Axis, Legend};
use charming::element::{
    AxisLabel, Color, Emphasis, EmphasisFocus, Formatter, Label, LabelPosition, SplitLine,
};
use charming::{
    datatype::datapoint::DataPoint, datatype::CompositeValue, df, element::axis_type::AxisType,
    series::Scatter, Chart, HtmlRenderer, ImageFormat, ImageRenderer,
};
use chrono::{DateTime, Datelike, NaiveDate, NaiveDateTime, NaiveTime};
use eyre::Result;
use itertools::join;
use jotdown::{Attributes, Container, Event};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::cmp::{max, min};
use std::collections::HashSet;

pub fn post_stats_graph(before_date: Option<NaiveDate>) -> Result<String> {
    // More alternatives:
    // https://github.com/vicanso/charts-rs
    // https://github.com/plotters-rs/plotters
    // https://github.com/askanium/rustplotlib?tab=readme-ov-file
    // https://github.com/plotly/plotly.rs <- with meta
    post_stats_graph_custom(before_date)
    // post_stats_graph_charming(before_date)
}

fn post_stats_graph_custom(before_date: Option<NaiveDate>) -> Result<String> {
    let base = AbsPath::current_dir().unwrap();
    let dir = base.join("posts");

    // Yeah, maybe we could refactor things so that we don't read the files again,
    // but that's a surprising amount of work and I don't feel that it's worth it at the moment.
    let posts = markup::find_markup_files(base, &[dir])
        .par_iter()
        .map(|path| CountedWordsPostItem::from_file(path.abs_path()))
        .filter(|file| {
            if let Ok(file) = file {
                if let Some(date) = before_date {
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
    let date_to_unix = |date: &NaiveDate| {
        let dt = &date.and_time(NaiveTime::from_hms_opt(0, 0, 0).unwrap());
        dt.and_utc().timestamp()
    };

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

    let custom_s = format!(
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
    );

    Ok(custom_s)
}

fn post_stats_graph_charming(before_date: Option<NaiveDate>) -> Result<String> {
    let base = AbsPath::current_dir().unwrap();
    let dir = base.join("posts");

    // Yeah, maybe we could refactor things so that we don't read the files again,
    // but that's a surprising amount of work and I don't feel that it's worth it at the moment.
    let posts = markup::find_markup_files(base, &[dir])
        .par_iter()
        .map(|path| CountedWordsPostItem::from_file(path.abs_path()))
        .filter(|file| {
            if let Ok(file) = file {
                if let Some(date) = before_date {
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
    let date_to_unix = |date: &NaiveDate| {
        let dt = &date.and_time(NaiveTime::from_hms_opt(0, 0, 0).unwrap());
        dt.and_utc().timestamp()
    };

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

    // More alternatives:
    // https://github.com/vicanso/charts-rs
    // https://github.com/plotters-rs/plotters
    // https://github.com/askanium/rustplotlib?tab=readme-ov-file
    // https://github.com/plotly/plotly.rs <- with meta

    // let data_sets = join(
    //     groups.groups.into_iter().rev().map(|group| {
    //         let points = join(
    //             group.posts.into_iter().map(|post| {
    //                 let x = date_to_x(&post.created);
    //                 let y = word_count_to_y(post.word_count);
    //                 let val = post.word_count;
    //                 let title = format!("{}: {}", post.title, post.word_count);
    //                 let href = post.url.href();
    //                 format!(
    //                     r#"<a href="{href}">
    // <circle cx="{x}" cy="{y}" data-value="{val}" r="{point_radius}">
    //   <title>{title}</title>
    // </circle>
    // </a>"#
    //                 )
    //             }),
    //             "\n",
    //         );
    //         let label = if group.id == "games" {
    //             let x = plot_left;
    //             let y = plot_bottom + 45;
    //             let title = group.id;
    //             let box_x = x - 12;
    //             let box_y = y - 8;
    //             let box_w = 10;
    //             format!(
    //                 r#"<rect x="{box_x}" y="{box_y}" width="{box_w}" height="{box_w}"
    //                         class="data-set-label-box" />
    // <text x="{plot_left}" y="{y}" class="data-set-label">{title}</text>"#
    //             )
    //         } else {
    //             "".into()
    //         };
    //         let class = group.id;
    //         format!(
    //             r#"<g class="data-set {class}">
    //   {points}
    //   {label}
    // </g>"#
    //         )
    //     }),
    //     "\n",
    // );
    //
    // let custom_s = format!(
    //         r#"
    // <svg version="1.2" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" class="blog-stats-graph" role="img"
    //     viewBox="0 0 {viewbox_w} {viewbox_h}">
    // <g class="grid x-axis">
    //   <line x1="{plot_left}" x2="{plot_right}" y1="{plot_bottom}" y2="{plot_bottom}"></line>
    // </g>
    // <g class="grid y-grid">
    //   <line x1="{plot_left}" x2="{plot_left}" y1="{plot_top}" y2="{plot_bottom}"></line>
    // </g>
    // <g class="labels x-labels">
    //   {x_ticks}
    // </g>
    // <g class="labels y-labels">
    //   {y_ticks}
    //   <text x="{y_label_x}" y="{padding}" class="label-title">Words</text>
    // </g>
    // {data_sets}
    // </svg>
    //     "#
    //     ).into();

    let x_data: Vec<_> = (0..x_tick_count)
        .map(|i| {
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

            // Some(date.
            //     .and_time(NaiveTime::from_hms_opt(0, 0, 0).unwrap())
            //     .and_utc()
            //     .timestamp())

            // let y = plot_bottom + 24;
            // let x = date_to_x(&date);
            let year = date.year();
            Some(year.to_string())
            // Some(format!(r#"<text x="{x}" y="{y}">{year}</text>"#))
        })
        .collect();

    let mut chart = Chart::new()
        .legend(Legend::new().bottom(0))
        .x_axis(
            Axis::new()
                .min(first_unix_time)
                .max(last_unix_time)
                .name("Year")
                .split_line(SplitLine::new().show(false))
                .axis_label(
                    AxisLabel::new().formatter(Formatter::Function(
                        r#"function (ts) {
                            const date = new Date(ts * 1000);
                            //return date.toISOString();
                            const year = date.getFullYear();
                            return year;
                        }"#
                        .into(),
                    )),
                ),
        )
        .y_axis(
            Axis::new()
                .name("Word count")
                .split_line(SplitLine::new().show(false)), // .data(x_data),
        );

    for group in groups.groups {
        let data: Vec<_> = group
            .posts
            .into_iter()
            .map(|post| {
                let x = date_to_unix(&post.created);
                // let x = post.created.format("%Y-%m-%d").to_string();
                // let y = post.word_count as f32;
                let y = post.word_count;
                DataPoint::from(CompositeValue::from(vec![
                    CompositeValue::from(x),
                    CompositeValue::from(y as f32),
                ]))
            })
            .collect();

        dbg!(&data);

        chart = chart.series(
            Scatter::new()
                .symbol_size(8)
                .data(data)
                .name(format!("{}: XXX", group.id))
                .id(group.id)
                .emphasis(
                    Emphasis::new()
                        .label(
                            Label::new()
                                .show(true)
                                .position(LabelPosition::Outside)
                                .color(Color::from("#fff")),
                        )
                        .focus(EmphasisFocus::Series),
                ), // .item_style(
                   //     ItemStyle::new()
                   //         .shadow_blur(10)
                   //         .shadow_color("rgba(120, 36, 50, 0.5)")
                   //         .shadow_offset_y(5)
                   //         .color(Color::RadialGradient {
                   //             x: 0.4,
                   //             y: 0.3,
                   //             r: 1.,
                   //             color_stops: vec![
                   //                 ColorStop::new(0, "rgb(251, 118, 123)"),
                   //                 ColorStop::new(1, "rgb(204, 46, 72)"),
                   //             ],
                   //         }),
                   // ),
        );
    }

    let mut renderer = ImageRenderer::new(viewbox_w, viewbox_h);
    // let renderer = HtmlRenderer::new("my-chart", viewbox_w as u64, viewbox_h as u64);
    let chart_s = renderer.render(&chart).unwrap();

    let html = Container::RawBlock { format: "html" };

    // let mut res = Vec::new();
    // res.push(Event::Start(html.clone(), Attributes::new()));
    // // res.push(Event::Str(custom_s));
    // res.push(Event::Str(chart_s.into()));
    // res.push(Event::End(html));

    Ok(format!(r#"<div class="charts-wrapper">{}</div>"#, chart_s))
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
                Group::tagged(
                    "games",
                    &["Experimental Gameplay Project", "Ludum Dare", "Games"],
                ),
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
