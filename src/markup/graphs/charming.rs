// use crate::content::CountedWordsPostItem;
// use crate::markup;
// use crate::paths::AbsPath;
// use charming::component::{Axis, Legend};
// use charming::element::{
//     AxisLabel, Color, Emphasis, EmphasisFocus, Formatter, Label, LabelPosition, SplitLine,
// };
// #[allow(unused_imports)]
// use charming::{
//     datatype::datapoint::DataPoint, datatype::CompositeValue, df, element::axis_type::AxisType,
//     series::Scatter, Chart, HtmlRenderer, ImageFormat, ImageRenderer,
// };
// use chrono::{NaiveDate, NaiveTime};
// use eyre::Result;
// use itertools::join;
// use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
// use std::cmp::{max, min};
// use std::collections::HashSet;
//
// #[allow(dead_code)]
// pub fn post_stats_graph_charming(before_date: Option<NaiveDate>) -> Result<String> {
//     let base = AbsPath::current_dir().unwrap();
//     let dir = base.join("posts");
//
//     // Yeah, maybe we could refactor things so that we don't read the files again,
//     // but that's a surprising amount of work and I don't feel that it's worth it at the moment.
//     let posts = markup::find_markup_files(base, &[dir])
//         .par_iter()
//         .map(|path| CountedWordsPostItem::from_file(path.abs_path()))
//         .filter(|file| {
//             if let Ok(file) = file {
//                 if let Some(date) = before_date {
//                     file.created <= date
//                 } else {
//                     true
//                 }
//             } else {
//                 true
//             }
//         })
//         .collect::<Result<Vec<_>>>()?;
//
//     let plot_max_y = 6800;
//
//     let mut groups = Groups::new();
//
//     let mut first_date = NaiveDate::from_ymd_opt(3000, 1, 1).unwrap();
//     let mut last_date = NaiveDate::from_ymd_opt(2000, 1, 1).unwrap();
//     for post in posts.into_iter() {
//         if post.word_count > plot_max_y {
//             panic!("You madman, you've created a post that's too long! Please update the y-axis labels to be pretty again.");
//         }
//
//         first_date = min(first_date, post.created);
//         last_date = max(last_date, post.created);
//
//         groups.insert(post);
//     }
//
//     let viewbox_w = 800;
//     let viewbox_h = 600;
//
//     let label_margin_x = 50;
//     let label_margin_y = 40;
//
//     let padding = 30;
//
//     let _plot_w = viewbox_w - label_margin_x - padding * 2;
//     let plot_h = viewbox_h - label_margin_y - padding * 2;
//
//     let plot_left = label_margin_x + padding;
//     let _plot_right = viewbox_w - padding;
//     let _plot_top = padding;
//     let _plot_bottom = plot_h + padding;
//
//     let word_count_to_y = |count| {
//         // Transform to 0..plot_h
//         // let remap = (count as f32 / plot_max_y as f32) as u32 * plot_h;
//         let remap = ((count as f32 / plot_max_y as f32) * plot_h as f32) as u32;
//         // Invert (y = 0 is at the top)
//         let inverted = plot_h - remap;
//         // Offset from top padding
//         inverted + padding
//     };
//
//     let y_label_x = plot_left - 10;
//
//     let _y_ticks = join(
//         [0, 1000, 2000, 3000, 4000, 5000, 6000]
//             .into_iter()
//             .map(|count| {
//                 let x = y_label_x;
//                 let y = word_count_to_y(count) + 4;
//                 format!(r#"<text x="{x}" y="{y}">{count}</text>"#)
//             }),
//         "\n",
//     );
//
//     let first_unix_time = first_date
//         .and_time(NaiveTime::from_hms_opt(0, 0, 0).unwrap())
//         .and_utc()
//         .timestamp();
//     let last_unix_time = last_date
//         .and_time(NaiveTime::from_hms_opt(23, 59, 59).unwrap())
//         .and_utc()
//         .timestamp();
//     let date_to_unix = |date: &NaiveDate| {
//         let dt = &date.and_time(NaiveTime::from_hms_opt(0, 0, 0).unwrap());
//         dt.and_utc().timestamp()
//     };
//
//     let mut chart = Chart::new()
//         .legend(Legend::new().bottom(0))
//         .x_axis(
//             Axis::new()
//                 .min(first_unix_time)
//                 .max(last_unix_time)
//                 .name("Year")
//                 .split_line(SplitLine::new().show(false))
//                 .axis_label(
//                     AxisLabel::new().formatter(Formatter::Function(
//                         r#"function (ts) {
//                             const date = new Date(ts * 1000);
//                             //return date.toISOString();
//                             const year = date.getFullYear();
//                             return year;
//                         }"#
//                         .into(),
//                     )),
//                 ),
//         )
//         .y_axis(
//             Axis::new()
//                 .name("Word count")
//                 .split_line(SplitLine::new().show(false)), // .data(x_data),
//         );
//
//     for group in groups.groups {
//         let data: Vec<_> = group
//             .posts
//             .into_iter()
//             .map(|post| {
//                 let x = date_to_unix(&post.created);
//                 // let x = post.created.format("%Y-%m-%d").to_string();
//                 // let y = post.word_count as f32;
//                 let y = post.word_count;
//                 DataPoint::from(CompositeValue::from(vec![
//                     CompositeValue::from(x),
//                     CompositeValue::from(y as f32),
//                 ]))
//             })
//             .collect();
//
//         dbg!(&data);
//
//         chart = chart.series(
//             Scatter::new()
//                 .symbol_size(8)
//                 .data(data)
//                 .name(format!("{}: XXX", group.id))
//                 .id(group.id)
//                 .emphasis(
//                     Emphasis::new()
//                         .label(
//                             Label::new()
//                                 .show(true)
//                                 .position(LabelPosition::Outside)
//                                 .color(Color::from("#fff")),
//                         )
//                         .focus(EmphasisFocus::Series),
//                 ), // .item_style(
//                    //     ItemStyle::new()
//                    //         .shadow_blur(10)
//                    //         .shadow_color("rgba(120, 36, 50, 0.5)")
//                    //         .shadow_offset_y(5)
//                    //         .color(Color::RadialGradient {
//                    //             x: 0.4,
//                    //             y: 0.3,
//                    //             r: 1.,
//                    //             color_stops: vec![
//                    //                 ColorStop::new(0, "rgb(251, 118, 123)"),
//                    //                 ColorStop::new(1, "rgb(204, 46, 72)"),
//                    //             ],
//                    //         }),
//                    // ),
//         );
//     }
//
//     let mut renderer = ImageRenderer::new(viewbox_w, viewbox_h);
//     // let renderer = HtmlRenderer::new("my-chart", viewbox_w as u64, viewbox_h as u64);
//     let chart_s = renderer.render(&chart).unwrap();
//
//     Ok(format!(r#"<div class="charts-wrapper">{}</div>"#, chart_s))
// }
//
// #[derive(Debug)]
// struct Groups {
//     groups: Vec<Group>,
// }
//
// impl Groups {
//     fn new() -> Self {
//         Self {
//             groups: vec![
//                 Group::series("voron_trident", &["voron_trident"]),
//                 Group::series("t-34", &["t-34"]),
//                 Group::tagged("crypto", &["Why Cryptocurrencies?"]),
//                 Group::tagged(
//                     "games",
//                     &["Experimental Gameplay Project", "Ludum Dare", "Games"],
//                 ),
//                 Group::tagged("game_design_course", &["Game Design Course"]),
//                 Group::tagged("yearly_review", &["Yearly Review"]),
//                 Group::tagged(
//                     "programming",
//                     &[
//                         "Rust",
//                         "Neovim",
//                         "Lua",
//                         "Racket",
//                         "CSS",
//                         "Elixir",
//                         "Gleam",
//                         "Programming",
//                     ],
//                 ),
//                 Group::tagged("life", &["Life", "School"]),
//                 Group::tagged("gaming", &["Gaming", "Netrunner"]),
//                 Group::tagged("linux", &["Linux", "Slackware", "Void Linux"]),
//                 Group::fallback("fallback"),
//             ],
//         }
//     }
//
//     fn insert(&mut self, post: CountedWordsPostItem) {
//         for group in self.groups.iter_mut() {
//             if group.accept(&post) {
//                 group.posts.push(post);
//                 return;
//             }
//         }
//         panic!("No group fallback #{post:?}");
//     }
// }
//
// #[derive(Default, Debug)]
// struct Group {
//     id: &'static str,
//     series: HashSet<&'static str>,
//     tags: HashSet<&'static str>,
//     posts: Vec<CountedWordsPostItem>,
//     fallback: bool,
// }
//
// impl Group {
//     fn series(id: &'static str, series: &[&'static str]) -> Self {
//         Self {
//             id,
//             series: series.iter().copied().collect(),
//             ..Default::default()
//         }
//     }
//
//     fn tagged(id: &'static str, tags: &[&'static str]) -> Self {
//         Self {
//             id,
//             tags: tags.iter().copied().collect(),
//             ..Default::default()
//         }
//     }
//
//     fn fallback(id: &'static str) -> Self {
//         Self {
//             id,
//             fallback: true,
//             ..Default::default()
//         }
//     }
//
//     fn accept(&self, post: &CountedWordsPostItem) -> bool {
//         if let Some(id) = &post.series_id {
//             if self.series.contains(id.as_str()) {
//                 return true;
//             }
//         }
//         if !self.tags.is_empty() {
//             for tag in &post.tags {
//                 if self.tags.contains(tag.id.as_str()) {
//                     return true;
//                 }
//             }
//         }
//         self.fallback
//     }
// }
