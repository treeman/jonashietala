use crate::content::CountedWordsPostItem;
use crate::markup;
use crate::paths::AbsPath;
use chrono::{DateTime, Datelike, NaiveDate, NaiveDateTime, NaiveTime};
use eyre::Result;
use itertools::join;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::cmp::{max, min};
use std::collections::HashSet;

#[allow(dead_code)]
pub fn post_stats_graph_custom(
    before_date: Option<NaiveDate>,
    caption: Option<String>,
) -> Result<String> {
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
    let label_margin_y = 65;

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

    let mut label_column = 0;
    let mut label_row = 0;

    let mut data_sets = Vec::new();

    let background_color = "#f1f1f1";
    let grid_color = "#a98a78";
    let text_color = "#54433a";

    for group in groups.groups {
        let color = group.default_color;
        let points = join(
            group.posts.into_iter().map(|post| {
                let x = date_to_x(&post.created);
                let y = word_count_to_y(post.word_count);
                let val = post.word_count;
                let title = format!("{}: {} words", post.title, post.word_count);
                let href = post.url.href();
                format!(
                    r#"<a href="{href}">
    <circle cx="{x}" cy="{y}" data-value="{val}" r="{point_radius}" fill="{color}">
      <title>{title}</title>
    </circle>
    </a>"#
                )
            }),
            "\n",
        );
        let label_width = 140;
        let label_height = 25;
        let num_labels_in_row = 5;
        let x_box_offset = 14;
        let y_box_offset = 10;
        let x = plot_left + label_column * label_width + x_box_offset;
        let y = plot_bottom + 50 + label_row * label_height;
        let title = group.name;
        let box_x = x - x_box_offset;
        let box_y = y - y_box_offset;
        let box_w = 10;
        label_column += 1;
        if label_column == num_labels_in_row {
            label_column = 0;
            label_row += 1;
        }
        let label = format!(
            r#"<rect x="{box_x}" y="{box_y}" width="{box_w}" height="{box_w}" fill="{color}"
                            class="data-set-label-box" />
    <text x="{x}" y="{y}" class="data-set-label" fill="{text_color}">{title}</text>"#
        );
        let class = group.id;
        data_sets.push(format!(
            r#"<g class="data-set {class}">
      {points}
      {label}
    </g>"#
        ));
    }

    // Reverse to place points on top
    let data_sets = join(data_sets.into_iter().rev(), "\n");

    let caption = if let Some(x) = caption {
        format!("<figcaption>{x}</figcaption>")
    } else {
        "".into()
    };

    let custom_s = format!(
        r#"
  <figure class="plot-wrapper">
    <svg version="1.2" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" class="blog-stats-graph" role="img"
        viewBox="0 0 {viewbox_w} {viewbox_h}">
    <g class="grid x-axis">
      <line x1="{plot_left}" x2="{plot_right}" y1="{plot_bottom}" y2="{plot_bottom}" fill="{grid_color}"></line>
    </g>
    <g class="grid y-grid">
      <line x1="{plot_left}" x2="{plot_left}" y1="{plot_top}" y2="{plot_bottom}" fill="{grid_color}"></line>
    </g>
    <g class="labels x-labels">
      {x_ticks}
    </g>
    <g class="labels y-labels">
      {y_ticks}
      <text x="{y_label_x}" y="{padding}" class="label-title" fill="{text_color}">Words</text>
    </g>
    {data_sets}
    </svg>
    {caption}
  </figure>
        "#
    );

    Ok(custom_s)
}

#[derive(Debug)]
struct Groups {
    groups: Vec<Group>,
}

impl Groups {
    fn new() -> Self {
        Self {
            groups: vec![
                Group::tagged("3D printing", "printing", "#be79bb", &["3D printing"]),
                Group::tagged("Keyboards", "keyboards", "#3d6568", &["Keyboards"]),
                Group::tagged(
                    "Book writing",
                    "crypto",
                    "#a06d00",
                    &["Why Cryptocurrencies?"],
                ),
                Group::tagged(
                    "Game creation",
                    "games",
                    "#7892bd",
                    &[
                        "Experimental Gameplay Project",
                        "Ludum Dare",
                        "Games",
                        "Game Design Course",
                    ],
                ),
                // Group::tagged(
                //     "Game Design course",
                //     "game_design_course",
                //     &["Game Design Course"],
                // ),
                Group::tagged(
                    "Yearly Review",
                    "yearly_review",
                    "#54433a",
                    &["Yearly Review"],
                ),
                Group::tagged(
                    "Programming",
                    "programming",
                    "#bf0021",
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
                Group::tagged("Life", "life", "#bc5c00", &["Life", "School"]),
                Group::tagged("Gaming", "gaming", "#739797", &["Gaming", "Netrunner"]),
                Group::tagged(
                    "Linux",
                    "linux",
                    "#904180",
                    &["Linux", "Slackware", "Void Linux"],
                ),
                Group::fallback("Other", "fallback", "#a98a78"),
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
    name: &'static str,
    series: HashSet<&'static str>,
    tags: HashSet<&'static str>,
    posts: Vec<CountedWordsPostItem>,
    default_color: &'static str,
    fallback: bool,
}

impl Group {
    #[allow(dead_code)]
    fn series(
        name: &'static str,
        id: &'static str,
        default_color: &'static str,
        series: &[&'static str],
    ) -> Self {
        Self {
            id,
            name,
            series: series.iter().copied().collect(),
            default_color,
            ..Default::default()
        }
    }

    fn tagged(
        name: &'static str,
        id: &'static str,
        default_color: &'static str,
        tags: &[&'static str],
    ) -> Self {
        Self {
            id,
            name,
            tags: tags.iter().copied().collect(),
            default_color,
            ..Default::default()
        }
    }

    fn fallback(name: &'static str, id: &'static str, default_color: &'static str) -> Self {
        Self {
            id,
            name,
            fallback: true,
            default_color,
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
                // if post.series_id == Some("voron_trident".to_string()) {
                //     dbg!(&tag);
                //     dbg!(&self.tags);
                // }
                if self.tags.contains(tag.id.as_str()) {
                    return true;
                }
            }
        }
        self.fallback
    }
}
