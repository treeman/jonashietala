use chrono::NaiveDate;
use eyre::Result;

// mod charming;
mod custom;
// mod plotly;

pub struct PostStatsGraph {
    pub before_date: Option<NaiveDate>,
    pub caption: Option<String>,
}

impl PostStatsGraph {
    pub fn generate(self) -> Result<String> {
        custom::post_stats_graph_custom(self.before_date, self.caption)
    }
}

// More alternatives:
// https://github.com/vicanso/charts-rs
// https://github.com/plotters-rs/plotters
// https://github.com/askanium/rustplotlib?tab=readme-ov-file
// https://github.com/plotly/plotly.rs <- with meta
