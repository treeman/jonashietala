// use std::fs;
//
// use chrono::NaiveDate;
// use eyre::Result;
//
// use plotly::{common::Mode, ImageFormat, Plot, Scatter};
//
// #[allow(dead_code)]
// pub fn post_stats_graph_plotly(_before_date: Option<NaiveDate>) -> Result<String> {
//     // let n: usize = 100;
//     // let t: Vec<f64> = Array::linspace(0., 10., n).into_raw_vec_and_offset().0;
//     // let y: Vec<f64> = t.iter().map(|x| x.sin()).collect();
//     //
//     // let trace = Scatter::new(t, y).mode(Mode::Markers);
//     // let mut plot = Plot::new();
//     // plot.add_trace(trace);
//     //
//     // plot.show();
//
//     let mut plot = Plot::new();
//     let trace = Scatter::new(vec![0, 1, 2], vec![2, 1, 0])
//         .mode(Mode::Markers)
//         // .custom_data(vec!["custom_data"])
//         .name("my_name")
//         .meta("meta");
//     plot.add_trace(trace);
//
//     // plot.write_image("out.png", ImageFormat::PNG, 800, 600, 1.0);
//
//     let file = "/tmp/plot.svg";
//     plot.write_image(file, ImageFormat::SVG, 800, 600, 1.0);
//
//     let data = fs::read_to_string(file)?;
//     dbg!(&data);
//
//     Ok(data)
// }
