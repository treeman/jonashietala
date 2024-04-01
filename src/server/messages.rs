use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum JsEvent {
    RefreshAll,
    RefreshPage {
        path: String,
    },
    PositionPage {
        path: String,
        linenum: u32,
        linecount: u32,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "id")]
pub enum NeovimEvent {
    CursorMoved {
        linenum: u32,
        linecount: u32,
        column: u32,
        path: String,
    },
    ListTags,
    ListUrls,
    ListPosts,
}

#[derive(Debug, Serialize)]
#[serde(tag = "id")]
pub enum NeovimResponse {}

// impl NeovimEvent {
//     fn to_js_event(self, site: Arc<Mutex<Site>>) -> Result<JsEvent> {
//         match self {
//             // TODO Use https://github.com/lotabout/fuzzy-matcher
//             // to calculate a better match with markup context
//             Self::CursorMoved {
//                 linenum,
//                 linecount,
//                 path,
//                 ..
//             } => {
//                 let site = site.lock().expect("To JsEvent failed");
//                 let path = Utf8PathBuf::from(path);
//                 // This works:
//                 // dbg!(&site.content.find_post(&path.file_name().unwrap()));
//                 // dbg!(&site
//                 //     .content
//                 //     .find_post(&site.file_path(path.clone().into())?.rel_path.0.as_str()));
//                 Ok(JsEvent::PositionPage {
//                     linenum,
//                     linecount,
//                     path: path.to_string(),
//                 })
//             }
//         }
//     }
// }
