use super::messages::{JsEvent, NeovimEvent, NeovimResponse};

pub enum Response {
    Js(JsEvent),
    Reply(NeovimResponse),
}

pub fn handle_msg(msg: NeovimEvent, site: &Arc<Mutex<Site>>) -> Option<Response> {
    None
}
