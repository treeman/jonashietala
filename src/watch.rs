use axum::{routing::get_service, Router};
use eyre::Result;
use flume::{Receiver, Sender};
use futures_util::{SinkExt, StreamExt};
use hotwatch::Hotwatch;
use serde::{Deserialize, Serialize};
use std::{net::SocketAddr, thread, time::Duration};
// use tokio::io::AsyncReadExt;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::net::{TcpListener, TcpStream};
use tokio_tungstenite::{accept_async, tungstenite::Message};
use tower_http::{services::ServeDir, trace::TraceLayer};
use tracing::{debug, error, info};

use crate::paths::AbsPath;
use crate::site::{Site, SiteOptions};

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
enum NeovimEvent {
    CursorMoved {
        linenum: u32,
        linecount: u32,
        column: u32,
        path: String,
    },
}

impl Into<JsEvent> for NeovimEvent {
    fn into(self) -> JsEvent {
        match self {
            Self::CursorMoved {
                linenum,
                linecount,
                path,
                ..
            } => JsEvent::PositionPage {
                linenum,
                linecount,
                path,
            },
        }
    }
}

pub async fn watch(output_dir: &AbsPath, current_dir: &AbsPath) -> Result<()> {
    let mut site = Site::load_content(SiteOptions {
        output_dir: output_dir.clone(),
        input_dir: current_dir.clone(),
        clear_output_dir: true,
        include_drafts: true,
        generate_feed: false,
        include_js: true,
    })?;

    site.render_all()?;

    let (tx, rx) = flume::unbounded::<JsEvent>();
    site.set_notifier(tx.clone());
    start_ws(rx).await?;
    start_neovim_listener(tx).await?;
    start_hotwatch(site);
    start_watch_server(output_dir).await?;

    Ok(())
}

fn start_hotwatch(mut site: Site) {
    tokio::task::spawn_blocking(move || {
        let mut hotwatch = Hotwatch::new_with_custom_delay(Duration::from_millis(100))
            .expect("hotwatch failed to initialize!");

        hotwatch
            .watch(".", move |event| {
                if let Err(err) = site.file_changed(event) {
                    error!("{err}");
                }
            })
            .expect("failed to watch folder!");

        loop {
            thread::sleep(Duration::from_secs(1));
        }
    });
}

async fn run_ws_server(stream: TcpStream, rx: Receiver<JsEvent>) -> Result<()> {
    let peer = stream.peer_addr()?;
    let ws_stream = accept_async(stream).await?;
    info!("ws connection from: {peer}");
    let (mut ws_sender, mut ws_receiver) = ws_stream.split();

    loop {
        tokio::select! {
            msg = ws_receiver.next() => {
                match msg {
                    Some(Ok(msg)) => {
                        if msg.is_close() {
                            debug!("ws connection closed");
                            break;
                        }
                    },
                    Some(Err(err)) => {
                        error!("ws error: {err:?}, closing ws");
                        break;
                    },
                    None => {
                        error!("Got None msg, closing ws");
                        break;
                    }
                }
            }
            msg = rx.recv_async() => {
                match msg {
                    Ok(msg) => {
                        debug!("Got internal event: {msg:?}");
                        ws_sender.send(Message::text(serde_json::to_string(&msg)?)).await?;
                    },
                    err => error!("Error receiving internal event: {err:?}"),
                }
            }
        }
    }

    Ok(())
}

async fn start_ws(rx: Receiver<JsEvent>) -> Result<()> {
    let addr = "127.0.0.1:8081";
    let listener = TcpListener::bind(addr).await?;

    tokio::spawn(async move {
        info!("events socket on: {addr}");

        loop {
            match listener.accept().await {
                Ok((stream, _)) => {
                    let rx = rx.clone();
                    tokio::spawn(async move {
                        if let Err(e) = run_ws_server(stream, rx).await {
                            error!("Websocket error: {e:?}");
                        }
                    });
                }
                Err(e) => error!("Listener error: {e:?}"),
            }
        }
    });

    Ok(())
}

async fn run_neovim_listener(stream: TcpStream, tx: Sender<JsEvent>) -> Result<()> {
    let peer = stream.peer_addr()?;
    info!("tcp connection from: {peer}");
    let mut reader = BufReader::new(stream);
    loop {
        let mut s = String::new();
        reader.read_line(&mut s).await?;
        if s.is_empty() {
            debug!("Tcp connection closed");
            return Ok(());
        } else {
            // Maybe don't error out hard...?
            let event = serde_json::from_str::<NeovimEvent>(&s)?;
            dbg!(&event);
            tx.send(event.into())?;
        }
    }
}

async fn start_neovim_listener(tx: Sender<JsEvent>) -> Result<()> {
    let addr = "127.0.0.1:8082";
    let listener = TcpListener::bind(addr).await?;

    tokio::spawn(async move {
        info!("tcp socket on: {addr}");
        loop {
            match listener.accept().await {
                Ok((stream, _)) => {
                    let tx = tx.clone();
                    tokio::spawn(async move {
                        if let Err(e) = run_neovim_listener(stream, tx).await {
                            error!("Neovim listener error: {e:?}");
                        }
                    });
                }
                Err(e) => error!("Listener error: {e:?}"),
            }
        }
    });

    Ok(())
}

async fn start_watch_server(output_dir: &AbsPath) -> Result<()> {
    let app: _ = Router::new()
        .fallback(get_service(ServeDir::new(&*output_dir)))
        .layer(TraceLayer::new_for_http());

    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));

    info!("serving site on {addr}");
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}
