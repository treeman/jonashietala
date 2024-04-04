mod handler;
mod messages;

use axum::{routing::get_service, Router};
use axum_server::Server;
// use camino::Utf8PathBuf;
use eyre::Result;
use flume::{Receiver, Sender};
use futures_util::{SinkExt, StreamExt};
use hotwatch::Hotwatch;
use std::sync::{Arc, Mutex};
use std::{net::SocketAddr, thread, time::Duration};
// use tokio::io::AsyncReadExt;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader, BufWriter};
use tokio::net::{TcpListener, TcpStream};
use tokio_tungstenite::{accept_async, tungstenite::Message};
use tower_http::{services::ServeDir, trace::TraceLayer};
use tracing::{debug, error, info};

use crate::paths::AbsPath;
use crate::site::{Site, SiteOptions};

pub use messages::WebEvent;

use handler::Response;
use messages::NeovimEvent;

pub async fn run(output_dir: &AbsPath, current_dir: &AbsPath) -> Result<()> {
    let mut site = Site::load_content(SiteOptions {
        output_dir: output_dir.clone(),
        input_dir: current_dir.clone(),
        clear_output_dir: true,
        include_drafts: true,
        generate_feed: false,
        include_js: true,
    })?;

    site.render_all()?;

    let (tx, rx) = flume::unbounded::<WebEvent>();
    site.set_notifier(tx.clone());

    let site = Arc::new(Mutex::new(site));

    start_web_connection(rx).await?;
    start_neovim_connection(site.clone(), tx).await?;
    start_hotwatch(site);
    start_file_watcher(output_dir).await?;

    Ok(())
}

async fn run_web_connection(stream: TcpStream, rx: Receiver<WebEvent>) -> Result<()> {
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

async fn start_web_connection(rx: Receiver<WebEvent>) -> Result<()> {
    let addr = "127.0.0.1:8081";
    let listener = TcpListener::bind(addr).await?;

    tokio::spawn(async move {
        info!("events socket on: {addr}");

        loop {
            match listener.accept().await {
                Ok((stream, _)) => {
                    let rx = rx.clone();
                    tokio::spawn(async move {
                        if let Err(e) = run_web_connection(stream, rx).await {
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

async fn run_neovim_connection(
    site: Arc<Mutex<Site>>,
    mut stream: TcpStream,
    tx: Sender<WebEvent>,
) -> Result<()> {
    let peer = stream.peer_addr()?;
    info!("tcp connection from: {peer}");
    let (reader, writer) = stream.split();
    let mut reader = BufReader::new(reader);
    let mut writer = BufWriter::new(writer);
    loop {
        let mut s = String::new();
        reader.read_line(&mut s).await?;
        if s.is_empty() {
            debug!("Tcp connection closed");
            return Ok(());
        } else {
            // Maybe don't error out hard...?
            let event = serde_json::from_str::<NeovimEvent>(&s)?;
            match handler::handle_msg(event, site.clone()) {
                Some(Response::Web(msg)) => tx.send(msg)?,
                Some(Response::Reply(msg)) => {
                    println!("Replying...");
                    let json = serde_json::to_string(&msg)?;
                    writer.write_all(json.as_bytes()).await?;
                    writer.write_all("\n".as_bytes()).await?;
                    writer.flush().await?;
                }
                None => {}
            }
            // writer.write_all("hello!".as_bytes());
            // tx.send(event.to_js_event(site.clone())?)?;
        }
    }
}

async fn start_neovim_connection(site: Arc<Mutex<Site>>, tx: Sender<WebEvent>) -> Result<()> {
    let addr = "127.0.0.1:8082";
    let listener = TcpListener::bind(addr).await?;

    tokio::spawn(async move {
        info!("tcp socket on: {addr}");
        loop {
            match listener.accept().await {
                Ok((stream, _)) => {
                    let tx = tx.clone();
                    let site = site.clone();
                    tokio::spawn(async move {
                        if let Err(e) = run_neovim_connection(site, stream, tx).await {
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

fn start_hotwatch(site: Arc<Mutex<Site>>) {
    tokio::task::spawn_blocking(move || {
        let mut hotwatch = Hotwatch::new_with_custom_delay(Duration::from_millis(100))
            .expect("hotwatch failed to initialize!");

        hotwatch
            .watch(".", move |event| {
                let mut site = site.lock().expect("Hotwatch failed");
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

async fn start_file_watcher(output_dir: &AbsPath) -> Result<()> {
    let app: _ = Router::new()
        .fallback(get_service(ServeDir::new(&*output_dir)))
        .layer(TraceLayer::new_for_http());

    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));

    info!("serving site on {addr}");
    Server::bind(addr).serve(app.into_make_service()).await?;

    Ok(())
}