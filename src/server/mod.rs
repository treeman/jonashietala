pub mod complete;
pub mod diagnostics;
mod goto_def;
mod handler;
mod info;
pub mod messages;

use crate::paths::AbsPath;
use crate::server::messages::NeovimResponse;
use crate::site::{Site, SiteOptions};
use axum::{routing::get_service, Router};
use axum_server::Server;
use eyre::Result;
use futures_util::{SinkExt, StreamExt};
use handler::Response;
use hotwatch::Hotwatch;
use messages::{NeovimEvent, WebEvent};
use std::sync::{Arc, Mutex};
use std::{net::SocketAddr, thread, time::Duration};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader, BufWriter};
use tokio::net::tcp::ReadHalf;
use tokio::net::tcp::WriteHalf;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::broadcast::{self, error::RecvError, Receiver, Sender};
use tokio_tungstenite::{accept_async, tungstenite::Message};
use tower_http::{services::ServeDir, trace::TraceLayer};
use tracing::{debug, error, info};

pub async fn run(output_dir: &AbsPath, current_dir: &AbsPath) -> Result<()> {
    let mut site = Site::load_content(SiteOptions {
        output_dir: output_dir.clone(),
        input_dir: current_dir.clone(),
        clear_output_dir: true,
        include_drafts: true,
        generate_feed: false,
        include_js: true,
        git_path_offset: None,
    })?;

    site.render_all()?;

    let (web_tx, mut web_rx) = broadcast::channel::<WebEvent>(32);
    let (nvim_tx, mut nvim_rx) = broadcast::channel::<NeovimResponse>(32);

    // These debug listeners are important so that the channels can be kept alive,
    // otherwise sending will fail.
    // (We could ignore those errors as well but we want debug messages anyway.)
    tokio::spawn(async move {
        loop {
            if let Ok(msg) = web_rx.recv().await {
                debug!("Got internal web event: {msg:?}");
            }
        }
    });
    tokio::spawn(async move {
        loop {
            if let Ok(msg) = nvim_rx.recv().await {
                debug!("Got neovim event: {msg:?}");
            }
        }
    });

    site.set_notifiers(web_tx.clone(), nvim_tx.clone());

    let site = Arc::new(Mutex::new(site));

    start_web_connection(web_tx.clone()).await?;
    start_neovim_connection(site.clone(), web_tx, nvim_tx).await?;
    start_hotwatch(site);
    start_file_watcher(output_dir).await?;

    Ok(())
}

async fn run_web_connection(stream: TcpStream, mut rx: Receiver<WebEvent>) -> Result<()> {
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
                            debug!("ws connection closed {peer}");
                            break;
                        }
                    },
                    Some(Err(err)) => {
                        error!("ws error: {err:?}, closing ws {peer}");
                        break;
                    },
                    None => {
                        error!("Got None msg, closing ws {peer}");
                        break;
                    }
                }
            }
            msg = rx.recv() => {
                match msg {
                    Ok(msg) => {
                        ws_sender.send(Message::text(serde_json::to_string(&msg)?)).await?;
                    },
                    err => error!("Error receiving internal event: {err:?} {peer}"),
                }
            }
        }
    }

    Ok(())
}

async fn start_web_connection(tx: Sender<WebEvent>) -> Result<()> {
    let addr = "127.0.0.1:8081";
    let listener = TcpListener::bind(addr).await?;

    tokio::spawn(async move {
        info!("events socket on: {addr}");
        let tx = tx;

        loop {
            match listener.accept().await {
                Ok((stream, _)) => {
                    let rx = tx.subscribe();
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

async fn read_line(reader: &mut BufReader<ReadHalf<'_>>) -> Result<String> {
    let mut s = String::new();
    reader.read_line(&mut s).await?;
    Ok(s)
}

async fn handle_nvim_reply<'a>(
    msg: Result<NeovimResponse, RecvError>,
    writer: &mut BufWriter<WriteHalf<'a>>,
) -> Result<()> {
    let msg = msg?;
    debug!("Sending: {msg:?}");
    let json = serde_json::to_string(&msg)?;
    writer.write_all(json.as_bytes()).await?;
    writer.write_all("\n".as_bytes()).await?;
    writer.flush().await?;
    Ok(())
}

async fn handle_nvim_msg(
    msg: Result<String>,
    site: &Arc<Mutex<Site>>,
    web_tx: &Sender<WebEvent>,
    nvim_tx: &Sender<NeovimResponse>,
) -> Result<bool> {
    let msg = msg?;

    if msg.is_empty() {
        info!("Tcp connection closed");
        Ok(true)
    } else {
        let event = serde_json::from_str::<NeovimEvent>(&msg)?;
        match handler::handle_msg(event, site.clone()) {
            Some(Response::Web(msg)) => web_tx.send(msg)?,
            Some(Response::Reply(msg)) => nvim_tx.send(msg)?,
            None => 0,
        };
        Ok(false)
    }
}

async fn run_neovim_connection(
    site: Arc<Mutex<Site>>,
    mut stream: TcpStream,
    web_tx: Sender<WebEvent>,
    nvim_tx: Sender<NeovimResponse>,
) -> Result<()> {
    let mut nvim_rx = nvim_tx.subscribe();
    let peer = stream.peer_addr()?;
    info!("Tcp connection from: {peer}");
    let (reader, writer) = stream.split();
    let mut reader = BufReader::new(reader);
    let mut writer = BufWriter::new(writer);

    loop {
        tokio::select! {
            msg = nvim_rx.recv() => {
                if let Err(err) = handle_nvim_reply(msg, &mut writer).await {
                    error!("Nvim reply error: {err:?}");
                }
            }
            msg = read_line(&mut reader) => {
                match handle_nvim_msg(msg, &site, &web_tx, &nvim_tx).await {
                    Ok(exit) => if exit {
                        return Ok(());
                    }
                    Err(err) => {
                        error!("Nvim msg error: {err:?}");
                    }
                }
            }
        }
    }
}

async fn start_neovim_connection(
    site: Arc<Mutex<Site>>,
    web_tx: Sender<WebEvent>,
    nvim_tx: Sender<NeovimResponse>,
) -> Result<()> {
    let addr = "127.0.0.1:8082";
    let listener = TcpListener::bind(addr).await?;

    tokio::spawn(async move {
        info!("tcp socket on: {addr}");
        loop {
            match listener.accept().await {
                Ok((stream, _)) => {
                    let web_tx = web_tx.clone();
                    let nvim_tx = nvim_tx.clone();
                    let site = site.clone();
                    tokio::spawn(async move {
                        if let Err(e) = run_neovim_connection(site, stream, web_tx, nvim_tx).await {
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
                    error!("hotwatch error: {err}");
                }
            })
            .expect("failed to watch folder!");

        loop {
            thread::sleep(Duration::from_secs(1));
        }
    });
}

async fn start_file_watcher(output_dir: &AbsPath) -> Result<()> {
    let app = Router::new()
        .fallback(get_service(ServeDir::new(output_dir)))
        .layer(TraceLayer::new_for_http());

    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));

    info!("serving site on: {addr}");
    Server::bind(addr).serve(app.into_make_service()).await?;

    Ok(())
}
