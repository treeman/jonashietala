use axum::{routing::get_service, Router};
use eyre::Result;
use futures_util::StreamExt;
use hotwatch::Hotwatch;
use std::{net::SocketAddr, thread, time::Duration};
use tokio::net::TcpListener;
use tokio_websockets::ServerBuilder;
use tower_http::{services::ServeDir, trace::TraceLayer};
use tracing::{error, info};

use crate::paths::AbsPath;
use crate::site::{Site, SiteOptions};

pub async fn watch(output_dir: &AbsPath, current_dir: &AbsPath) -> Result<()> {
    let site = Site::load_content(SiteOptions {
        output_dir: output_dir.clone(),
        input_dir: current_dir.clone(),
        clear_output_dir: true,
        include_drafts: true,
        generate_feed: false,
    })?;

    site.render_all()?;

    start_ws().await;
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

async fn run_ws() -> Result<()> {
    let addr = "127.0.0.1:8081";
    let listener = TcpListener::bind(addr).await?;
    info!("events socked on {addr}");

    loop {
        let (conn, _) = listener.accept().await?;
        let mut server = ServerBuilder::new().accept(conn).await?;

        while let Some(Ok(msg)) = server.next().await {
            info!("Received: {msg:?}");
        }
    }
}

async fn start_ws() {
    tokio::spawn(async move {
        run_ws()
            .await
            .unwrap_or_else(|e| panic!("Websocket crashed {e:?}"));
    });
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
