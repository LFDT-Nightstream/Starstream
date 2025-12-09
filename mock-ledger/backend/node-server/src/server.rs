use core::pin::pin;

use std::sync::Arc;

use anyhow::Context as _;
use futures::stream::select_all;
use futures::StreamExt as _;
use tokio::task::JoinSet;
use tokio::{select, signal};
use tracing::{debug, error, info, warn};
use wrpc_transport::Server;

use crate::handler::binding::bindings;
use crate::handler::core::Handler;

pub async fn run_server(addr: String, handler: Handler) -> anyhow::Result<()> {
    let srv = Arc::new(Server::default());

    // Bind TCP listener
    let lis = tokio::net::TcpListener::bind(&addr)
        .await
        .with_context(|| format!("failed to bind TCP listener on `{addr}`"))?;

    // Spawn TCP accept loop
    let accept = tokio::spawn({
        let srv = Arc::clone(&srv);
        async move {
            loop {
                if let Err(err) = srv.accept(&lis).await {
                    error!(?err, "failed to accept TCP connection");
                }
            }
        }
    });

    // Start serving wRPC handler
    let invocations = bindings::serve(srv.as_ref(), handler)
        .await
        .context("failed to serve dynamic handler")?;

    // Process invocations
    let mut invocations = select_all(
        invocations
            .into_iter()
            .map(|(contract_hash, name, invocations)| invocations.map(move |res| (contract_hash, name, res))),
    );
    let shutdown = signal::ctrl_c();
    let mut shutdown = pin!(shutdown);
    let mut tasks = JoinSet::new();

    loop {
        select! {
            Some((contract_hash, name, res)) = invocations.next() => {
                match res {
                    Ok(fut) => {
                        debug!(contract_hash, name, "invocation accepted");
                        tasks.spawn(async move {
                            if let Err(err) = fut.await {
                                warn!(?err, "failed to handle invocation");
                            } else {
                                info!(contract_hash, name, "invocation successfully handled");
                            }
                        });
                    }
                    Err(err) => {
                        warn!(?err, contract_hash, name, "failed to accept invocation");
                    }
                }
            }
            Some(res) = tasks.join_next() => {
                if let Err(err) = res {
                    error!(?err, "failed to join task")
                }
            }
            res = &mut shutdown => {
                accept.abort();
                while let Some(res) = tasks.join_next().await {
                    if let Err(err) = res {
                        error!(?err, "failed to join task")
                    }
                }
                return res.context("failed to listen for ^C")
            }
        }
    }
}

