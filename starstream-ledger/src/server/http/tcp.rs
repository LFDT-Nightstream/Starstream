use core::future::poll_fn;
use core::pin::pin;
use core::task::{Poll, ready};
use core::time::Duration;

use std::error::Error as _;
use std::sync::Arc;

use futures::TryFutureExt as _;
use hyper::service::service_fn;
use hyper_util::rt::{TokioIo, TokioTimer};
use hyper_util::server::graceful::GracefulShutdown;
use tokio::net::TcpListener;
use tokio::sync::{Notify, TryAcquireError};
use tokio::task::JoinSet;
use tokio::time::sleep;
use tokio_io_timeout::TimeoutStream;
use tracing::{Instrument as _, debug, error, info, instrument, warn};

use crate::server::Ledger;
use crate::server::http::{
    allow_cross_origin, build_options_or_method_not_allowed, build_text_response,
};

fn is_connection_error(err: &std::io::Error) -> bool {
    matches!(
        err.kind(),
        std::io::ErrorKind::ConnectionRefused
            | std::io::ErrorKind::ConnectionAborted
            | std::io::ErrorKind::ConnectionReset
    )
}

fn is_timeout(err: &hyper::Error) -> bool {
    err.is_timeout()
        || err
            .source()
            .and_then(|err| err.downcast_ref::<std::io::Error>())
            .is_some_and(|err| err.kind() == std::io::ErrorKind::TimedOut)
}

impl Ledger {
    /// Return the future serving the ledger HTTP API on `listener`.
    ///
    /// At most `max_requests` connections are served at once, and a connection
    /// is closed if it does not send a complete request head within
    /// `io_timeout` of opening or of finishing its previous response, if it
    /// sends no chunk of a request body for `io_timeout`, or if a response
    /// write to it makes no progress for `io_timeout`.
    ///
    /// Calling [`Notify::notify_one`] on the returned handle shuts the server
    /// down; [`Notify::notify_waiters`] is lost if it races the first poll of
    /// the future.
    #[instrument(skip_all)]
    pub fn handle_http(
        self: Arc<Self>,
        listener: TcpListener,
        io_timeout: Duration,
    ) -> (impl Future<Output = ()> + use<>, Arc<Notify>) {
        let ledger = self.clone();
        let svc = service_fn(move |req: http::Request<hyper::body::Incoming>| {
            let ledger = ledger.clone();
            async move {
                let _permit = match ledger.permits.try_acquire() {
                    Ok(permit) => permit,
                    Err(TryAcquireError::NoPermits) => {
                        return build_text_response(
                            http::StatusCode::SERVICE_UNAVAILABLE,
                            "maximum concurrent request count reached",
                        );
                    }
                    Err(TryAcquireError::Closed) => {
                        return build_text_response(
                            http::StatusCode::INTERNAL_SERVER_ERROR,
                            "semaphore closed",
                        );
                    }
                };
                let (
                    http::request::Parts {
                        method,
                        uri,
                        headers,
                        ..
                    },
                    body,
                ) = req.into_parts();
                let Some(pq) = uri.path_and_query() else {
                    return build_text_response(
                        http::StatusCode::BAD_REQUEST,
                        "request target unsupported",
                    );
                };
                let path = pq.path();
                let path = path
                    .strip_suffix('/')
                    .filter(|path| !path.is_empty())
                    .unwrap_or(path);
                let mut path = path.split('/');
                let Some("") = path.next() else {
                    return build_text_response(
                        http::StatusCode::BAD_REQUEST,
                        "request target unsupported",
                    );
                };
                match (
                    method.as_str(),
                    path.next(),
                    path.next(),
                    path.next(),
                    path.next(),
                ) {
                    ("GET", Some("contracts"), Some(digest), None, ..) => match ledger
                        .handle_contract_get::<false>(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    ("HEAD", Some("contracts"), Some(digest), None, ..) => match ledger
                        .handle_contract_get::<true>(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    (_, Some("contracts"), Some(..), None, ..) => {
                        build_options_or_method_not_allowed(
                            "GET, HEAD, OPTIONS",
                            &method,
                            pq.path(),
                        )
                    }

                    ("GET", Some("transactions"), Some(digest), None, ..) => match ledger
                        .handle_transaction_get::<false>(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    ("HEAD", Some("transactions"), Some(digest), None, ..) => match ledger
                        .handle_transaction_get::<true>(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    (_, Some("transactions"), Some(..), None, ..) => {
                        build_options_or_method_not_allowed(
                            "GET, HEAD, OPTIONS",
                            &method,
                            pq.path(),
                        )
                    }

                    ("POST", Some("rpc"), None, ..) => {
                        match ledger.handle_rpc_post(headers, body, io_timeout).await {
                            Ok(res) => Ok(res),
                            Err(err) => {
                                build_text_response(err.http_status_code(), err.to_string())
                            }
                        }
                    }
                    ("GET", Some("rpc"), None, ..) => match ledger
                        .handle_rpc_get::<false>(headers, pq.query())
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    ("HEAD", Some("rpc"), None, ..) => match ledger
                        .handle_rpc_get::<true>(headers, pq.query())
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    (_, Some("rpc"), None, ..) => build_options_or_method_not_allowed(
                        "GET, HEAD, OPTIONS, POST",
                        &method,
                        pq.path(),
                    ),

                    _ => build_text_response(
                        http::StatusCode::NOT_FOUND,
                        format!("path `{}` not found", pq.path()),
                    ),
                }
            }
            .map_ok(allow_cross_origin)
        });
        let shutdown = Arc::new(Notify::new());
        let mut srv = hyper::server::conn::http1::Builder::new();
        srv.timer(TokioTimer::new()).header_read_timeout(io_timeout);
        let max_connections = self.max_requests as usize;
        (
            {
                let shutdown = Arc::clone(&shutdown).notified_owned();
                async move {
                    let graceful = GracefulShutdown::default();
                    let mut tasks = JoinSet::new();
                    let mut shutdown = pin!(shutdown);
                    loop {
                        match poll_fn(|cx| {
                            while let Poll::Ready(Some(res)) = tasks.poll_join_next(cx) {
                                if let Err(err) = res {
                                    error!(?err, "HTTP task panicked");
                                }
                            }
                            match shutdown.as_mut().poll(cx) {
                                Poll::Ready(()) => Poll::Ready(None),
                                // Postpone accepting until a connection task
                                // completes, which wakes this future via
                                // `poll_join_next` above.
                                Poll::Pending if tasks.len() >= max_connections => Poll::Pending,
                                Poll::Pending => {
                                    let res = ready!(listener.poll_accept(cx));
                                    Poll::Ready(Some(res))
                                }
                            }
                        })
                        .await
                        {
                            Some(Ok((stream, addr))) => {
                                info!(?addr, "accepted TCP connection");
                                let mut stream = TimeoutStream::new(stream);
                                stream.set_write_timeout(Some(io_timeout));
                                let conn = srv
                                    .serve_connection(TokioIo::new(Box::pin(stream)), svc.clone());
                                let conn = graceful.watch(conn);
                                tasks.spawn(
                                    async move {
                                        if let Err(err) = conn.await {
                                            if is_timeout(&err) {
                                                debug!(?err, "closed stalled HTTP connection");
                                            } else {
                                                warn!(?err, "failed to serve HTTP connection");
                                            }
                                        }
                                    }
                                    .in_current_span(),
                                );
                            }
                            Some(Err(err)) if is_connection_error(&err) => {
                                debug!(?err, "failed to accept TCP connection")
                            }
                            Some(Err(err)) => {
                                error!(?err, "failed to accept TCP connection");
                                sleep(Duration::from_secs(1)).await;
                            }
                            None => break,
                        }
                    }
                    graceful.shutdown().await;
                    while let Some(res) = tasks.join_next().await {
                        if let Err(err) = res {
                            error!(?err, "HTTP task panicked");
                        }
                    }
                }
                .in_current_span()
            },
            shutdown,
        )
    }
}
