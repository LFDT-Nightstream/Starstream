use core::error::Error;
use core::future::poll_fn;
use core::pin::pin;
use core::task::{Poll, ready};
use core::time::Duration;

use std::sync::Arc;

use hyper::service::service_fn;
use hyper_util::rt::{TokioIo, TokioTimer};
use hyper_util::server::graceful::GracefulShutdown;
use tokio::net::TcpListener;
use tokio::sync::Notify;
use tokio::task::JoinSet;
use tokio::time::sleep;
use tokio_io_timeout::TimeoutStream;
use tracing::{Instrument as _, debug, error, info, instrument, warn};

use crate::server::Ledger;

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
    /// Return the future serving the ledger HTTP API on `lis`.
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
    pub fn serve_http(
        self: Arc<Self>,
        lis: TcpListener,
        io_timeout: Duration,
    ) -> (impl Future<Output = ()> + use<>, Arc<Notify>) {
        let ledger = Arc::clone(&self);
        let svc = service_fn(move |req: http::Request<hyper::body::Incoming>| {
            let ledger = Arc::clone(&ledger);
            async move { ledger.handle_http(req, || sleep(io_timeout)).await }
        });
        let shutdown: Arc<Notify> = Arc::default();
        let mut srv = hyper::server::conn::http1::Builder::new();
        srv.timer(TokioTimer::new()).header_read_timeout(io_timeout);
        let max_connections = usize::try_from(self.max_requests).unwrap_or(usize::MAX);
        (
            {
                let shutdown = Arc::clone(&shutdown).notified_owned();
                async move {
                    let graceful = GracefulShutdown::default();
                    let mut tasks = JoinSet::default();
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
                                    let res = ready!(lis.poll_accept(cx));
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
