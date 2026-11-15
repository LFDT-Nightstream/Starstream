use core::time::Duration;

use hyper_util::client::legacy::connect::HttpConnector;
use hyper_util::rt::{TokioExecutor, TokioTimer};
use starstream_ledger::client::http::ClientBuilder;

use crate::{Args, exec};

impl Args {
    /// Execute the command against the ledger at `--url`.
    pub async fn exec(self) -> anyhow::Result<()> {
        let mut http = hyper_util::client::legacy::Client::builder(TokioExecutor::new());
        http.pool_timer(TokioTimer::new())
            .pool_idle_timeout(Duration::from_secs(self.pool_idle_timeout));
        let client = ClientBuilder::from_hyper(http, HttpConnector::new(), self.url).build();
        exec(self.command, &client).await
    }
}
