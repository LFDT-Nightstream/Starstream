use core::ffi::c_char;

use std::ffi::CString;

use anyhow::{Context as _, anyhow};
use bytes::Bytes;
use http_body_util::{BodyExt as _, Full};
use starstream_ledger::client::http::{ClientBuilder, Transport, TransportService};

use crate::{Args, exec};

#[link(wasm_import_module = "starstream")]
unsafe extern "C" {
    /// Sends a request with `headers_len` headers, each a name followed by a
    /// value in `headers`, and returns the status of the response, storing
    /// the length of its body in `len`, or -1 if it failed, storing the length
    /// of the message. `fetch_body` reads either.
    fn fetch(
        method: *const c_char,
        uri: *const c_char,
        headers: *const *const c_char,
        headers_len: usize,
        body: *const u8,
        body_len: usize,
        len: *mut usize,
    ) -> i32;

    /// Copies the body or failure message of the last `fetch` to `dst`.
    fn fetch_body(dst: *mut u8);
}

/// [Transport] of the `starstream` `fetch` import.
#[derive(Clone)]
struct FetchTransport;

impl Transport for FetchTransport {
    async fn request(
        &self,
        req: http::Request<Full<Bytes>>,
    ) -> anyhow::Result<(http::response::Parts, Bytes)> {
        let (parts, body) = req.into_parts();
        let Ok(body) = body.collect().await;
        let body = body.to_bytes();
        let method = CString::new(parts.method.as_str()).context("method is not valid")?;
        let uri = CString::new(parts.uri.to_string()).context("URI is not valid")?;
        let mut headers = Vec::with_capacity(parts.headers.len().saturating_mul(2));
        for (name, value) in &parts.headers {
            let name = CString::new(name.as_str()).context("header name is not valid")?;
            let value = CString::new(value.as_bytes()).context("header value is not valid")?;
            headers.push(name);
            headers.push(value);
        }
        let headers: Vec<_> = headers.iter().map(|s| s.as_ptr()).collect();
        let mut len = 0;
        let status = unsafe {
            fetch(
                method.as_ptr(),
                uri.as_ptr(),
                headers.as_ptr(),
                parts.headers.len(),
                body.as_ptr(),
                body.len(),
                &mut len,
            )
        };
        let mut buf = vec![0; len];
        unsafe { fetch_body(buf.as_mut_ptr()) };
        let status = u16::try_from(status).map_err(|_| {
            anyhow!(
                "failed to send request to `{}`: {}",
                parts.uri,
                String::from_utf8_lossy(&buf)
            )
        })?;
        let res = http::Response::builder()
            .status(status)
            .body(())
            .context("failed to build response")?;
        let (parts, ()) = res.into_parts();
        Ok((parts, buf.into()))
    }
}

impl Args {
    /// Execute the command against the ledger at `--url`.
    pub async fn exec(self) -> anyhow::Result<()> {
        let mut config = wasmtime::Config::default();
        starstream_ledger::configure_wasmtime(&mut config);
        starstream_runtime_web::configure_wasmtime(&mut config)?;
        let engine = wasmtime::Engine::new(&config)?;
        let client = ClientBuilder::new(
            TransportService(FetchTransport),
            FetchTransport,
            self.url,
            engine,
        )
        .build();
        exec(self.command, &client).await
    }
}
