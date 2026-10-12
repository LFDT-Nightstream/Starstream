//! Starstream ledger wRPC protocol definitions.

use std::io::Cursor;

use anyhow::ensure;
use futures::{StreamExt as _, TryStreamExt as _};
use tokio::io::{AsyncRead, AsyncReadExt as _};
use tokio_util::codec::{Decoder, FramedParts, FramedRead};
use tokio_util::io::StreamReader;
use wrpc_transport::FrameDecoder;

#[cfg(any(feature = "client", feature = "server"))]
pub mod bindings;
#[cfg(any(feature = "client", feature = "server"))]
pub mod codec;

pub const LEDGER_BLOCK_INSTANCE: &str = "starstream:ledger/block";
pub const LEDGER_CONTRACT_INSTANCE: &str = "starstream:ledger/contract";
pub const LEDGER_TRANSACTION_INSTANCE: &str = "starstream:ledger/transaction";
pub const LEDGER_GENESIS_INSTANCE: &str = "starstream:ledger/genesis";
pub const LEDGER_UTXO_INSTANCE: &str = "starstream:ledger/utxo";

/// Flatten a framed wRPC stream
pub fn flatten_stream(body: impl AsyncRead + Unpin) -> impl AsyncRead + Unpin {
    let body = FramedRead::new(body, FrameDecoder::default()).map(|frame| {
        let wrpc_transport::Frame { path, data } = frame?;
        ensure!(path.is_empty(), "async values not supported");
        Ok(data)
    });
    StreamReader::new(body.map_err(std::io::Error::other))
}

/// Decode parameters from `body` using `dec`, failing on trailing data.
pub async fn read_params<D>(body: impl AsyncRead + Unpin, dec: D) -> Result<D::Item, D::Error>
where
    D: Decoder,
    D::Error: From<std::io::Error>,
{
    let mut io = FramedRead::new(body, dec);
    let params = io.try_next().await?;
    let params = params.ok_or(std::io::Error::from(std::io::ErrorKind::UnexpectedEof))?;
    let FramedParts { io, read_buf, .. } = io.into_parts();
    ensure_eof(Cursor::new(read_buf).chain(io)).await?;
    Ok(params)
}

/// Fail unless `r` is exhausted
pub async fn ensure_eof(mut r: impl AsyncRead + Unpin) -> std::io::Result<()> {
    let n = r.read(&mut [0]).await?;
    if n == 0 {
        return Ok(());
    }
    Err(std::io::Error::new(
        std::io::ErrorKind::InvalidData,
        "unexpected trailing parameters",
    ))
}
