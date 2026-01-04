use anyhow::Context as _;
use bytes::BytesMut;
use wrpc_pack::{pack, unpack};
use crate::rpc;

pub async fn explicit_call(
    addr: String,
    contract_hash: String,
    function: String,
) -> anyhow::Result<i64> {
    let wrpc = wrpc_transport::tcp::Client::from(addr);
    // let params = wit_bindgen_wrpc::bytes::Bytes::new(); // Empty params

    let mut params_bytes = BytesMut::new();
    pack((5,), &mut params_bytes)?;

    let result = rpc::bindings::starstream::wrpc_multiplexer::handler::call(
        &wrpc,
        (),
        &contract_hash.clone(),
        &function.clone(),
        &params_bytes.into(),
    )
    .await
    .with_context(|| format!("failed to call `{contract_hash}.{function}`"))?;

    // note: you have to "know" that the result of the call here is an i64. It will be a runtime error if you get the type wrong
    let result: i64 = unpack(&mut result.into())?;
    Ok(result)
}