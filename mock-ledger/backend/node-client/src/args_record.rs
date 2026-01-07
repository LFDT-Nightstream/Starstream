use anyhow::Context as _;
use bytes::BytesMut;
use wrpc_pack::{pack, unpack};
use crate::rpc;

pub mod args_record_bindings {
    wit_bindgen_wrpc::generate!({
        path: "../node-server/genesis/wit/args-record/wit"
    });
}


pub async fn explicit_call(
    addr: String,
    contract_hash: String,
    function: String,
) -> anyhow::Result<i64> {
    let wrpc = wrpc_transport::tcp::Client::from(addr);

    let mut params_bytes = BytesMut::new();
    let token = args_record_bindings::Token {
        amount: 5,
        price: 10,
    };
    pack((token,), &mut params_bytes)?;

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
    // you can do better if you have access to the WIT
    let result: i64 = unpack(&mut result.into())?;
    Ok(result)
}