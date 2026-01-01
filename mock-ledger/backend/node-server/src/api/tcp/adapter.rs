use core::net::SocketAddr;

use crate::api::{binding::bindings, handler::Handler};

impl bindings::exports::starstream::wrpc_multiplexer::handler::Handler<SocketAddr> for Handler {
    async fn call(
        &self,
        _ctx: SocketAddr,
        contract_hash: String,
        function: String,
        params: wit_bindgen_wrpc::bytes::Bytes,
    ) -> anyhow::Result<wit_bindgen_wrpc::bytes::Bytes> {
        self.call(contract_hash, function, params)
            .await
            .map(|v| v.into())
    }
}
// Add back once we have other RPC methods
// impl bindings::exports::starstream::node_rpc::handler::Handler<SocketAddr> for Handler {
// }
