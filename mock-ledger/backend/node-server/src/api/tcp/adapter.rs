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

impl bindings::exports::starstream::node_rpc::registry::Handler<SocketAddr> for Handler {
    async fn get_wit(
        &self,
        _ctx: SocketAddr,
        hash: String,
        resolve: bool,
    ) -> anyhow::Result<bindings::exports::starstream::node_rpc::registry::ComponentInterface> {
        // TODO: a proper registry
        Ok(
            bindings::exports::starstream::node_rpc::registry::ComponentInterface {
                wit: String::from(
                    "
                package root:component;
                world root {
                export get-value: func(value: s64) -> s64;
                }
            ",
                ),
                entrypoint: String::from("root"),
            },
        )
    }
}
