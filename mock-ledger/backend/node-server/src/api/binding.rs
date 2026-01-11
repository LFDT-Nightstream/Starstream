pub mod bindings {
    wit_bindgen_wrpc::generate!({
        with: {
            "starstream:node-rpc/handler": generate,
            "starstream:registry/handler": generate,
            "starstream:wrpc-multiplexer/handler": generate,
        }
    });
}
