use wac_graph::{CompositionGraph, EncodeOptions, types::Package};
use wit_component::ComponentEncoder;

use crate::CompileResult;

impl CompileResult {
    pub fn to_component(&self) -> miette::Result<Vec<u8>> {
        let Some(wasm) = &self.wasm else {
            panic!("CompileResult::to_component has no wasm")
        };
        let mut encoder = ComponentEncoder::default();
        encoder = encoder.validate(true);
        encoder = encoder
            .module(&wasm)
            .expect("ComponentEncoder::module failed");
        let mut component = encoder.encode().expect("ComponentEncoder::encode failed");

        if !self.libraries.is_empty() {
            let mut graph = CompositionGraph::new();

            let root = Package::from_bytes("root", None, component, graph.types_mut()).unwrap();
            let root = graph.register_package(root).unwrap();
            let root = graph.instantiate(root);

            for (name, bytes) in &self.libraries {
                let lib =
                    Package::from_bytes(name, None, bytes.to_vec(), graph.types_mut()).unwrap();
                let lib = graph.register_package(lib).unwrap();
                let lib = graph.instantiate(lib);
                graph.set_instantiation_argument(root, name, lib).unwrap();
            }

            component = graph.encode(EncodeOptions::default()).unwrap();
        }

        Ok(component)
    }
}
