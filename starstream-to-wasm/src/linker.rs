use std::rc::Rc;

use starstream_compiler::WasmLinkage;
use wac_graph::{CompositionGraph, EncodeOptions, types::Package};
use wit_component::{ComponentEncoder, LibraryInfo};

use crate::CompileResult;

impl CompileResult {
    pub fn to_component(&self) -> miette::Result<Rc<[u8]>> {
        let mut cached = self.component.borrow_mut();
        if let Some(cached) = &*cached {
            return Ok(cached.clone());
        }

        let comp: Rc<[u8]> = self.to_component_inner()?.into();
        *cached = Some(comp.clone());
        Ok(comp)
    }

    fn to_component_inner(&self) -> miette::Result<Vec<u8>> {
        let Some(wasm) = &self.wasm else {
            panic!("CompileResult::to_component has no wasm")
        };
        let mut encoder = ComponentEncoder::default();
        encoder.validate(true);
        encoder
            .module(wasm)
            .expect("ComponentEncoder::module failed");

        // Imported core modules are combined into the main component using `ComponentEncoder`.
        for (name, bytes, linkage) in &self.libraries {
            if matches!(linkage, WasmLinkage::Core) {
                encoder
                    .library(
                        name,
                        bytes,
                        LibraryInfo {
                            arguments: Default::default(),
                        },
                    )
                    .expect("ComponentEncoder::library failed");
            }
        }

        let mut component = encoder.encode().expect("ComponentEncoder::encode failed");

        // Imported components are composed together using `wac_graph`.
        if self
            .libraries
            .iter()
            .any(|(_, _, linkage)| matches!(linkage, WasmLinkage::Component))
        {
            let mut graph = CompositionGraph::new();

            let root = Package::from_bytes("root", None, component, graph.types_mut()).unwrap();
            let root = graph.register_package(root).unwrap();
            let root = graph.instantiate(root);

            for (name, bytes, linkage) in &self.libraries {
                if matches!(linkage, WasmLinkage::Component) {
                    let lib =
                        Package::from_bytes(name, None, bytes.to_vec(), graph.types_mut()).unwrap();
                    let lib = graph.register_package(lib).unwrap();
                    let lib = graph.instantiate(lib);
                    graph.set_instantiation_argument(root, name, lib).unwrap();
                }
            }

            component = graph.encode(EncodeOptions::default()).unwrap();
        }

        Ok(component)
    }
}
