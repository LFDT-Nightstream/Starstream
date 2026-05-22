//! Converter from basic-block control flow graphs to Wasm stack machine structured control flow.

// Based LLVM's relooper/stackifier for WebAssembly
// and on the algorithm described in "Solving the structured control flow problem once and for all", Yuri Iozzelli.
// https://medium.com/leaningtech/solving-the-structured-control-flow-problem-once-and-for-all-5123117b1ee2

// Currently only supports reducible CFGs (those without gotos into loops).

use crate::StFunction;

pub struct Stackifier<'a> {
    func: &'a StFunction,
    entry: usize,
}

impl<'a> Stackifier<'a> {
    pub fn new(func: &'a StFunction, entry: usize) -> Self {
        Stackifier { func, entry }
    }
}

impl<'a> wasm_encoder::Encode for Stackifier<'a> {
    fn encode(&self, sink: &mut Vec<u8>) {
        let Stackifier { func, entry } = *self;
        func.cfg.assert_complete();

        func.locals.len().encode(sink);
        for (count, ty) in &func.locals {
            count.encode(sink);
            ty.encode(sink);
        }

        eprintln!("{}", func.cfg.to_mermaid());
        // TODO: actually stackify
    }
}
