use zk_engine::{
    nova::{
        provider::{Bn256EngineKZG, GrumpkinEngine, hyperkzg, ipa_pc},
        spartan::ppsnark::RelaxedR1CSSNARK,
    },
    wasm_ctx::{WASMArgsBuilder, WASMCtx},
    wasm_snark::{StepSize, WasmSNARK},
};

type E1 = Bn256EngineKZG;
type E2 = GrumpkinEngine;
type EE1 = hyperkzg::EvaluationEngine<E1>;
type EE2 = ipa_pc::EvaluationEngine<E2>;
type S1 = RelaxedR1CSSNARK<E1, EE1>;
type S2 = RelaxedR1CSSNARK<E2, EE2>;
type Snark = WasmSNARK<E1, E2, S1, S2>;

use crate::Transaction;

impl Transaction {
    pub fn do_nebula_stuff(&self) {
        eprintln!();

        let inner = self.store.data();
        eprintln!(
            "Programs: {}\nWitnesses: {}",
            inner.programs.len(),
            inner.witnesses.len()
        );

        // Start with the very first witness, which should always be Root->0.
        for witness in &inner.witnesses {
            eprintln!("{witness:?}");
        }

        let step_size = StepSize::new(1000).set_memory_step_size(50_000);
        let public_params = Snark::setup(step_size).unwrap();

        for program in &inner.programs {
            eprintln!("{program:?}");
            let wasm_args = WASMArgsBuilder::default()
                .bytecode(self.code_cache.get(program.code).wasm().to_vec())
                .invoke(&program.entry_point)
                // TODO: func_args with first witness
                .build();
            let wasm_ctx = WASMCtx::new(wasm_args);
            let (snark, instance) = Snark::prove(&public_params, &wasm_ctx, step_size).unwrap();
            snark.verify(&public_params, &instance).unwrap();
        }
    }
}
