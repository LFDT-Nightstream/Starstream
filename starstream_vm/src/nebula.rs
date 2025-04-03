use zk_engine::{
    error::ZKWASMError,
    nova::{
        provider::{Bn256EngineIPA, ipa_pc},
        spartan,
        traits::Dual,
    },
    utils::logging::init_logger,
    wasm_ctx::{WASMArgsBuilder, WASMCtx},
    wasm_snark::{StepSize, WasmSNARK},
};

type E = Bn256EngineIPA;
type EE1 = ipa_pc::EvaluationEngine<E>;
type EE2 = ipa_pc::EvaluationEngine<Dual<E>>;
type S1 = spartan::batched::BatchedRelaxedR1CSSNARK<E, EE1>;
type S2 = spartan::batched::BatchedRelaxedR1CSSNARK<Dual<E>, EE2>;
type Snark = WasmSNARK<E, S1, S2>;

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
        let public_params = Snark::setup(step_size);

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
