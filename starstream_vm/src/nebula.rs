use zk_engine::{
    error::ZKWASMError,
    nova::{
        provider::{Bn256EngineKZG, GrumpkinEngine, hyperkzg, ipa_pc},
        spartan::ppsnark::RelaxedR1CSSNARK,
    },
    wasm_ctx::{WASMArgs, WASMArgsBuilder, ZKWASMCtx},
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

/// A context that supplies a linker fulfilling Starstream imports.
struct StarstreamWasmCtx {
    args: WASMArgs,
}

impl ZKWASMCtx for StarstreamWasmCtx {
    type T = ();

    fn store(engine: &wasmi::Engine) -> wasmi::Store<Self::T> {
        wasmi::Store::new(engine, ())
    }

    fn linker(engine: &wasmi::Engine) -> Result<wasmi::Linker<Self::T>, ZKWASMError> {
        Ok(<wasmi::Linker<()>>::new(engine))
    }

    fn args(&self) -> &WASMArgs {
        &self.args
    }
}

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
        dbg!(&step_size);
        let public_params = Snark::setup(step_size).unwrap();
        eprintln!("snark setup done");

        for program in &inner.programs {
            eprintln!("{program:?}");
            let wasm_args = WASMArgsBuilder::default()
                .bytecode(self.code_cache.get(program.code).wasm().to_vec())
                .invoke(&program.entry_point)
                // TODO: func_args with first witness
                .build();
            let wasm_ctx = StarstreamWasmCtx { args: wasm_args };
            let (snark, instance) = Snark::prove(&public_params, &wasm_ctx, step_size).unwrap();
            snark.verify(&public_params, &instance).unwrap();
        }
    }
}
