use zk_engine::{
    error::ZKWASMError,
    nova::{
        provider::{Bn256EngineIPA, ipa_pc},
        spartan,
        traits::Dual,
    },
    utils::logging::init_logger,
    wasm_ctx::{WASMArgs, WASMArgsBuilder, WASMCtx, ZKWASMCtx},
    wasm_snark::{StepSize, WasmSNARK},
};

use crate::Transaction;

type E = Bn256EngineIPA;
type EE1 = ipa_pc::EvaluationEngine<E>;
type EE2 = ipa_pc::EvaluationEngine<Dual<E>>;
type S1 = spartan::batched::BatchedRelaxedR1CSSNARK<E, EE1>;
type S2 = spartan::batched::BatchedRelaxedR1CSSNARK<Dual<E>, EE2>;
type Snark = WasmSNARK<E, S1, S2>;

/// A context that supplies a linker fulfilling Starstream imports.
struct StarstreamWasmCtx {
    args: WASMArgs,
}

impl ZKWASMCtx for StarstreamWasmCtx {
    type T = ();

    fn create_store(engine: &wasmi::Engine) -> wasmi::Store<Self::T> {
        wasmi::Store::new(engine, ())
    }

    fn create_linker(engine: &wasmi::Engine) -> Result<wasmi::Linker<Self::T>, ZKWASMError> {
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
        let public_params = Snark::setup(step_size);

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
