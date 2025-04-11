use wasmi::{Engine, Linker, Module, Store};
use zk_engine::{
    error::ZKWASMError,
    nova::{
        provider::{Bn256EngineIPA, ipa_pc},
        spartan,
        traits::Dual,
    },
    utils::logging::init_logger,
    wasm_ctx::{WASMArgs, WASMArgsBuilder, ZKWASMCtx},
    wasm_snark::{StepSize, WasmSNARK},
};

use crate::{Transaction, TransactionInner, TxProgram, code::CodeHash, fake_import};

type E = Bn256EngineIPA;
type EE1 = ipa_pc::EvaluationEngine<E>;
type EE2 = ipa_pc::EvaluationEngine<Dual<E>>;
type S1 = spartan::batched::BatchedRelaxedR1CSSNARK<E, EE1>;
type S2 = spartan::batched::BatchedRelaxedR1CSSNARK<Dual<E>, EE2>;
type Snark = WasmSNARK<E, S1, S2>;

fn starstream_env_zk<T>(linker: &mut Linker<T>, module: &str, this_code: CodeHash) {
    linker
        .func_wrap(module, "abort", || -> () {
            panic!("contract called abort()");
        })
        .unwrap();
}

#[derive(Clone)]
struct StoreData<'a> {
    tx: &'a TransactionInner,
    program: &'a TxProgram,
    witness: usize,
}

/// A context that supplies a linker fulfilling Starstream imports.
struct StarstreamWasmCtx<'a> {
    args: WASMArgs,
    data: StoreData<'a>,
}

impl<'a> ZKWASMCtx for StarstreamWasmCtx<'a> {
    type T = StoreData<'a>;

    fn create_store(&self, engine: &Engine) -> Store<Self::T> {
        Store::new(engine, self.data.clone())
    }

    fn create_linker(
        &self,
        engine: &Engine,
        module: &Module,
    ) -> Result<Linker<Self::T>, ZKWASMError> {
        let mut linker = Linker::<Self::T>::new(engine);

        // Set real imports
        starstream_env_zk(&mut linker, "env", self.data.program.code);

        // Fake all remaining imports
        for import in module.imports() {
            fake_import(&mut linker, &import, "not yet implemented");
        }

        Ok(linker)
    }

    fn args(&self) -> &WASMArgs {
        &self.args
    }
}

impl Transaction {
    pub fn do_nebula_stuff(&self) {
        eprintln!();
        init_logger();

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
            let wasm_ctx = StarstreamWasmCtx {
                args: wasm_args,
                data: StoreData {
                    tx: inner,
                    program,
                    witness: 0,
                },
            };
            let (snark, instance) = Snark::prove(&public_params, &wasm_ctx, step_size).unwrap();
            snark.verify(&public_params, &instance).unwrap();
        }
    }
}
