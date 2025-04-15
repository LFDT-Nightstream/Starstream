use wasmi::{Caller, Engine, ExternType, Linker, Module, Store, core::TrapCode};
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

use crate::{
    ProgramIdx, Transaction, TransactionInner, TxProgram, WasmiError, code::CodeHash, fake_import,
    starstream_eprint,
};

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
    linker
        .func_wrap(
            module,
            "eprint",
            |caller: Caller<T>, ptr: u32, len: u32| -> () {
                starstream_eprint(caller, ptr, len);
            },
        )
        .unwrap();
}

#[derive(Clone)]
struct StoreData<'a> {
    tx: &'a TransactionInner,
    program_idx: ProgramIdx,
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

        // Remaining imports slurp witnesses
        for import in module.imports() {
            //fake_import(&mut linker, &import, "not yet implemented");
            if let ExternType::Func(func_ty) = import.ty() {
                let desc = format!("{:?}.{:?}", import.module(), import.name());
                let _ = linker.func_new(
                    import.module(),
                    import.name(),
                    func_ty.clone(),
                    move |mut caller, inputs, outputs| -> Result<(), WasmiError> {
                        let data = caller.data_mut();
                        // This call corresponds to a witness FROM us, so find that.
                        eprintln!("{}{:?}", desc, inputs);
                        while data.witness < data.tx.witnesses.len() {
                            if data.tx.witnesses[data.witness].from_program == data.program_idx {
                                eprintln!("  -> {:?}", &data.tx.witnesses[data.witness]);
                                break;
                            }
                            data.witness += 1;
                        }
                        // Then find the next witness TO us to correspond.
                        // TODO: explicitly store this rather than guessing.
                        while data.witness < data.tx.witnesses.len() {
                            eprintln!("  ?? {:?}", &data.tx.witnesses[data.witness]);
                            if data.tx.witnesses[data.witness].to_program == data.program_idx {
                                eprintln!("  <- {:?}", &data.tx.witnesses[data.witness]);

                                outputs.clone_from_slice(&data.tx.witnesses[data.witness].values);
                                // TODO: copy memory registered in witness

                                data.witness += 1;
                                return Ok(());
                            }
                            data.witness += 1;
                        }
                        // No more witnesses to use, so trace ends now.
                        Err(WasmiError::from(TrapCode::OutOfFuel))
                    },
                );
            }
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

        //let step_size = StepSize::new(1000).set_memory_step_size(50_000);
        //let public_params = Snark::setup(step_size);

        for (i, program) in inner.programs.iter().enumerate() {
            eprintln!("\n{:?} {program:?}", ProgramIdx(i));
            let wasm_args = WASMArgsBuilder::default()
                .bytecode(self.code_cache.get(program.code).wasm().to_vec())
                .invoke(&program.entry_point)
                // TODO: func_args with first witness
                .build();
            let wasm_ctx = StarstreamWasmCtx {
                args: wasm_args,
                data: StoreData {
                    tx: inner,
                    program_idx: ProgramIdx(i),
                    program,
                    witness: 0,
                },
            };
            let trace = wasm_ctx.execution_trace();
            eprintln!("{:?}", trace.map(|_| ()));
            //let (snark, instance) = Snark::prove(&public_params, &wasm_ctx, step_size).unwrap();
            //snark.verify(&public_params, &instance).unwrap();
        }
    }
}
