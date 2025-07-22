use log::{debug, info, trace};
use wasmi::{Caller, Engine, ExternType, Linker, Module, Store, Value};
use zk_engine::{
    error::ZKWASMError,
    nova::{
        provider::{Bn256EngineIPA, ipa_pc},
        spartan::batched::BatchedRelaxedR1CSSNARK,
        traits::Dual,
    },
    utils::logging::init_logger,
    wasm_ctx::{WASMArgs, WASMArgsBuilder, ZKWASMCtx},
    wasm_snark::{StepSize, WasmSNARK, ZKWASMInstance},
};

mod memory;

use crate::{
    ProgramIdx, Transaction, TransactionInner, TransactionProof, TxProgram, WasmiError,
    code::CodeHash, memory, starstream_eprint,
};

type Eng1 = Bn256EngineIPA;
type Eng2 = Dual<Eng1>;
type EvalEng1 = ipa_pc::EvaluationEngine<Eng1>;
type EvalEng2 = ipa_pc::EvaluationEngine<Eng2>;
type Snark1 = BatchedRelaxedR1CSSNARK<Eng1, EvalEng1>;
type Snark2 = BatchedRelaxedR1CSSNARK<Eng2, EvalEng2>;
type Snark = WasmSNARK<Eng1, Snark1, Snark2>;
type PublicParams = zk_engine::wasm_snark::WASMPublicParams<Eng1, Snark1, Snark2>;

struct OurPublicParams {
    step_size: StepSize,
    params: PublicParams,
}

thread_local! {
    static PUBLIC_PARAMS: OurPublicParams = {
        info!("Begin Snark::setup, this may take a while...");
        let step_size = StepSize::new(1000).set_memory_step_size(50_000);
        let params = Snark::setup(step_size);
        info!("End Snark::setup");
        OurPublicParams { step_size, params }
    };
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct ProgramProof {
    snark: Snark,
    instance: ZKWASMInstance<Eng1>,
}

impl ProgramProof {
    pub fn verify(&self) {
        PUBLIC_PARAMS.with(|pp| {
            self.snark.verify(&pp.params, &self.instance).unwrap();
        });
    }
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct TableProof {
    // TODO
}

impl TableProof {
    pub fn verify(&self) {}
}

#[allow(clippy::unused_unit)] // False positive. `clippy --fix` breaks the code.
fn starstream_env_zk<T>(linker: &mut Linker<T>, module: &str, this_code: CodeHash) {
    linker
        .func_wrap(module, "abort", || -> () {
            panic!("contract called abort()");
        })
        .unwrap();
    linker
        .func_wrap(module, "eprint", |caller: Caller<T>, ptr: u32, len: u32| {
            starstream_eprint(caller, ptr, len);
        })
        .unwrap();
    // TODO: starstream_coordination_code
    linker
        .func_wrap(
            module,
            "starstream_this_code",
            move |mut caller: Caller<T>, return_addr: u32| {
                trace!("starstream_this_code({return_addr:#x})");
                let (memory, _) = memory(&mut caller);
                let hash = this_code.raw();
                memory[return_addr as usize..return_addr as usize + hash.len()]
                    .copy_from_slice(&hash);
            },
        )
        .unwrap();
    // TODO: effect handler stuff
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
                        debug!("{}{:?}", desc, inputs);
                        while data.witness < data.tx.witnesses.len() {
                            if data.tx.witnesses[data.witness].from_program == data.program_idx {
                                debug!("  -> {:?}", &data.tx.witnesses[data.witness]);
                                break;
                            }
                            data.witness += 1;
                        }
                        // Then find the next witness TO us to correspond.
                        // TODO: explicitly store this rather than guessing.
                        while data.witness < data.tx.witnesses.len() {
                            if data.tx.witnesses[data.witness].to_program == data.program_idx {
                                debug!("  <- {:?}", &data.tx.witnesses[data.witness]);

                                outputs.clone_from_slice(&data.tx.witnesses[data.witness].values);
                                // TODO: copy memory registered in witness

                                data.witness += 1;
                                return Ok(());
                            }
                            data.witness += 1;
                        }
                        // No more witnesses to use, so trace ends now.
                        Err(WasmiError::i32_exit(0))
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
    pub(crate) fn do_nebula_stuff(&self) -> TransactionProof {
        // Throw away `tracing` logs for now. Maybe if we determine they have
        // anything useful, we can use them later.
        init_logger();

        let inner = self.store.data();
        info!(
            "Preparing to prove: {} programs, {} witnesses",
            inner.programs.len(),
            inner.witnesses.len()
        );

        // Start with the very first witness, which should always be Root->0.
        for witness in &inner.witnesses {
            debug!("{witness:?}");
        }

        PUBLIC_PARAMS.with(|pp| {
            // Prove the traces of each program.
            let mut program_proofs = Vec::new();
            for (i, program) in inner.programs.iter().enumerate() {
                let program_idx = ProgramIdx(i);
                debug!("{:?} {program:?}", program_idx);

                // Scan for first witness TO our program to be the func_args.
                let mut witness = 0;
                let mut func_args = Vec::new();
                while witness < inner.witnesses.len() {
                    if inner.witnesses[witness].to_program == program_idx {
                        debug!("  -> {:?}", &inner.witnesses[witness]);
                        func_args = inner.witnesses[witness]
                            .values
                            .iter()
                            .map(|v| match v {
                                Value::F32(x) => x.to_string(),
                                Value::F64(x) => x.to_string(),
                                Value::I32(x) => x.to_string(),
                                Value::I64(x) => x.to_string(),
                                _ => unimplemented!(),
                            })
                            .collect();
                        break;
                    }
                    witness += 1;
                }

                let wasm_args = WASMArgsBuilder::default()
                    .bytecode(self.code_cache.get(program.code).wasm().to_vec())
                    .invoke(&program.entry_point)
                    .func_args(func_args)
                    .build();
                let wasm_ctx = StarstreamWasmCtx {
                    args: wasm_args,
                    data: StoreData {
                        tx: inner,
                        program_idx,
                        program,
                        witness,
                    },
                };
                let (snark, instance) = Snark::prove(&pp.params, &wasm_ctx, pp.step_size).unwrap();
                debug!("Finished program proof {i}");
                program_proofs.push(ProgramProof { snark, instance });
            }

            // HUGE TODO: prove that the program traces and the continuation table actually correspond.

            // Memory-consistency-check the continuation table.
            // TODO: this whole thing is of highly questionable soundness.
            let continuations = self.map_continuations();

            // Initial set (IS) is all zeroes for now. In real life, it would be made to include the input UTXO states.
            let mut fs = Vec::new();
            for i in 0..1024 {
                fs.push((i, 0, 0));
            }
            //let is = fs.clone();
            let mut rs = Vec::new();
            let mut ws = Vec::new();
            let mut global_ts = 0;
            for each in continuations.iter() {
                // A continuation is like a read of the old state + a write of the new state.
                for (addr, &val) in each.state_after.as_u64s().iter().enumerate() {
                    // Using this as the "address" probably isn't great; what we
                    // really want is to use the UTXO's identity and act as if
                    // its state is stored at that address somehow.
                    // But this table also includes every continuation step, not
                    // just UTXO-level stuff.
                    memory::read_op(addr, &mut global_ts, &mut fs, &mut rs, &mut ws);
                    memory::write_op(addr, val, &mut global_ts, &mut fs, &mut rs, &mut ws);
                }

                // TODO: Include `input`, `entry_point`, and the relevant entry from `program_proofs`.
            }

            // TODO: Turn IS, RS, WS, and FS into a real ZK circuit proving their correspondence.
            // See "Prove grand products for MCC" in `zkEngine_dev/src/wasm_snark/mod.rs`.
            let table_proof = TableProof {};

            TransactionProof {
                continuations,
                program_proofs,
                table_proof,
            }
        })
    }
}
