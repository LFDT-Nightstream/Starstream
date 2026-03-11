use crate::abi::{FUNCTION_ID_STEP, HostImportCall};
use crate::executor::{ExecutorError, HostImportOutcome, ProgramDefinition, StarstreamExecutor};
use starstream_interleaving_spec::{ProcessId, Ref, Value};
use wasm_runtime_layer::{
    Extern, Func, FuncType, Imports, Instance, Module, Store, StoreContextMut, ValueType,
};

pub type GuestRuntime = wasm_runtime_layer::Engine<wasmtime_runtime_layer::Engine>;
pub type GuestStore<T> = Store<T, wasmtime_runtime_layer::Engine>;
pub type GuestModule = Module;
pub type WasmtimeGuest = Instance;

#[derive(Debug, thiserror::Error)]
pub enum WasmtimeRuntimeError {
    #[error("runtime layer error: {0}")]
    Runtime(#[from] anyhow::Error),
    #[error("executor error: {0}")]
    Executor(#[from] ExecutorError),
    #[error("missing export `{0}`")]
    MissingExport(&'static str),
    #[error("export `{0}` is not a function")]
    ExportNotFunction(&'static str),
}

pub fn default_runtime() -> GuestRuntime {
    wasm_runtime_layer::Engine::new(wasmtime_runtime_layer::Engine::default())
}

#[derive(Debug)]
pub struct RuntimeHostState {
    pub current_process: ProcessId,
    pub executor: StarstreamExecutor<u32>,
}

impl Default for RuntimeHostState {
    fn default() -> Self {
        Self {
            current_process: ProcessId(0),
            executor: StarstreamExecutor::new(),
        }
    }
}

pub struct WasmtimeStarstreamExecutor {
    pub runtime: GuestRuntime,
    pub store: GuestStore<RuntimeHostState>,
    imports: Imports,
}

impl WasmtimeStarstreamExecutor {
    pub fn new() -> Self {
        let runtime = default_runtime();
        let mut store = Store::new(&runtime, RuntimeHostState::default());
        let imports = build_imports(&mut store);
        Self {
            runtime,
            store,
            imports,
        }
    }

    pub fn executor(&self) -> &StarstreamExecutor<u32> {
        &self.store.data().executor
    }

    pub fn executor_mut(&mut self) -> &mut StarstreamExecutor<u32> {
        &mut self.store.data_mut().executor
    }

    pub fn compile_program(
        &mut self,
        program: &ProgramDefinition,
    ) -> Result<(ProcessId, GuestModule), WasmtimeRuntimeError> {
        let pid = self.executor_mut().register_program(program);
        let module = Module::new(&self.runtime, &program.module_bytes)?;
        Ok((pid, module))
    }

    pub fn instantiate_module(
        &mut self,
        module: &GuestModule,
    ) -> Result<WasmtimeGuest, WasmtimeRuntimeError> {
        Ok(Instance::new(&mut self.store, module, &self.imports)?)
    }

    pub fn run_step(
        &mut self,
        pid: ProcessId,
        instance: &WasmtimeGuest,
    ) -> Result<(), WasmtimeRuntimeError> {
        self.store.data_mut().current_process = pid;
        let step = instance
            .get_export(&self.store, "step")
            .ok_or(WasmtimeRuntimeError::MissingExport("step"))?
            .into_func()
            .ok_or(WasmtimeRuntimeError::ExportNotFunction("step"))?;
        let mut results = [];
        step.call(&mut self.store, &[], &mut results)?;
        Ok(())
    }
}

fn build_imports(store: &mut GuestStore<RuntimeHostState>) -> Imports {
    let mut imports = Imports::default();

    imports.define(
        "env",
        "starstream-new-ref",
        Extern::Func(Func::new(
            &mut *store,
            FuncType::new([ValueType::I32], [ValueType::I64]).with_name("starstream-new-ref"),
            |mut ctx: StoreContextMut<'_, RuntimeHostState, wasmtime_runtime_layer::Engine>,
             args,
             results| {
                let size_words = match args {
                    [wasm_runtime_layer::Value::I32(v)] => *v as u32,
                    _ => anyhow::bail!("invalid args for starstream-new-ref"),
                };
                let caller = ctx.data().current_process;
                let outcome = ctx
                    .data_mut()
                    .executor
                    .record_import(caller, HostImportCall::NewRef { size_words })?;
                let reff = match outcome {
                    HostImportOutcome::Ref(reff) => reff,
                    _ => anyhow::bail!("starstream-new-ref must return a ref"),
                };
                results[0] = wasm_runtime_layer::Value::I64(reff.0 as i64);
                Ok(())
            },
        )),
    );

    imports.define(
        "env",
        "starstream-ref-get",
        Extern::Func(Func::new(
            &mut *store,
            FuncType::new(
                [ValueType::I64, ValueType::I32],
                [
                    ValueType::I64,
                    ValueType::I64,
                    ValueType::I64,
                    ValueType::I64,
                ],
            )
            .with_name("starstream-ref-get"),
            |mut ctx: StoreContextMut<'_, RuntimeHostState, wasmtime_runtime_layer::Engine>,
             args,
             results| {
                let (reff, offset_words) = match args {
                    [
                        wasm_runtime_layer::Value::I64(reff),
                        wasm_runtime_layer::Value::I32(offset),
                    ] => (Ref(*reff as u64), *offset as u32),
                    _ => anyhow::bail!("invalid args for starstream-ref-get"),
                };
                let caller = ctx.data().current_process;
                let outcome = ctx
                    .data_mut()
                    .executor
                    .record_import(caller, HostImportCall::RefGet { reff, offset_words })?;
                let lanes = match outcome {
                    HostImportOutcome::Lanes(lanes) => lanes,
                    _ => anyhow::bail!("starstream-ref-get must return lanes"),
                };
                for (idx, lane) in lanes.into_iter().enumerate() {
                    results[idx] = wasm_runtime_layer::Value::I64(lane.0 as i64);
                }
                Ok(())
            },
        )),
    );

    imports.define(
        "env",
        "starstream-ref-write",
        Extern::Func(Func::new(
            &mut *store,
            FuncType::new(
                [
                    ValueType::I64,
                    ValueType::I32,
                    ValueType::I64,
                    ValueType::I64,
                    ValueType::I64,
                    ValueType::I64,
                ],
                [],
            )
            .with_name("starstream-ref-write"),
            |mut ctx: StoreContextMut<'_, RuntimeHostState, wasmtime_runtime_layer::Engine>,
             args,
             _results| {
                let (reff, offset_words, lanes) = match args {
                    [
                        wasm_runtime_layer::Value::I64(reff),
                        wasm_runtime_layer::Value::I32(offset),
                        wasm_runtime_layer::Value::I64(a0),
                        wasm_runtime_layer::Value::I64(a1),
                        wasm_runtime_layer::Value::I64(a2),
                        wasm_runtime_layer::Value::I64(a3),
                    ] => (
                        Ref(*reff as u64),
                        *offset as u32,
                        [
                            Value(*a0 as u64),
                            Value(*a1 as u64),
                            Value(*a2 as u64),
                            Value(*a3 as u64),
                        ],
                    ),
                    _ => anyhow::bail!("invalid args for starstream-ref-write"),
                };
                let caller = ctx.data().current_process;
                ctx.data_mut().executor.record_import(
                    caller,
                    HostImportCall::RefWrite {
                        reff,
                        offset_words,
                        lanes,
                    },
                )?;
                Ok(())
            },
        )),
    );

    imports.define(
        "env",
        "starstream-resume",
        Extern::Func(Func::new(
            &mut *store,
            FuncType::new([ValueType::I32, ValueType::I64], []).with_name("starstream-resume"),
            |mut ctx: StoreContextMut<'_, RuntimeHostState, wasmtime_runtime_layer::Engine>,
             args,
             _results| {
                let (target, payload) = match args {
                    [
                        wasm_runtime_layer::Value::I32(target),
                        wasm_runtime_layer::Value::I64(payload),
                    ] => (*target as u32, Ref(*payload as u64)),
                    _ => anyhow::bail!("invalid args for starstream-resume"),
                };
                let caller = ctx.data().current_process;
                ctx.data_mut().executor.record_import(
                    caller,
                    HostImportCall::Resume {
                        target,
                        payload,
                        function_id: FUNCTION_ID_STEP,
                    },
                )?;
                Ok(())
            },
        )),
    );

    imports.define(
        "env",
        "starstream-yield",
        Extern::Func(Func::new(
            &mut *store,
            FuncType::new([ValueType::I64], []).with_name("starstream-yield"),
            |mut ctx: StoreContextMut<'_, RuntimeHostState, wasmtime_runtime_layer::Engine>,
             args,
             _results| {
                let payload = match args {
                    [wasm_runtime_layer::Value::I64(payload)] => Ref(*payload as u64),
                    _ => anyhow::bail!("invalid args for starstream-yield"),
                };
                let caller = ctx.data().current_process;
                ctx.data_mut()
                    .executor
                    .record_import(caller, HostImportCall::Yield { payload })?;
                Ok(())
            },
        )),
    );

    imports.define(
        "env",
        "starstream-return",
        Extern::Func(Func::new(
            &mut *store,
            FuncType::new([], []).with_name("starstream-return"),
            |mut ctx: StoreContextMut<'_, RuntimeHostState, wasmtime_runtime_layer::Engine>,
             _args,
             _results| {
                let caller = ctx.data().current_process;
                ctx.data_mut()
                    .executor
                    .record_import(caller, HostImportCall::Return)?;
                Ok(())
            },
        )),
    );

    imports
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::ProcessKind;
    use starstream_interleaving_spec::{Hash, WasmModule, WitLedgerEffect};

    fn dummy_hash() -> Hash<WasmModule> {
        Hash([1, 2, 3, 4], std::marker::PhantomData)
    }

    #[test]
    fn handwritten_module_can_allocate_ref_and_yield() {
        let mut runtime = WasmtimeStarstreamExecutor::new();
        let wat = wat::parse_str(
            r#"
            (module
              (import "env" "starstream-new-ref" (func $new_ref (param i32) (result i64)))
              (import "env" "starstream-ref-write" (func $ref_write (param i64 i32 i64 i64 i64 i64)))
              (import "env" "starstream-yield" (func $yield (param i64)))
              (func (export "step") (local i64)
                i32.const 1
                call $new_ref
                local.tee 0
                i32.const 0
                i64.const 42
                i64.const 0
                i64.const 0
                i64.const 0
                call $ref_write
                local.get 0
                call $yield)
              (memory (export "memory") 1))
            "#,
        )
        .unwrap();

        let program = ProgramDefinition {
            kind: ProcessKind::Utxo,
            program_hash: dummy_hash(),
            module_bytes: wat,
        };

        let (pid, module) = runtime.compile_program(&program).unwrap();
        let instance = runtime.instantiate_module(&module).unwrap();
        runtime.run_step(pid, &instance).unwrap();

        let trace = runtime.executor().traces().get(&pid).unwrap();
        assert!(matches!(trace[0], WitLedgerEffect::NewRef { .. }));
        assert!(matches!(trace[1], WitLedgerEffect::RefWrite { .. }));
        assert!(matches!(trace[2], WitLedgerEffect::Yield { .. }));
    }
}
