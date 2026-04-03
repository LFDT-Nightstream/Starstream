use crate::component::WasmtimeComponentStarstreamExecutor;
use starstream_interleaving_proof::commit as commit_trace_effect;
use starstream_interleaving_spec::builder::TransactionBuilder;
use starstream_interleaving_spec::{
    CoroutineState, Hash, Ledger, LedgerEffectsCommitment, ProcessId, ProvenTransaction, Ref,
    UtxoId, WasmModule, WitEffectOutput, WitLedgerEffect, ZkTransactionProof,
};
use std::collections::{BTreeSet, HashMap};

#[derive(Clone, Debug)]
pub struct SessionInputBinding {
    pub resource: u32,
    pub pid: ProcessId,
    pub utxo_id: UtxoId,
}

#[derive(Clone)]
pub struct TransactionSession {
    pub ledger: Ledger,
    pub inputs: Vec<SessionInputBinding>,
    pub coord_pids: Vec<ProcessId>,
}

#[derive(Debug, thiserror::Error)]
pub enum TransactionSessionError {
    #[error("missing runtime trace for process {0:?}")]
    MissingTrace(ProcessId),
    #[error("missing utxo entry for input")]
    MissingInputUtxo,
    #[error("missing process slot for pid {0:?}")]
    MissingProcess(ProcessId),
    #[error("failed to apply generated transaction: {0}")]
    Apply(#[from] starstream_interleaving_spec::VerificationError),
}

impl TransactionSession {
    pub fn new(ledger: Ledger) -> Self {
        Self {
            ledger,
            inputs: Vec::new(),
            coord_pids: Vec::new(),
        }
    }

    pub fn bind_input(
        &mut self,
        runtime: &mut WasmtimeComponentStarstreamExecutor,
        resource: u32,
        pid: ProcessId,
        utxo_id: UtxoId,
    ) {
        runtime.executor_mut().bind_resource(resource, pid);
        self.inputs.push(SessionInputBinding {
            resource,
            pid,
            utxo_id,
        });
        runtime.set_input_resources(self.inputs.iter().map(|input| input.resource).collect());
    }

    pub fn register_coord(&mut self, pid: ProcessId) {
        self.coord_pids.push(pid);
    }

    pub fn build_transaction(
        &self,
        runtime: &WasmtimeComponentStarstreamExecutor,
    ) -> Result<ProvenTransaction, TransactionSessionError> {
        let traces = runtime.executor().traces().clone();
        let state = runtime.executor().state();
        let spawn_map = collect_spawn_edges(&traces);

        let mut builder = TransactionBuilder::new();

        for input in &self.inputs {
            let trace = traces
                .get(&input.pid)
                .cloned()
                .ok_or(TransactionSessionError::MissingTrace(input.pid))?;
            let continuation = derive_input_continuation(&self.ledger, &input.utxo_id, &trace)?;
            let host_calls_root = trace_commitment(&trace);
            builder = builder.with_input_and_trace_commitment(
                input.utxo_id.clone(),
                continuation,
                trace,
                host_calls_root,
            );
        }

        let input_pids: BTreeSet<usize> = self.inputs.iter().map(|input| input.pid.0).collect();
        let coord_pids: BTreeSet<usize> = self.coord_pids.iter().map(|pid| pid.0).collect();
        let mut created_pids = BTreeSet::new();
        created_pids.extend(spawn_map.keys().map(|pid| pid.0));

        for pid_idx in created_pids {
            let pid = ProcessId(pid_idx);
            let slot = state
                .processes
                .get(pid_idx)
                .ok_or(TransactionSessionError::MissingProcess(pid))?;
            let mut trace = traces.get(&pid).cloned().unwrap_or_default();
            if let Some((creator, init, _)) = spawn_map.get(&pid).copied() {
                prepend_if_missing(
                    &mut trace,
                    WitLedgerEffect::Init {
                        val: WitEffectOutput::Resolved(init),
                        caller: WitEffectOutput::Resolved(creator),
                    },
                );
            }
            let host_calls_root = trace_commitment(&trace);
            builder = builder.with_fresh_output_and_trace_commitment(
                starstream_interleaving_spec::NewOutput {
                    state: CoroutineState::Utxo { storage: vec![] },
                    contract_hash: slot.definition.program_hash,
                },
                trace,
                host_calls_root,
            );
        }

        for pid in &self.coord_pids {
            let slot = state
                .processes
                .get(pid.0)
                .ok_or(TransactionSessionError::MissingProcess(*pid))?;
            let trace = traces.get(pid).cloned().unwrap_or_default();
            let host_calls_root = trace_commitment(&trace);
            builder = builder.with_coord_script_and_trace_commitment(
                slot.definition.program_hash,
                trace,
                host_calls_root,
            );
        }

        if let Some(entrypoint) = self.coord_pids.first() {
            builder = builder.with_entrypoint(entrypoint.0);
        }

        let _ = input_pids;
        let _ = coord_pids;
        Ok(builder.build(ZkTransactionProof::Dummy))
    }

    pub fn apply_transaction(
        &self,
        runtime: &WasmtimeComponentStarstreamExecutor,
    ) -> Result<Ledger, TransactionSessionError> {
        let tx = self.build_transaction(runtime)?;
        Ok(self.ledger.apply_transaction(&tx)?)
    }
}

fn derive_input_continuation(
    ledger: &Ledger,
    utxo_id: &UtxoId,
    trace: &[WitLedgerEffect],
) -> Result<Option<CoroutineState>, TransactionSessionError> {
    if trace
        .iter()
        .any(|effect| matches!(effect, WitLedgerEffect::Burn { .. }))
    {
        return Ok(None);
    }
    let entry = ledger
        .utxos
        .get(utxo_id)
        .ok_or(TransactionSessionError::MissingInputUtxo)?;
    Ok(Some(entry.state.clone()))
}

fn collect_spawn_edges(
    traces: &HashMap<ProcessId, Vec<WitLedgerEffect>>,
) -> HashMap<ProcessId, (ProcessId, Ref, Hash<WasmModule>)> {
    let mut edges = HashMap::new();
    for (caller, trace) in traces {
        for effect in trace {
            if let WitLedgerEffect::NewUtxo {
                program_hash,
                val,
                id,
            } = effect
            {
                let pid = id.unwrap();
                edges.insert(pid, (*caller, *val, *program_hash));
            }
        }
    }
    edges
}

fn prepend_if_missing(trace: &mut Vec<WitLedgerEffect>, effect: WitLedgerEffect) {
    let front = trace.first();
    let second = trace.get(1);
    let already_present = match (&effect, front, second) {
        (WitLedgerEffect::Activation { .. }, Some(WitLedgerEffect::Activation { .. }), _) => true,
        (WitLedgerEffect::Init { .. }, Some(WitLedgerEffect::Init { .. }), _) => true,
        (
            WitLedgerEffect::Activation { .. },
            Some(WitLedgerEffect::Enter { .. }),
            Some(WitLedgerEffect::Activation { .. }),
        ) => true,
        (
            WitLedgerEffect::Init { .. },
            Some(WitLedgerEffect::Enter { .. }),
            Some(WitLedgerEffect::Init { .. }),
        ) => true,
        _ => false,
    };
    if !already_present {
        // `Enter` is the only Starstream-visible effect allowed before the
        // derived callee-entry context (`Activation` / `Init`) for these paths.
        trace.insert(index_after_optional_enter(trace), effect);
    }
}

fn trace_commitment(trace: &[WitLedgerEffect]) -> LedgerEffectsCommitment {
    trace
        .iter()
        .cloned()
        .fold(LedgerEffectsCommitment::iv(), commit_trace_effect)
}

fn index_after_optional_enter(trace: &[WitLedgerEffect]) -> usize {
    usize::from(matches!(trace.first(), Some(WitLedgerEffect::Enter { .. })))
}

#[cfg(test)]
mod tests {
    #![allow(dead_code)]

    use super::*;
    use crate::component::WasmtimeComponentStarstreamExecutor;
    use crate::state::ProcessKind;
    use imbl::HashMap as ImHashMap;
    use starstream_interleaving_spec::{CoroutineId, UtxoEntry};

    fn print_wat(name: &str, wasm: &[u8]) {
        if std::env::var_os("DEBUG_COMPONENTS").is_none() {
            return;
        }

        match wasmprinter::print_bytes(wasm) {
            Ok(wat) => eprintln!("--- WAT: {name} ---\n{wat}"),
            Err(err) => eprintln!("--- WAT: {name} (failed: {err}) ---"),
        }
    }

    fn print_component_wit(name: &str, wasm: &[u8]) {
        if std::env::var_os("DEBUG_COMPONENTS").is_none() {
            return;
        }

        match wit_component::decode(wasm) {
            Ok(decoded) => {
                let mut printer = wit_component::WitPrinter::default();
                match printer.print(decoded.resolve(), decoded.package(), &[]) {
                    Ok(()) => eprintln!("--- WIT: {name} ---\n{}", printer.output),
                    Err(err) => eprintln!("--- WIT: {name} (print failed: {err}) ---"),
                }
            }
            Err(err) => eprintln!("--- WIT: {name} (decode failed: {err}) ---"),
        }
    }

    fn print_runtime_traces(runtime: &WasmtimeComponentStarstreamExecutor) {
        if std::env::var_os("DEBUG_TRACES").is_none() {
            return;
        }

        eprintln!("--- Runtime Effect Log ---");
        for (i, (pid, effect)) in runtime.effect_log().iter().enumerate() {
            eprintln!("  [{i}] pid {pid:?}: {effect:?}");
        }
    }

    fn print_transaction_traces(tx: &ProvenTransaction) {
        if std::env::var_os("DEBUG_TRACES").is_none() {
            return;
        }

        eprintln!("--- Transaction Spending Proofs ---");
        for (i, proof) in tx.witness.spending_proofs.iter().enumerate() {
            eprintln!("spending[{i}]:");
            for (j, effect) in proof.trace.iter().enumerate() {
                eprintln!("  [{j}] {effect:?}");
            }
        }

        eprintln!("--- Transaction New Output Proofs ---");
        for (i, proof) in tx.witness.new_output_proofs.iter().enumerate() {
            eprintln!("new_output[{i}]:");
            for (j, effect) in proof.trace.iter().enumerate() {
                eprintln!("  [{j}] {effect:?}");
            }
        }

        eprintln!("--- Transaction Coord Proofs ---");
        for (i, proof) in tx.witness.coordination_scripts.iter().enumerate() {
            eprintln!("coord[{i}]:");
            for (j, effect) in proof.trace.iter().enumerate() {
                eprintln!("  [{j}] {effect:?}");
            }
        }
    }

    fn hash(bytes: [u64; 4]) -> Hash<WasmModule> {
        Hash(bytes, std::marker::PhantomData)
    }

    fn genesis_ledger(input_hash: Hash<WasmModule>) -> (Ledger, UtxoId) {
        let utxo_id = UtxoId {
            contract_hash: input_hash,
            nonce: 0,
        };
        let coroutine_id = CoroutineId {
            creation_tx_hash: Hash([1, 0, 0, 0], std::marker::PhantomData),
            creation_output_index: 0,
        };
        let mut ledger = Ledger {
            utxos: ImHashMap::new(),
            contract_counters: ImHashMap::new(),
            utxo_to_coroutine: ImHashMap::new(),
            ownership_registry: ImHashMap::new(),
        };
        ledger.utxos.insert(
            utxo_id.clone(),
            UtxoEntry {
                state: CoroutineState::Utxo { storage: vec![] },
                contract_hash: input_hash,
            },
        );
        ledger
            .utxo_to_coroutine
            .insert(utxo_id.clone(), coroutine_id);
        ledger.contract_counters.insert(input_hash, 1);
        ledger.contract_counters.insert(hash([9, 10, 11, 12]), 0);
        (ledger, utxo_id)
    }

    fn spendable_utxo_component(initial_amount: u64) -> Vec<u8> {
        wat::parse_str(&format!(
            r#"
            (component
              (import "utxo-api" (instance $shared-utxo-api
                (export "utxo" (type (sub resource)))
              ))
              (alias export $shared-utxo-api "utxo" (type $shared-utxo))

              (import "ledger" (instance $ledger
                (export "burn" (func $burn))
              ))

              (core func $burn-lowered (canon lower (func $ledger "burn")))

              (core module $m
                (import "ledger" "burn" (func $burn))
                (global $amount (mut i64) (i64.const {initial_amount}))
                (global $owner (mut i64) (i64.const 7))
                (func $utxo-dtor (param i32))

                (func $main-impl
                  global.get $amount
                  i64.eqz
                  if
                    call $burn
                  end)

                (func (export "main")
                  call $main-impl)

                (func (export "amount") (result i64)
                  global.get $amount)

                (func (export "spend") (param i64)
                  global.get $amount
                  local.get 0
                  i64.sub
                  global.set $amount
                  call $main-impl)

                (func (export "utxo-api#[method]utxo.amount") (param i32) (result i64)
                  local.get 0
                  drop
                  global.get $amount)

                (func (export "utxo-api#[method]utxo.spend") (param i32 i64)
                  local.get 0
                  drop
                  global.get $amount
                  local.get 1
                  i64.sub
                  global.set $amount
                  call $main-impl))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "burn" (func $burn-lowered))
                  ))
                )
              )

              (type $utxo-amount-type (func (param "self" (borrow $shared-utxo)) (result u64)))
              (alias core export $i "utxo-api#[method]utxo.amount" (core func $utxo-amount-core))
              (func $utxo-amount-lifted
                (type $utxo-amount-type)
                (canon lift (core func $utxo-amount-core)))

              (type $utxo-spend-type (func (param "self" (borrow $shared-utxo)) (param "tokens" u64)))
              (alias core export $i "utxo-api#[method]utxo.spend" (core func $utxo-spend-core))
              (func $utxo-spend-lifted
                (type $utxo-spend-type)
                (canon lift (core func $utxo-spend-core)))

              (component $utxo-api
                (import "import-type-utxo" (type $import-utxo (sub resource)))
                (import "import-method-utxo-amount"
                  (func $import-utxo-amount (param "self" (borrow $import-utxo)) (result u64)))
                (import "import-method-utxo-spend"
                  (func $import-utxo-spend (param "self" (borrow $import-utxo)) (param "tokens" u64)))
                (export $utxo "utxo" (type $import-utxo))
                (export "[method]utxo.amount"
                  (func $import-utxo-amount)
                  (func (param "self" (borrow $utxo)) (result u64)))
                (export "[method]utxo.spend"
                  (func $import-utxo-spend)
                  (func (param "self" (borrow $utxo)) (param "tokens" u64))))

              (instance $utxo-api-instance
                (instantiate $utxo-api
                  (with "import-type-utxo" (type $shared-utxo))
                  (with "import-method-utxo-amount" (func $utxo-amount-lifted))
                  (with "import-method-utxo-spend" (func $utxo-spend-lifted))
                )
              )

              (func (export "main") (canon lift (core func $i "main")))
              (func (export "amount") (result u64) (canon lift (core func $i "amount")))
              (func (export "spend") (param "tokens" u64) (canon lift (core func $i "spend")))
              (export "utxo-api" (instance $utxo-api-instance)))
            "#,
            initial_amount = initial_amount,
        ))
        .unwrap()
    }

    fn amount_and_spend_coord_component(
        import_name: &str,
        first_spend_amount: u64,
        second_spend_amount: u64,
    ) -> Vec<u8> {
        wat::parse_str(&format!(
            r#"
            (component
              (import "{import_name}" (instance $utxo-api
                (export "utxo" (type (sub resource)))
                (export "handle" (func $handle (result (own 0))))
                (export "[method]utxo.amount" (func $amount (param "self" (borrow 0)) (result u64)))
                (export "[method]utxo.spend" (func $spend (param "self" (borrow 0)) (param "tokens" u64)))
              ))

              (core func $handle-lowered (canon lower (func $utxo-api "handle")))
              (core func $amount-lowered (canon lower (func $utxo-api "[method]utxo.amount")))
              (core func $spend-lowered (canon lower (func $utxo-api "[method]utxo.spend")))

              (core module $m
                (import "{import_name}" "handle" (func $handle (result i32)))
                (import "{import_name}" "[method]utxo.amount" (func $amount (param i32) (result i64)))
                (import "{import_name}" "[method]utxo.spend" (func $spend (param i32 i64)))
                (func (export "main") (local i32)
                  call $handle
                  local.tee 0
                  call $amount
                  drop
                  local.get 0
                  i64.const {first_spend_amount}
                  call $spend
                  local.get 0
                  call $amount
                  drop
                  local.get 0
                  i64.const {second_spend_amount}
                  call $spend)
              )

              (core instance $i
                (instantiate $m
                  (with "{import_name}" (instance
                    (export "handle" (func $handle-lowered))
                    (export "[method]utxo.amount" (func $amount-lowered))
                    (export "[method]utxo.spend" (func $spend-lowered))
                  ))
                )
              )

              (func (export "main") (canon lift (core func $i "main"))))
            "#,
            import_name = import_name,
            first_spend_amount = first_spend_amount,
            second_spend_amount = second_spend_amount,
        ))
        .unwrap()
    }

    #[test]
    fn test_token_like_utxo_with_coord() {
        const UTXO_RESOURCE: u32 = 1;
        const INITIAL_AMOUNT: u64 = 55;
        const FIRST_SPEND: u64 = 13;
        const FINAL_SPEND: u64 = INITIAL_AMOUNT - FIRST_SPEND;
        const MIN_EXPECTED_UTXO_ENTERS: usize = 3;

        let mut runtime = WasmtimeComponentStarstreamExecutor::new().unwrap();
        let input_hash = hash([5, 6, 7, 8]);
        let coord_hash = hash([21, 22, 23, 24]);
        let (ledger, input_utxo_id) = genesis_ledger(input_hash);

        let utxo_program = spendable_utxo_component(INITIAL_AMOUNT);
        let coord_program = amount_and_spend_coord_component("utxo-api", FIRST_SPEND, FINAL_SPEND);

        print_wat("component-runtime/utxo", &utxo_program);
        print_component_wit("component-runtime/utxo", &utxo_program);
        print_wat("component-runtime/coord", &coord_program);
        print_component_wit("component-runtime/coord", &coord_program);

        let (utxo_pid, utxo_component) = runtime
            .compile_component(&crate::ProgramDefinition {
                kind: ProcessKind::Utxo,
                program_hash: input_hash,
                module_bytes: utxo_program,
            })
            .unwrap();

        let _utxo_instance = runtime
            .instantiate_process(utxo_pid, &utxo_component)
            .unwrap();

        let (coord_pid, coord_component) = runtime
            .compile_component(&crate::ProgramDefinition {
                kind: ProcessKind::Coord,
                program_hash: coord_hash,
                module_bytes: coord_program,
            })
            .unwrap();
        let coord_instance = runtime.instantiate_component(&coord_component).unwrap();

        let mut session = TransactionSession::new(ledger.clone());
        session.bind_input(&mut runtime, UTXO_RESOURCE, utxo_pid, input_utxo_id.clone());
        runtime.bind_import_resource("utxo-api", UTXO_RESOURCE);
        session.register_coord(coord_pid);

        runtime.run_main(coord_pid, &coord_instance).unwrap();
        print_runtime_traces(&runtime);

        let mut tx = session.build_transaction(&runtime).unwrap();
        print_transaction_traces(&tx);
        assert_eq!(tx.body.inputs, vec![input_utxo_id.clone()]);
        assert_eq!(tx.body.new_outputs.len(), 0);
        assert_eq!(tx.body.coordination_scripts_keys, vec![coord_hash]);
        let spending_trace = &tx.witness.spending_proofs[0].trace;
        assert!(
            spending_trace
                .iter()
                .filter(|effect| matches!(effect, WitLedgerEffect::Enter { .. }))
                .count()
                >= MIN_EXPECTED_UTXO_ENTERS
        );

        if std::env::var_os("RUN_ZK_PROOF").is_some() {
            let (inst, wit) = ledger
                .interleaving_artifacts(&tx.body, &tx.witness)
                .unwrap();
            tx.witness.interleaving_proof =
                starstream_interleaving_proof::prove(inst, wit).unwrap();
        }

        let applied = ledger.apply_transaction(&tx).unwrap();
        assert!(!applied.utxos.contains_key(&input_utxo_id));
    }
}
