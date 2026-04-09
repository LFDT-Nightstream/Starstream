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
        if let Some(entry) = self.ledger.utxos.get(&utxo_id) {
            let _ = runtime.restore_process_state(pid, &entry.state);
        }
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
        runtime: &mut WasmtimeComponentStarstreamExecutor,
    ) -> Result<ProvenTransaction, TransactionSessionError> {
        let traces = runtime.executor().traces().clone();
        let spawn_map = collect_spawn_edges(&traces)?;

        let mut builder = TransactionBuilder::new();

        for input in &self.inputs {
            let trace = traces
                .get(&input.pid)
                .cloned()
                .ok_or(TransactionSessionError::MissingTrace(input.pid))?;
            let continuation = derive_input_continuation(
                &self.ledger,
                runtime,
                &input.utxo_id,
                input.pid,
                &trace,
            )?;
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
        let state = runtime.executor().state();

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
        runtime: &mut WasmtimeComponentStarstreamExecutor,
    ) -> Result<Ledger, TransactionSessionError> {
        let tx = self.build_transaction(runtime)?;
        Ok(self.ledger.apply_transaction(&tx)?)
    }
}

fn derive_input_continuation(
    ledger: &Ledger,
    runtime: &mut WasmtimeComponentStarstreamExecutor,
    utxo_id: &UtxoId,
    pid: ProcessId,
    trace: &[WitLedgerEffect],
) -> Result<Option<CoroutineState>, TransactionSessionError> {
    if trace
        .iter()
        .any(|effect| matches!(effect, WitLedgerEffect::Burn {}))
    {
        return Ok(None);
    }
    if let Ok(Some(state)) = runtime.snapshot_process_state(pid) {
        return Ok(Some(state));
    }
    let entry = ledger
        .utxos
        .get(utxo_id)
        .ok_or(TransactionSessionError::MissingInputUtxo)?;
    Ok(Some(entry.state.clone()))
}

fn collect_spawn_edges(
    traces: &HashMap<ProcessId, Vec<WitLedgerEffect>>,
) -> Result<HashMap<ProcessId, (ProcessId, Ref, Hash<WasmModule>)>, TransactionSessionError> {
    let mut edges = HashMap::new();
    for (caller, trace) in traces {
        for effect in trace {
            if let WitLedgerEffect::NewUtxo {
                program_hash,
                val,
                id,
            } = effect
            {
                let WitEffectOutput::Resolved(pid) = id;
                edges.insert(*pid, (*caller, *val, *program_hash));
            }
        }
    }
    Ok(edges)
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
    use super::*;
    use crate::component::WasmtimeComponentStarstreamExecutor;
    use crate::state::ProcessKind;
    use imbl::HashMap as ImHashMap;
    use starstream_interleaving_spec::{CoroutineId, UtxoEntry, Value};

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

        let decoded = std::panic::catch_unwind(|| wit_component::decode(wasm));
        match decoded {
            Ok(Ok(decoded)) => {
                let printed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let mut printer = wit_component::WitPrinter::default();
                    match printer.print(decoded.resolve(), decoded.package(), &[]) {
                        Ok(()) => eprintln!("--- WIT: {name} ---\n{}", printer.output),
                        Err(err) => eprintln!("--- WIT: {name} (print failed: {err}) ---"),
                    }
                }));
                if printed.is_err() {
                    eprintln!("--- WIT: {name} (print panicked) ---");
                }
            }
            Ok(Err(err)) => eprintln!("--- WIT: {name} (decode failed: {err}) ---"),
            Err(_) => eprintln!("--- WIT: {name} (decode panicked) ---"),
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

    fn genesis_ledger(input_hash: Hash<WasmModule>, initial_amount: u64) -> (Ledger, UtxoId) {
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
                state: CoroutineState::Utxo {
                    storage: vec![starstream_interleaving_spec::Value(initial_amount)],
                },
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
              (import "ledger" (instance $ledger
                (export "burn" (func $burn))
              ))

              (core func $burn-lowered (canon lower (func $ledger "burn")))
              (type $utxo (resource (rep i32)))
              (core func $resource.new (canon resource.new $utxo))
              (core func $resource.rep (canon resource.rep $utxo))
              (core func $resource.drop (canon resource.drop $utxo))
              (core instance $export-utxo-api
                (export "[resource-new]utxo" (func $resource.new))
                (export "[resource-rep]utxo" (func $resource.rep))
                (export "[resource-drop]utxo" (func $resource.drop))
              )

              (core module $m
                (import "[export]utxo-api" "[resource-new]utxo" (func $new-utxo (param i32) (result i32)))
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

                (func (export "utxo-api#[method]utxo.main") (param i32)
                  local.get 0
                  drop
                  call $main-impl)

                (func (export "utxo-api#[constructor]utxo") (result i32)
                  i32.const 0
                  call $new-utxo)

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
                  call $main-impl)

                (func (export "snapshot-state") (result i64)
                  global.get $amount)

                (func (export "snapshot-restore") (param i64)
                  local.get 0
                  global.set $amount))

              (core instance $i
                (instantiate $m
                  (with "[export]utxo-api" (instance $export-utxo-api))
                  (with "ledger" (instance
                    (export "burn" (func $burn-lowered))
                  ))
                )
              )

              (type $utxo-main-type (func (param "self" (borrow $utxo))))
              (alias core export $i "utxo-api#[method]utxo.main" (core func $utxo-main-core))
              (func $utxo-main-lifted
                (type $utxo-main-type)
                (canon lift (core func $utxo-main-core)))

              (type $utxo-constructor-type (func (result (own $utxo))))
              (alias core export $i "utxo-api#[constructor]utxo" (core func $utxo-constructor-core))
              (func $utxo-constructor-lifted
                (type $utxo-constructor-type)
                (canon lift (core func $utxo-constructor-core)))

              (type $utxo-amount-type (func (param "self" (borrow $utxo)) (result u64)))
              (alias core export $i "utxo-api#[method]utxo.amount" (core func $utxo-amount-core))
              (func $utxo-amount-lifted
                (type $utxo-amount-type)
                (canon lift (core func $utxo-amount-core)))

              (type $utxo-spend-type (func (param "self" (borrow $utxo)) (param "tokens" u64)))
              (alias core export $i "utxo-api#[method]utxo.spend" (core func $utxo-spend-core))
              (func $utxo-spend-lifted
                (type $utxo-spend-type)
                (canon lift (core func $utxo-spend-core)))

              (type $snapshot-state-type (func (result u64)))
              (alias core export $i "snapshot-state" (core func $snapshot-state-core))
              (func (export "snapshot-state")
                (type $snapshot-state-type)
                (canon lift (core func $snapshot-state-core)))

              (type $snapshot-restore-type (func (param "state" u64)))
              (alias core export $i "snapshot-restore" (core func $snapshot-restore-core))
              (func (export "snapshot-restore")
                (type $snapshot-restore-type)
                (canon lift (core func $snapshot-restore-core)))

              (component $utxo-api
                (import "import-type-utxo" (type $import-utxo (sub resource)))
                (type $own-import-utxo (own $import-utxo))
                (type $constructor-import-utxo (func (result $own-import-utxo)))
                (import "import-constructor-utxo" (func $import-constructor-utxo (type $constructor-import-utxo)))
                (type $borrow-import-utxo-main (borrow $import-utxo))
                (type $method-import-utxo-main (func (param "self" $borrow-import-utxo-main)))
                (import "import-method-utxo-main"
                  (func $import-utxo-main (type $method-import-utxo-main)))
                (type $borrow-import-utxo (borrow $import-utxo))
                (type $method-import-utxo-amount (func (param "self" $borrow-import-utxo) (result u64)))
                (import "import-method-utxo-amount"
                  (func $import-utxo-amount (type $method-import-utxo-amount)))
                (type $borrow-import-utxo-2 (borrow $import-utxo))
                (type $method-import-utxo-spend (func (param "self" $borrow-import-utxo-2) (param "tokens" u64)))
                (import "import-method-utxo-spend"
                  (func $import-utxo-spend (type $method-import-utxo-spend)))
                (export $utxo "utxo" (type $import-utxo))
                (type $own-utxo (own $utxo))
                (type $constructor-utxo (func (result $own-utxo)))
                (export "[constructor]utxo"
                  (func $import-constructor-utxo)
                  (func (type $constructor-utxo)))
                (type $borrow-utxo-main (borrow $utxo))
                (type $method-utxo-main (func (param "self" $borrow-utxo-main)))
                (export "[method]utxo.main"
                  (func $import-utxo-main)
                  (func (type $method-utxo-main)))
                (type $borrow-utxo (borrow $utxo))
                (type $method-utxo-amount (func (param "self" $borrow-utxo) (result u64)))
                (export "[method]utxo.amount"
                  (func $import-utxo-amount)
                  (func (type $method-utxo-amount)))
                (type $borrow-utxo-2 (borrow $utxo))
                (type $method-utxo-spend (func (param "self" $borrow-utxo-2) (param "tokens" u64)))
                (export "[method]utxo.spend"
                  (func $import-utxo-spend)
                  (func (type $method-utxo-spend))))

              (instance $utxo-api-instance
                (instantiate $utxo-api
                  (with "import-type-utxo" (type $utxo))
                  (with "import-method-utxo-main" (func $utxo-main-lifted))
                  (with "import-constructor-utxo" (func $utxo-constructor-lifted))
                  (with "import-method-utxo-amount" (func $utxo-amount-lifted))
                  (with "import-method-utxo-spend" (func $utxo-spend-lifted))
                )
              )

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
                (export "[constructor]utxo" (func $construct (result (own 0))))
                (export "[method]utxo.amount" (func $amount (param "self" (borrow 0)) (result u64)))
                (export "[method]utxo.spend" (func $spend (param "self" (borrow 0)) (param "tokens" u64)))
              ))

              (alias export $utxo-api "utxo" (type $utxo))
              (core func $construct-lowered (canon lower (func $utxo-api "[constructor]utxo")))
              (core func $amount-lowered (canon lower (func $utxo-api "[method]utxo.amount")))
              (core func $spend-lowered (canon lower (func $utxo-api "[method]utxo.spend")))

              (core module $m
                (import "{import_name}" "[constructor]utxo" (func $construct (result i32)))
                (import "{import_name}" "[method]utxo.amount" (func $amount (param i32) (result i64)))
                (import "{import_name}" "[method]utxo.spend" (func $spend (param i32 i64)))
                (func (export "main") (local i32)
                  call $construct
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
                    (export "[constructor]utxo" (func $construct-lowered))
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
        let (ledger, input_utxo_id) = genesis_ledger(input_hash, INITIAL_AMOUNT);

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
        session.register_coord(coord_pid);

        runtime.run_main(coord_pid, &coord_instance).unwrap();
        print_runtime_traces(&runtime);

        let mut tx = session.build_transaction(&mut runtime).unwrap();
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

    #[test]
    fn test_snapshot_state_updates_utxo_continuation() {
        const UTXO_RESOURCE: u32 = 1;
        const INITIAL_AMOUNT: u64 = 55;
        const FIRST_SPEND: u64 = 13;
        const SECOND_SPEND: u64 = 10;
        const EXPECTED_REMAINING: u64 = INITIAL_AMOUNT - FIRST_SPEND - SECOND_SPEND;

        let mut runtime = WasmtimeComponentStarstreamExecutor::new().unwrap();
        let input_hash = hash([41, 42, 43, 44]);
        let coord_hash = hash([51, 52, 53, 54]);
        let (ledger, input_utxo_id) = genesis_ledger(input_hash, INITIAL_AMOUNT);

        let utxo_program = spendable_utxo_component(INITIAL_AMOUNT);
        let coord_program = amount_and_spend_coord_component("utxo-api", FIRST_SPEND, SECOND_SPEND);

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
        session.register_coord(coord_pid);

        runtime.run_main(coord_pid, &coord_instance).unwrap();

        let tx = session.build_transaction(&mut runtime).unwrap();
        match tx.body.continuations.as_slice() {
            [Some(CoroutineState::Utxo { storage })] => {
                assert_eq!(storage.as_slice(), &[Value(EXPECTED_REMAINING)]);
            }
            _ => panic!("unexpected continuation shape"),
        }
    }
}
