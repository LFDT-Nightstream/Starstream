use crate::component::WasmtimeComponentStarstreamExecutor;
use starstream_interleaving_spec::builder::TransactionBuilder;
use starstream_interleaving_spec::{
    CoroutineState, Hash, Ledger, ProcessId, ProvenTransaction, Ref, UtxoId, Value, WasmModule,
    WitEffectOutput, WitLedgerEffect, ZkTransactionProof,
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
        let traces: HashMap<ProcessId, Vec<WitLedgerEffect>> = runtime
            .executor()
            .traces()
            .iter()
            .map(|(pid, trace)| (*pid, normalize_runtime_trace(trace)))
            .collect();
        let traces =
            resolve_resume_outputs(remap_refs_to_spec_numbering(&traces, runtime.effect_log()));
        let state = runtime.executor().state();
        let resume_map = collect_resume_edges(&traces);
        let spawn_map = collect_spawn_edges(&traces);

        let mut builder = TransactionBuilder::new();

        for input in &self.inputs {
            let mut trace = traces
                .get(&input.pid)
                .cloned()
                .ok_or(TransactionSessionError::MissingTrace(input.pid))?;
            if let Some((caller, val)) = resume_map.get(&input.pid).copied() {
                prepend_if_missing(
                    &mut trace,
                    WitLedgerEffect::Activation {
                        val: WitEffectOutput::Resolved(val),
                        caller: WitEffectOutput::Resolved(caller),
                    },
                );
            }
            let continuation = derive_input_continuation(&self.ledger, &input.utxo_id, &trace)?;
            builder = builder.with_input(input.utxo_id.clone(), continuation, trace);
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
            if let Some((caller, val)) = resume_map.get(&pid).copied() {
                prepend_if_missing(
                    &mut trace,
                    WitLedgerEffect::Activation {
                        val: WitEffectOutput::Resolved(val),
                        caller: WitEffectOutput::Resolved(caller),
                    },
                );
            }
            builder = builder.with_fresh_output(
                starstream_interleaving_spec::NewOutput {
                    state: CoroutineState {
                        pc: 0,
                        globals: vec![],
                    },
                    contract_hash: slot.definition.program_hash,
                },
                trace,
            );
        }

        for pid in &self.coord_pids {
            let slot = state
                .processes
                .get(pid.0)
                .ok_or(TransactionSessionError::MissingProcess(*pid))?;
            let trace = traces.get(pid).cloned().unwrap_or_default();
            builder = builder.with_coord_script(slot.definition.program_hash, trace);
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

fn collect_resume_edges(
    traces: &HashMap<ProcessId, Vec<WitLedgerEffect>>,
) -> HashMap<ProcessId, (ProcessId, Ref)> {
    let mut edges = HashMap::new();
    for (caller, trace) in traces {
        for effect in trace {
            if let WitLedgerEffect::Resume { target, val, .. } = effect {
                edges.insert(*target, (*caller, *val));
            }
        }
    }
    edges
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

fn normalize_runtime_trace(trace: &[WitLedgerEffect]) -> Vec<WitLedgerEffect> {
    let mut out = Vec::with_capacity(trace.len() * 2);
    for effect in trace {
        out.push(effect.clone());
        if let WitLedgerEffect::NewRef { size, .. } = effect {
            for _ in 0..*size {
                out.push(WitLedgerEffect::RefPush {
                    vals: [Value::nil(); starstream_interleaving_spec::REF_PUSH_WIDTH],
                });
            }
        }
    }
    out
}

fn remap_refs_to_spec_numbering(
    traces: &HashMap<ProcessId, Vec<WitLedgerEffect>>,
    effect_log: &[(ProcessId, WitLedgerEffect)],
) -> HashMap<ProcessId, Vec<WitLedgerEffect>> {
    let mut mapping = HashMap::<Ref, Ref>::new();
    let mut next_ref = 0u64;
    for (_, effect) in effect_log {
        if let WitLedgerEffect::NewRef { size, ret } = effect {
            let old = ret.unwrap();
            mapping.insert(old, Ref(next_ref));
            next_ref += (*size as u64) * (starstream_interleaving_spec::REF_PUSH_WIDTH as u64);
        }
    }

    traces
        .iter()
        .map(|(pid, trace)| {
            let remapped = trace
                .iter()
                .cloned()
                .map(|effect| remap_effect_refs(effect, &mapping))
                .collect();
            (*pid, remapped)
        })
        .collect()
}

fn resolve_resume_outputs(
    mut traces: HashMap<ProcessId, Vec<WitLedgerEffect>>,
) -> HashMap<ProcessId, Vec<WitLedgerEffect>> {
    let terminal_values: HashMap<ProcessId, Ref> = traces
        .iter()
        .filter_map(|(pid, trace)| {
            trace.iter().find_map(|effect| match effect {
                WitLedgerEffect::Yield { val } => Some((*pid, *val)),
                WitLedgerEffect::Burn { ret } => Some((*pid, *ret)),
                _ => None,
            })
        })
        .collect();

    for trace in traces.values_mut() {
        for effect in trace.iter_mut() {
            if let WitLedgerEffect::Resume { target, ret, .. } = effect
                && let Some(value) = terminal_values.get(target).copied()
            {
                *ret = WitEffectOutput::Resolved(value);
            }
        }
    }

    traces
}

fn remap_effect_refs(effect: WitLedgerEffect, mapping: &HashMap<Ref, Ref>) -> WitLedgerEffect {
    match effect {
        WitLedgerEffect::Resume {
            target,
            f_id,
            val,
            ret,
            caller,
        } => WitLedgerEffect::Resume {
            target,
            f_id,
            val: remap_ref(val, mapping),
            ret: WitEffectOutput::Resolved(remap_ref(ret.unwrap(), mapping)),
            caller,
        },
        WitLedgerEffect::Yield { val } => WitLedgerEffect::Yield {
            val: remap_ref(val, mapping),
        },
        WitLedgerEffect::ProgramHash {
            target,
            program_hash,
        } => WitLedgerEffect::ProgramHash {
            target,
            program_hash,
        },
        WitLedgerEffect::NewUtxo {
            program_hash,
            val,
            id,
        } => WitLedgerEffect::NewUtxo {
            program_hash,
            val: remap_ref(val, mapping),
            id,
        },
        WitLedgerEffect::NewCoord {
            program_hash,
            val,
            id,
        } => WitLedgerEffect::NewCoord {
            program_hash,
            val: remap_ref(val, mapping),
            id,
        },
        WitLedgerEffect::CallEffectHandler {
            interface_id,
            val,
            ret,
        } => WitLedgerEffect::CallEffectHandler {
            interface_id,
            val: remap_ref(val, mapping),
            ret: WitEffectOutput::Resolved(remap_ref(ret.unwrap(), mapping)),
        },
        WitLedgerEffect::Burn { ret } => WitLedgerEffect::Burn {
            ret: remap_ref(ret, mapping),
        },
        WitLedgerEffect::Activation { val, caller } => WitLedgerEffect::Activation {
            val: WitEffectOutput::Resolved(remap_ref(val.unwrap(), mapping)),
            caller,
        },
        WitLedgerEffect::Init { val, caller } => WitLedgerEffect::Init {
            val: WitEffectOutput::Resolved(remap_ref(val.unwrap(), mapping)),
            caller,
        },
        WitLedgerEffect::NewRef { size, ret } => WitLedgerEffect::NewRef {
            size,
            ret: WitEffectOutput::Resolved(remap_ref(ret.unwrap(), mapping)),
        },
        WitLedgerEffect::RefGet { reff, offset, ret } => WitLedgerEffect::RefGet {
            reff: remap_ref(reff, mapping),
            offset,
            ret,
        },
        WitLedgerEffect::RefWrite { reff, offset, vals } => WitLedgerEffect::RefWrite {
            reff: remap_ref(reff, mapping),
            offset,
            vals,
        },
        other => other,
    }
}

fn remap_ref(reff: Ref, mapping: &HashMap<Ref, Ref>) -> Ref {
    mapping.get(&reff).copied().unwrap_or(reff)
}

fn prepend_if_missing(trace: &mut Vec<WitLedgerEffect>, effect: WitLedgerEffect) {
    let already_present = match (&effect, trace.first()) {
        (WitLedgerEffect::Activation { .. }, Some(WitLedgerEffect::Activation { .. })) => true,
        (WitLedgerEffect::Init { .. }, Some(WitLedgerEffect::Init { .. })) => true,
        _ => false,
    };
    if !already_present {
        trace.insert(0, effect);
    }
}

#[cfg(test)]
mod tests {
    #![allow(dead_code)]

    use super::*;
    use crate::component::WasmtimeComponentStarstreamExecutor;
    use crate::state::ProcessKind;
    use imbl::HashMap as ImHashMap;
    use starstream_interleaving_spec::{CoroutineId, UtxoEntry};

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
                state: CoroutineState {
                    pc: 0,
                    globals: vec![],
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

    fn yielding_component() -> Vec<u8> {
        wat::parse_str(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "starstream-ref-write" (func $ref-write
                  (param "reff" u64)
                  (param "offset" u32)
                  (param "a0" u64)
                  (param "a1" u64)
                  (param "a2" u64)
                  (param "a3" u64)))
                (export "yield" (func $yield (param "payload" u64)))
              ))

              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $ref-write-lowered (canon lower (func $ledger "starstream-ref-write")))
              (core func $yield-lowered (canon lower (func $ledger "yield")))

              (core module $m
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "starstream-ref-write"
                  (func $ref-write (param i64 i32 i64 i64 i64 i64)))
                (import "ledger" "yield" (func $yield (param i64)))

                (func (export "step") (local i64)
                  i32.const 1
                  call $new-ref
                  local.tee 0
                  i32.const 0
                  i64.const 42
                  i64.const 0
                  i64.const 0
                  i64.const 0
                  call $ref-write
                  local.get 0
                  call $yield))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "starstream-ref-write" (func $ref-write-lowered))
                    (export "yield" (func $yield-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
        )
        .unwrap()
    }

    fn resuming_component() -> Vec<u8> {
        wat::parse_str(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "utxo" (type (sub resource)))
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "input-utxo" (func $input-utxo (param "index" u32) (result (own 0))))
                (export "resume" (func $resume (param "target" (borrow 0)) (param "payload" u64)))
              ))

              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $input-utxo-lowered (canon lower (func $ledger "input-utxo")))
              (core func $resume-lowered (canon lower (func $ledger "resume")))

              (core module $m
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "input-utxo" (func $input-utxo (param i32) (result i32)))
                (import "ledger" "resume" (func $resume (param i32 i64)))
                (func (export "step") (local i32) (local i64)
                  i32.const 0
                  call $input-utxo
                  local.set 0
                  i32.const 1
                  call $new-ref
                  local.set 1
                  local.get 0
                  local.get 1
                  call $resume))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "input-utxo" (func $input-utxo-lowered))
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "resume" (func $resume-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
        )
        .unwrap()
    }

    fn amount_component(amount: u64) -> Vec<u8> {
        wat::parse_str(&format!(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "starstream-ref-write" (func $ref-write
                  (param "reff" u64)
                  (param "offset" u32)
                  (param "a0" u64)
                  (param "a1" u64)
                  (param "a2" u64)
                  (param "a3" u64)))
                (export "yield" (func $yield (param "payload" u64)))
              ))

              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $ref-write-lowered (canon lower (func $ledger "starstream-ref-write")))
              (core func $yield-lowered (canon lower (func $ledger "yield")))

              (core module $m
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "starstream-ref-write"
                  (func $ref-write (param i64 i32 i64 i64 i64 i64)))
                (import "ledger" "yield" (func $yield (param i64)))

                (func $emit (local i64)
                  i32.const 1
                  call $new-ref
                  local.tee 0
                  i32.const 0
                  i64.const {amount}
                  i64.const 0
                  i64.const 0
                  i64.const 0
                  call $ref-write
                  local.get 0
                  call $yield)

                (func (export "step")
                  call $emit)

                (func (export "amount")
                  call $emit))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "starstream-ref-write" (func $ref-write-lowered))
                    (export "yield" (func $yield-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step")))
              (func (export "amount") (canon lift (core func $i "amount"))))
            "#,
            amount = amount,
        ))
        .unwrap()
    }

    #[allow(dead_code)]
    fn amount_query_coord_component(import_name: &str) -> Vec<u8> {
        wat::parse_str(&format!(
            r#"
            (component
              (import "{import_name}" (instance $utxo-api
                (export "utxo" (type (sub resource)))
                (export "handle" (func $handle (result (own 0))))
                (export "[method]utxo.amount" (func $amount (param "self" (borrow 0)) (result u64)))
              ))
              (import "ledger" (instance $ledger
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "starstream-ref-write" (func $ref-write
                  (param "reff" u64)
                  (param "offset" u32)
                  (param "a0" u64)
                  (param "a1" u64)
                  (param "a2" u64)
                  (param "a3" u64)))
                (export "yield" (func $yield (param "payload" u64)))
              ))

              (core func $handle-lowered (canon lower (func $utxo-api "handle")))
              (core func $amount-lowered (canon lower (func $utxo-api "[method]utxo.amount")))
              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $ref-write-lowered (canon lower (func $ledger "starstream-ref-write")))
              (core func $yield-lowered (canon lower (func $ledger "yield")))

              (core module $m
                (import "{import_name}" "handle" (func $handle (result i32)))
                (import "{import_name}" "[method]utxo.amount" (func $amount (param i32) (result i64)))
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "starstream-ref-write"
                  (func $ref-write (param i64 i32 i64 i64 i64 i64)))
                (import "ledger" "yield" (func $yield (param i64)))
                (func (export "step") (local i32) (local i64) (local i64)
                  call $handle
                  local.set 0
                  local.get 0
                  call $amount
                  local.set 1
                  i32.const 1
                  call $new-ref
                  local.set 2
                  local.get 2
                  i32.const 0
                  local.get 1
                  i64.const 0
                  i64.const 0
                  i64.const 0
                  call $ref-write
                  local.get 2
                  call $yield))

              (core instance $i
                (instantiate $m
                  (with "{import_name}" (instance
                    (export "handle" (func $handle-lowered))
                    (export "[method]utxo.amount" (func $amount-lowered))
                  ))
                  (with "ledger" (instance
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "starstream-ref-write" (func $ref-write-lowered))
                    (export "yield" (func $yield-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
            import_name = import_name,
        ))
        .unwrap()
    }

    fn amount_query_returning_coord_component(import_name: &str) -> Vec<u8> {
        wat::parse_str(&format!(
            r#"
            (component
              (import "{import_name}" (instance $utxo-api
                (export "utxo" (type (sub resource)))
                (export "handle" (func $handle (result (own 0))))
                (export "[method]utxo.amount" (func $amount (param "self" (borrow 0)) (result u64)))
              ))
              (import "ledger" (instance $ledger
                (export "return" (func $return))
              ))

              (core func $handle-lowered (canon lower (func $utxo-api "handle")))
              (core func $amount-lowered (canon lower (func $utxo-api "[method]utxo.amount")))
              (core func $return-lowered (canon lower (func $ledger "return")))

              (core module $m
                (import "{import_name}" "handle" (func $handle (result i32)))
                (import "{import_name}" "[method]utxo.amount" (func $amount (param i32) (result i64)))
                (import "ledger" "return" (func $return))
                (func (export "step")
                  call $handle
                  call $amount
                  drop
                  call $return))

              (core instance $i
                (instantiate $m
                  (with "{import_name}" (instance
                    (export "handle" (func $handle-lowered))
                    (export "[method]utxo.amount" (func $amount-lowered))
                  ))
                  (with "ledger" (instance
                    (export "return" (func $return-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
            import_name = import_name,
        ))
        .unwrap()
    }

    #[test]
    fn session_can_build_and_apply_transaction_from_method_call_execution() {
        let mut runtime = WasmtimeComponentStarstreamExecutor::new().unwrap();
        let input_hash = hash([5, 6, 7, 8]);
        let coord_hash = hash([21, 22, 23, 24]);
        let (ledger, input_utxo_id) = genesis_ledger(input_hash);

        let utxo_program = amount_component(55);
        let coord_program = amount_query_returning_coord_component("other-utxo-api");

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
        session.bind_input(&mut runtime, 7, utxo_pid, input_utxo_id.clone());
        runtime.bind_import_resource("other-utxo-api", 7);
        session.register_coord(coord_pid);

        runtime.run_step(coord_pid, &coord_instance).unwrap();

        let tx = session.build_transaction(&runtime).unwrap();
        assert_eq!(tx.body.inputs, vec![input_utxo_id.clone()]);
        assert_eq!(tx.body.new_outputs.len(), 0);
        assert_eq!(tx.body.coordination_scripts_keys, vec![coord_hash]);

        let applied = session.apply_transaction(&runtime).unwrap();
        assert!(applied.utxos.contains_key(&input_utxo_id));
    }
}
