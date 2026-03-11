use crate::abi::{FUNCTION_ID_STEP, HostImportCall, TemporaryAbi};
use crate::state::{ProcessDefinition, ProcessKind, ProgramHash, StarstreamState};
use starstream_interleaving_spec::{
    InterfaceId, ProcessId, Ref, Value, WitEffectOutput, WitLedgerEffect,
};
use std::collections::HashMap;

#[derive(Debug, thiserror::Error)]
pub enum ExecutorError {
    #[error("runtime error: {0}")]
    Runtime(String),
    #[error("unknown process resource")]
    UnknownProcessResource,
    #[error("unknown ref")]
    UnknownRef,
    #[error("unsupported feature: {0}")]
    Unsupported(&'static str),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProgramDefinition {
    pub kind: ProcessKind,
    pub program_hash: ProgramHash,
    pub module_bytes: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HostImportOutcome {
    None,
    Ref(Ref),
    Lanes([Value; 4]),
}

#[derive(Debug)]
pub struct StarstreamExecutor<Resource> {
    abi: TemporaryAbi,
    state: StarstreamState<Resource>,
    handler_stack: HashMap<InterfaceId, Vec<ProcessId>>,
    traces: HashMap<ProcessId, Vec<WitLedgerEffect>>,
    effect_log: Vec<(ProcessId, WitLedgerEffect)>,
}

impl<Resource> StarstreamExecutor<Resource>
where
    Resource: Copy + Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self {
            abi: TemporaryAbi,
            state: StarstreamState::default(),
            handler_stack: HashMap::new(),
            traces: HashMap::new(),
            effect_log: Vec::new(),
        }
    }

    pub fn abi(&self) -> TemporaryAbi {
        self.abi
    }

    pub fn state(&self) -> &StarstreamState<Resource> {
        &self.state
    }

    pub fn traces(&self) -> &HashMap<ProcessId, Vec<WitLedgerEffect>> {
        &self.traces
    }

    pub fn effect_log(&self) -> &[(ProcessId, WitLedgerEffect)] {
        &self.effect_log
    }

    pub fn bind_resource(&mut self, resource: Resource, pid: ProcessId) {
        self.state.resources.insert(resource, pid);
        if let Some(slot) = self.state.processes.get_mut(pid.0) {
            slot.exported_resource = Some(resource);
        }
    }

    pub fn register_program(&mut self, program: &ProgramDefinition) -> ProcessId {
        self.state.push_process(
            ProcessDefinition {
                kind: program.kind,
                program_hash: program.program_hash,
            },
            None,
        )
    }

    pub fn record_import(
        &mut self,
        caller: ProcessId,
        call: HostImportCall<Resource>,
    ) -> Result<HostImportOutcome, ExecutorError> {
        let (effect, outcome) = match call {
            HostImportCall::NewRef { size_words } => {
                let reff = self.state.refs.alloc_words(size_words);
                (
                    WitLedgerEffect::NewRef {
                        size: size_words as usize,
                        ret: WitEffectOutput::Resolved(reff),
                    },
                    HostImportOutcome::Ref(reff),
                )
            }
            HostImportCall::RefGet { reff, offset_words } => {
                let lanes = self
                    .state
                    .refs
                    .read_lanes(reff, offset_words)
                    .ok_or(ExecutorError::UnknownRef)?;
                (
                    WitLedgerEffect::RefGet {
                        reff,
                        offset: offset_words as usize,
                        ret: WitEffectOutput::Resolved(lanes),
                    },
                    HostImportOutcome::Lanes(lanes),
                )
            }
            HostImportCall::RefWrite {
                reff,
                offset_words,
                lanes,
            } => {
                if !self.state.refs.write_lanes(reff, offset_words, lanes) {
                    return Err(ExecutorError::UnknownRef);
                }
                (
                    WitLedgerEffect::RefWrite {
                        reff,
                        offset: offset_words as usize,
                        vals: lanes,
                    },
                    HostImportOutcome::None,
                )
            }
            HostImportCall::Resume {
                target,
                payload,
                function_id,
            } => {
                let target = self
                    .state
                    .resolve_resource(target)
                    .ok_or(ExecutorError::UnknownProcessResource)?;
                (
                    WitLedgerEffect::Resume {
                        target,
                        f_id: starstream_interleaving_spec::FunctionId(function_id as usize),
                        val: payload,
                        ret: WitEffectOutput::Resolved(Ref(0)),
                        caller: WitEffectOutput::Resolved(None),
                    },
                    HostImportOutcome::None,
                )
            }
            HostImportCall::Yield { payload } => (
                WitLedgerEffect::Yield { val: payload },
                HostImportOutcome::None,
            ),
            HostImportCall::Return => (WitLedgerEffect::Return {}, HostImportOutcome::None),
            HostImportCall::NewUtxo { program_hash, init } => {
                let target = self.state.push_process(
                    ProcessDefinition {
                        kind: ProcessKind::Utxo,
                        program_hash,
                    },
                    None,
                );
                (
                    WitLedgerEffect::NewUtxo {
                        program_hash,
                        val: init,
                        id: WitEffectOutput::Resolved(target),
                    },
                    HostImportOutcome::None,
                )
            }
            HostImportCall::NewCoord { program_hash, init } => {
                let target = self.state.push_process(
                    ProcessDefinition {
                        kind: ProcessKind::Coord,
                        program_hash,
                    },
                    None,
                );
                (
                    WitLedgerEffect::NewCoord {
                        program_hash,
                        val: init,
                        id: WitEffectOutput::Resolved(target),
                    },
                    HostImportOutcome::None,
                )
            }
            HostImportCall::InstallHandler { interface_id } => {
                self.handler_stack
                    .entry(interface_id)
                    .or_default()
                    .push(caller);
                (
                    WitLedgerEffect::InstallHandler { interface_id },
                    HostImportOutcome::None,
                )
            }
            HostImportCall::UninstallHandler { interface_id } => {
                let stack = self.handler_stack.entry(interface_id).or_default();
                let _ = stack.pop();
                (
                    WitLedgerEffect::UninstallHandler { interface_id },
                    HostImportOutcome::None,
                )
            }
            HostImportCall::CallEffectHandler {
                interface_id,
                payload,
                function_id: _,
            } => (
                // TODO(interleaving-proof): preserve `function_id` in the witness/effect schema.
                WitLedgerEffect::CallEffectHandler {
                    interface_id,
                    val: payload,
                    ret: WitEffectOutput::Resolved(Ref(0)),
                },
                HostImportOutcome::None,
            ),
        };
        self.traces.entry(caller).or_default().push(effect.clone());
        self.effect_log.push((caller, effect));
        Ok(outcome)
    }

    pub fn default_entry_function() -> u32 {
        FUNCTION_ID_STEP
    }
}
