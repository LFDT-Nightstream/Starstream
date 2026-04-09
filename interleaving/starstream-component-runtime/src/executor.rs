use crate::abi::HostImportCall;
use crate::state::{ProcessDefinition, ProcessKind, ProgramHash, StarstreamState};
use starstream_interleaving_spec::{
    FunctionId, InterfaceId, ProcessId, Ref, Value, WitEffectOutput, WitLedgerEffect,
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
    #[error("missing pending resume edge to {target:?} from {caller:?}")]
    MissingPendingResume {
        caller: ProcessId,
        target: ProcessId,
    },
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
    state: StarstreamState<Resource>,
    handler_stack: HashMap<InterfaceId, Vec<ProcessId>>,
    ref_building: HashMap<ProcessId, (Ref, u32)>,
    traces: HashMap<ProcessId, Vec<WitLedgerEffect>>,
    effect_log: Vec<(ProcessId, WitLedgerEffect)>,
}

impl<Resource> StarstreamExecutor<Resource>
where
    Resource: Copy + Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self {
            state: StarstreamState::default(),
            handler_stack: HashMap::new(),
            ref_building: HashMap::new(),
            traces: HashMap::new(),
            effect_log: Vec::new(),
        }
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
        let (effect, outcome) =
            match call {
                HostImportCall::NewRef { size_words } => {
                    let reff = self.state.refs.alloc_words(size_words);
                    if size_words > 0 {
                        self.ref_building.insert(caller, (reff, 0));
                    } else {
                        self.ref_building.remove(&caller);
                    }
                    (
                        WitLedgerEffect::NewRef {
                            size: size_words as usize,
                            ret: WitEffectOutput::Resolved(reff),
                        },
                        HostImportOutcome::Ref(reff),
                    )
                }
                HostImportCall::RefPush { lanes } => {
                    let (reff, offset_words) = self.ref_building.get(&caller).copied().ok_or(
                        ExecutorError::Unsupported("ref-push without active new-ref"),
                    )?;
                    if !self.state.refs.write_lanes(reff, offset_words, lanes) {
                        return Err(ExecutorError::UnknownRef);
                    }
                    let next_offset = offset_words + 1;
                    let size_words = self
                        .state
                        .refs
                        .ref_size_words(reff)
                        .ok_or(ExecutorError::UnknownRef)?;
                    if next_offset >= size_words {
                        self.ref_building.remove(&caller);
                    } else {
                        self.ref_building.insert(caller, (reff, next_offset));
                    }
                    (
                        WitLedgerEffect::RefPush { vals: lanes },
                        HostImportOutcome::None,
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
                            f_id: starstream_interleaving_spec::FunctionId::from(function_id),
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
                HostImportCall::Burn => (WitLedgerEffect::Burn {}, HostImportOutcome::None),
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
                    function_id,
                } => (
                    WitLedgerEffect::CallEffectHandler {
                        interface_id,
                        f_id: FunctionId::from(function_id),
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

    pub fn append_effect(&mut self, pid: ProcessId, effect: WitLedgerEffect) {
        self.traces.entry(pid).or_default().push(effect.clone());
        self.effect_log.push((pid, effect));
    }

    pub fn resolve_resume_output(
        &mut self,
        caller: ProcessId,
        target: ProcessId,
        ret: Ref,
    ) -> Result<(), ExecutorError> {
        let trace = self
            .traces
            .get_mut(&caller)
            .ok_or(ExecutorError::MissingPendingResume { caller, target })?;
        let Some(effect) = find_pending_resume_mut(trace.iter_mut().rev(), target) else {
            return Err(ExecutorError::MissingPendingResume { caller, target });
        };
        set_resume_output(effect, ret);

        if let Some((_, effect)) = self
            .effect_log
            .iter_mut()
            .rev()
            .find(|(pid, effect)| *pid == caller && is_pending_resume(effect, target))
        {
            set_resume_output(effect, ret);
        }

        Ok(())
    }
}

fn is_pending_resume(effect: &WitLedgerEffect, target: ProcessId) -> bool {
    matches!(
        effect,
        WitLedgerEffect::Resume {
            target: resume_target,
            ..
        } if *resume_target == target
    )
}

fn find_pending_resume_mut<'a, I>(
    mut effects: I,
    target: ProcessId,
) -> Option<&'a mut WitLedgerEffect>
where
    I: Iterator<Item = &'a mut WitLedgerEffect>,
{
    effects.find(|effect| is_pending_resume(effect, target))
}

fn set_resume_output(effect: &mut WitLedgerEffect, ret: Ref) {
    if let WitLedgerEffect::Resume { ret: out, .. } = effect {
        *out = WitEffectOutput::Resolved(ret);
    }
}
