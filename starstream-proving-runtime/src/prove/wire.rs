use minicbor::{Decode, Encode};
use neo_wasm::{
    WasmCountdownState, WasmEventAbsorbState, WasmFrameMoveState, WasmHostEventState,
    WasmOutputState, WasmStepState,
};
use nightstream::PrimeField64;

use super::*;

/// Public proof data. Program images and initial states are selected from the verifier's inputs.
#[derive(Encode, Decode)]
pub struct EncodedTransactionProof {
    #[n(0)]
    instances: Vec<EncodedInstance>,
    #[n(1)]
    methods: Vec<[u32; 8]>,
    #[cbor(n(2), with = "minicbor::bytes")]
    interleaving: Vec<u8>,
}

#[derive(Encode, Decode)]
struct EncodedInstance {
    #[n(0)]
    coroutine: u32,
    #[n(1)]
    profile: u8,
    #[n(2)]
    program: [u64; 4],
    #[n(3)]
    entry_fref: u32,
    #[n(4)]
    last: TerminalState,
    #[cbor(n(5), with = "minicbor::bytes")]
    proof: Vec<u8>,
}

impl TransactionProof {
    pub fn encode_proof(&self) -> wasmtime::Result<EncodedTransactionProof> {
        let instances = self
            .instances
            .iter()
            .map(|i| {
                ensure!(
                    i.last.is_terminal() && !i.last.trapped,
                    "expected successful terminal Wasm state"
                );
                Ok(EncodedInstance {
                    coroutine: i.coroutine,
                    profile: match i.size {
                        WasmSize::Micro => 0,
                        WasmSize::Small => 1,
                        WasmSize::Medium => 2,
                        WasmSize::Large => 3,
                    },
                    program: i.program.d_init().map(|x| x.as_canonical_u64()),
                    entry_fref: i.initial.host_callee_fref,
                    last: TerminalState::from(i.last),
                    proof: i.proof.to_bytes()?,
                })
            })
            .collect::<wasmtime::Result<_>>()?;
        Ok(EncodedTransactionProof {
            instances,
            methods: self.methods.iter().map(|m| m.0).collect(),
            interleaving: self.interleaving.to_bytes()?,
        })
    }
}

impl EncodedInstance {
    fn size(&self) -> wasmtime::Result<WasmSize> {
        match self.profile {
            0 => Ok(WasmSize::Micro),
            1 => Ok(WasmSize::Small),
            2 => Ok(WasmSize::Medium),
            3 => Ok(WasmSize::Large),
            _ => bail!("unknown Wasm circuit profile {}", self.profile),
        }
    }
}

impl EncodedTransactionProof {
    pub fn component_sizes(&self) -> ProofSizes {
        ProofSizes {
            wasm: self
                .instances
                .iter()
                .map(|instance| instance.proof.len())
                .collect(),
            interleaving: self.interleaving.len(),
        }
    }

    pub fn circuit_sizes(&self) -> wasmtime::Result<Vec<WasmSize>> {
        self.instances.iter().map(EncodedInstance::size).collect()
    }

    pub fn decode_proof(
        self,
        expected: &PreparedVerification,
    ) -> wasmtime::Result<TransactionProof> {
        let instances = self
            .instances
            .into_iter()
            .map(|i| {
                let size = i.size()?;
                let (program, initial) =
                    expected.resolve(i.coroutine, size, i.program, i.entry_fref)?;
                Ok(WasmInstanceProof {
                    coroutine: i.coroutine,
                    size,
                    program: program.clone(),
                    initial: *initial,
                    last: i.last.into(),
                    proof: nebula::FinalProof::from_bytes(&i.proof)?,
                })
            })
            .collect::<wasmtime::Result<_>>()?;
        Ok(TransactionProof {
            instances,
            methods: self.methods.into_iter().map(MethodHash).collect(),
            interleaving: CompressedInterleavingProof::from_bytes(&self.interleaving)?,
        })
    }
}

// Successful terminal states have fixed suspension, trap, and event-drain flags.
#[derive(Encode, Decode)]
struct TerminalState {
    #[n(0)]
    pc: u64,
    #[n(1)]
    sp: u64,
    #[n(2)]
    stack_frame_base: u64,
    #[n(3)]
    output: (bool, u32, u32),
    #[n(4)]
    call_stack_depth: u64,
    #[n(5)]
    memory_pages: Option<u32>,
    #[n(6)]
    max_memory_pages: Option<u32>,
    #[n(7)]
    locals_fbp: u64,
    #[n(8)]
    frame_move: (bool, u32, u64),
    #[n(9)]
    local_zero: (bool, u32),
    #[n(10)]
    host_callee_fref: u32,
    #[n(11)]
    comm_chain: [u64; 4],
    #[n(12)]
    outer_prefix: [u64; 4],
    #[n(13)]
    outer_chain: [u64; 4],
    #[n(14)]
    evbuf: [u64; 8],
    #[n(15)]
    turn_export_fref: u32,
    #[n(16)]
    event_index: u32,
    #[n(17)]
    args_base: u64,
}

impl From<WasmStepState> for TerminalState {
    fn from(s: WasmStepState) -> Self {
        Self {
            pc: s.pc,
            sp: s.sp,
            stack_frame_base: s.stack_frame_base,
            output: (s.output.enabled, s.output.value_lo, s.output.value_hi),
            call_stack_depth: s.call_stack_depth,
            memory_pages: s.memory_pages,
            max_memory_pages: s.max_memory_pages,
            locals_fbp: s.locals_fbp,
            frame_move: (
                s.frame_move.active,
                s.frame_move.remaining,
                s.frame_move.distance,
            ),
            local_zero: (s.local_zero.active, s.local_zero.remaining),
            host_callee_fref: s.host_callee_fref,
            comm_chain: s.comm_chain,
            outer_prefix: s.event_absorb.outer_prefix,
            outer_chain: s.event_absorb.outer_chain,
            evbuf: s.event_absorb.evbuf,
            turn_export_fref: s.host_events.turn_export_fref,
            event_index: s.host_events.event_index,
            args_base: s.host_events.args_base,
        }
    }
}

impl From<TerminalState> for WasmStepState {
    fn from(s: TerminalState) -> Self {
        Self {
            pc: s.pc,
            sp: s.sp,
            stack_frame_base: s.stack_frame_base,
            output: WasmOutputState {
                enabled: s.output.0,
                value_lo: s.output.1,
                value_hi: s.output.2,
            },
            call_stack_depth: s.call_stack_depth,
            memory_pages: s.memory_pages,
            max_memory_pages: s.max_memory_pages,
            locals_fbp: s.locals_fbp,
            activation_finished: true,
            trapped: false,
            frame_move: WasmFrameMoveState {
                active: s.frame_move.0,
                remaining: s.frame_move.1,
                distance: s.frame_move.2,
            },
            local_zero: WasmCountdownState {
                active: s.local_zero.0,
                remaining: s.local_zero.1,
            },
            host_callee_fref: s.host_callee_fref,
            comm_chain: s.comm_chain,
            event_absorb: WasmEventAbsorbState {
                outer_prefix: s.outer_prefix,
                outer_chain: s.outer_chain,
                evbuf: s.evbuf,
                object_active: false,
            },
            host_events: WasmHostEventState {
                turn_export_fref: s.turn_export_fref,
                event_index: s.event_index,
                args_base: s.args_base,
                ..WasmHostEventState::ZERO
            },
        }
    }
}
