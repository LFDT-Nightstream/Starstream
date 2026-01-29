use crate::F;
use crate::coroutine_args_gadget::coroutine_args_ops;
use crate::memory::{IVCMemory, IVCMemoryAllocated};
use crate::memory_tags::MemoryTag;
use crate::opcode_dsl::{OpcodeSynthDsl, OpcodeTraceDsl};
use crate::optional::{OptionalF, OptionalFpVar};
use crate::switchboard::{MemSwitchboardBool, MemSwitchboardWires};
use ark_ff::{AdditiveGroup, Field};
use ark_r1cs_std::alloc::AllocVar;
use ark_r1cs_std::fields::{FieldVar, fp::FpVar};
use ark_r1cs_std::prelude::Boolean;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};

#[derive(Clone, Debug)]
pub struct ProgramState {
    pub expected_input: OptionalF<F>,
    pub expected_resumer: OptionalF<F>,
    pub activation: F,
    pub init: F,
    pub counters: F,
    pub initialized: bool,
    pub finalized: bool,
    pub did_burn: bool,
    pub ownership: OptionalF<F>, // encoded optional process id
}

/// IVC wires (state between steps)
#[derive(Clone)]
pub struct ProgramStateWires {
    pub expected_input: OptionalFpVar<F>,
    pub expected_resumer: OptionalFpVar<F>,
    pub activation: FpVar<F>,
    pub init: FpVar<F>,
    pub counters: FpVar<F>,
    pub initialized: Boolean<F>,
    pub finalized: Boolean<F>,
    pub did_burn: Boolean<F>,
    pub ownership: OptionalFpVar<F>, // encoded optional process id
}

struct RawProgramState<V> {
    expected_input: V,
    expected_resumer: V,
    activation: V,
    init: V,
    counters: V,
    initialized: V,
    finalized: V,
    did_burn: V,
    ownership: V,
}

fn program_state_read_ops<D: crate::opcode_dsl::OpcodeDsl>(
    dsl: &mut D,
    switches: &crate::switchboard::MemSwitchboard<D::Bool>,
    addr: &D::Val,
) -> Result<RawProgramState<D::Val>, D::Error> {
    let expected_input = dsl.read(&switches.expected_input, MemoryTag::ExpectedInput, addr)?;
    let expected_resumer =
        dsl.read(&switches.expected_resumer, MemoryTag::ExpectedResumer, addr)?;
    let (activation, init) = coroutine_args_ops(dsl, &switches.activation, &switches.init, addr)?;
    let counters = dsl.read(&switches.counters, MemoryTag::Counters, addr)?;
    let initialized = dsl.read(&switches.initialized, MemoryTag::Initialized, addr)?;
    let finalized = dsl.read(&switches.finalized, MemoryTag::Finalized, addr)?;
    let did_burn = dsl.read(&switches.did_burn, MemoryTag::DidBurn, addr)?;
    let ownership = dsl.read(&switches.ownership, MemoryTag::Ownership, addr)?;

    Ok(RawProgramState {
        expected_input,
        expected_resumer,
        activation,
        init,
        counters,
        initialized,
        finalized,
        did_burn,
        ownership,
    })
}

fn program_state_write_ops<D: crate::opcode_dsl::OpcodeDsl>(
    dsl: &mut D,
    switches: &crate::switchboard::MemSwitchboard<D::Bool>,
    addr: &D::Val,
    state: &RawProgramState<D::Val>,
) -> Result<(), D::Error> {
    dsl.write(
        &switches.expected_input,
        MemoryTag::ExpectedInput,
        addr,
        &state.expected_input,
    )?;
    dsl.write(
        &switches.expected_resumer,
        MemoryTag::ExpectedResumer,
        addr,
        &state.expected_resumer,
    )?;
    dsl.write(
        &switches.activation,
        MemoryTag::Activation,
        addr,
        &state.activation,
    )?;
    dsl.write(&switches.init, MemoryTag::Init, addr, &state.init)?;
    dsl.write(
        &switches.counters,
        MemoryTag::Counters,
        addr,
        &state.counters,
    )?;
    dsl.write(
        &switches.initialized,
        MemoryTag::Initialized,
        addr,
        &state.initialized,
    )?;
    dsl.write(
        &switches.finalized,
        MemoryTag::Finalized,
        addr,
        &state.finalized,
    )?;
    dsl.write(
        &switches.did_burn,
        MemoryTag::DidBurn,
        addr,
        &state.did_burn,
    )?;
    dsl.write(
        &switches.ownership,
        MemoryTag::Ownership,
        addr,
        &state.ownership,
    )?;
    Ok(())
}

fn raw_from_state(state: &ProgramState) -> RawProgramState<F> {
    RawProgramState {
        expected_input: state.expected_input.encoded(),
        expected_resumer: state.expected_resumer.encoded(),
        activation: state.activation,
        init: state.init,
        counters: state.counters,
        initialized: F::from(state.initialized),
        finalized: F::from(state.finalized),
        did_burn: F::from(state.did_burn),
        ownership: state.ownership.encoded(),
    }
}

fn raw_from_wires(state: &ProgramStateWires) -> RawProgramState<FpVar<F>> {
    RawProgramState {
        expected_input: state.expected_input.encoded(),
        expected_resumer: state.expected_resumer.encoded(),
        activation: state.activation.clone(),
        init: state.init.clone(),
        counters: state.counters.clone(),
        initialized: state.initialized.clone().into(),
        finalized: state.finalized.clone().into(),
        did_burn: state.did_burn.clone().into(),
        ownership: state.ownership.encoded(),
    }
}

impl ProgramStateWires {
    pub fn from_write_values(
        cs: ConstraintSystemRef<F>,
        write_values: &ProgramState,
    ) -> Result<ProgramStateWires, SynthesisError> {
        Ok(ProgramStateWires {
            expected_input: OptionalFpVar::new(FpVar::new_witness(cs.clone(), || {
                Ok(write_values.expected_input.encoded())
            })?),
            expected_resumer: OptionalFpVar::new(FpVar::new_witness(cs.clone(), || {
                Ok(write_values.expected_resumer.encoded())
            })?),
            activation: FpVar::new_witness(cs.clone(), || Ok(write_values.activation))?,
            init: FpVar::new_witness(cs.clone(), || Ok(write_values.init))?,
            counters: FpVar::new_witness(cs.clone(), || Ok(write_values.counters))?,
            initialized: Boolean::new_witness(cs.clone(), || Ok(write_values.initialized))?,
            finalized: Boolean::new_witness(cs.clone(), || Ok(write_values.finalized))?,
            did_burn: Boolean::new_witness(cs.clone(), || Ok(write_values.did_burn))?,
            ownership: OptionalFpVar::new(FpVar::new_witness(cs.clone(), || {
                Ok(write_values.ownership.encoded())
            })?),
        })
    }
}

// Out-of-circuit write version.
pub fn trace_program_state_writes<M: IVCMemory<F>>(
    mem: &mut M,
    pid: u64,
    state: &ProgramState,
    switches: &MemSwitchboardBool,
) {
    let raw = raw_from_state(state);
    let mut dsl = OpcodeTraceDsl { mb: mem };
    let addr = F::from(pid);
    program_state_write_ops(&mut dsl, switches, &addr, &raw).expect("trace program state writes");
}

// In-circuit write version.
pub fn program_state_write_wires<M: IVCMemoryAllocated<F>>(
    rm: &mut M,
    cs: &ConstraintSystemRef<F>,
    address: FpVar<F>,
    state: &ProgramStateWires,
    switches: &MemSwitchboardWires,
) -> Result<(), SynthesisError> {
    let raw = raw_from_wires(state);
    let mut dsl = OpcodeSynthDsl { cs: cs.clone(), rm };
    program_state_write_ops(&mut dsl, switches, &address, &raw)?;
    Ok(())
}

// Out-of-circuit read version.
pub fn trace_program_state_reads<M: IVCMemory<F>>(
    mem: &mut M,
    pid: u64,
    switches: &MemSwitchboardBool,
) -> ProgramState {
    let mut dsl = OpcodeTraceDsl { mb: mem };
    let addr = F::from(pid);
    let raw = program_state_read_ops(&mut dsl, switches, &addr).expect("trace program state");

    ProgramState {
        expected_input: OptionalF::from_encoded(raw.expected_input),
        expected_resumer: OptionalF::from_encoded(raw.expected_resumer),
        activation: raw.activation,
        init: raw.init,
        counters: raw.counters,
        initialized: raw.initialized == F::ONE,
        finalized: raw.finalized == F::ONE,
        did_burn: raw.did_burn == F::ONE,
        ownership: OptionalF::from_encoded(raw.ownership),
    }
}

pub fn program_state_read_wires<M: IVCMemoryAllocated<F>>(
    rm: &mut M,
    cs: &ConstraintSystemRef<F>,
    address: FpVar<F>,
    switches: &MemSwitchboardWires,
) -> Result<ProgramStateWires, SynthesisError> {
    let mut dsl = OpcodeSynthDsl { cs: cs.clone(), rm };
    let raw = program_state_read_ops(&mut dsl, switches, &address)?;

    Ok(ProgramStateWires {
        expected_input: OptionalFpVar::new(raw.expected_input),
        expected_resumer: OptionalFpVar::new(raw.expected_resumer),
        activation: raw.activation,
        init: raw.init,
        counters: raw.counters,
        initialized: raw.initialized.is_one()?,
        finalized: raw.finalized.is_one()?,
        did_burn: raw.did_burn.is_one()?,
        ownership: OptionalFpVar::new(raw.ownership),
    })
}

impl ProgramState {
    pub fn dummy() -> Self {
        Self {
            finalized: false,
            expected_input: OptionalF::none(),
            expected_resumer: OptionalF::none(),
            activation: F::ZERO,
            init: F::ZERO,
            counters: F::ZERO,
            initialized: false,
            did_burn: false,
            ownership: OptionalF::none(),
        }
    }

    pub fn debug_print(&self) {
        tracing::debug!("expected_input={}", self.expected_input.encoded());
        tracing::debug!("expected_resumer={}", self.expected_resumer.encoded());
        tracing::debug!("activation={}", self.activation);
        tracing::debug!("init={}", self.init);
        tracing::debug!("counters={}", self.counters);
        tracing::debug!("finalized={}", self.finalized);
        tracing::debug!("did_burn={}", self.did_burn);
        tracing::debug!("ownership={}", self.ownership.encoded());
    }
}
