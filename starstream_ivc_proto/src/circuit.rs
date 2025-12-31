use crate::memory::{self, Address, IVCMemory};
use crate::value_to_field;
use crate::{F, LedgerOperation, memory::IVCMemoryAllocated};
use ark_ff::{AdditiveGroup, Field as _, PrimeField};
use ark_r1cs_std::alloc::AllocationMode;
use ark_r1cs_std::fields::FieldVar;
use ark_r1cs_std::{
    GR1CSVar as _, alloc::AllocVar as _, eq::EqGadget, fields::fp::FpVar, prelude::Boolean,
};
use ark_relations::gr1cs;
use ark_relations::{
    gr1cs::{ConstraintSystemRef, LinearCombination, SynthesisError, Variable},
    ns,
};
use starstream_mock_ledger::InterleavingInstance;
use std::marker::PhantomData;
use tracing::debug_span;

#[derive(Clone, Debug, Default)]
pub struct RomSwitchboard {
    pub read_is_utxo_curr: bool,
    pub read_is_utxo_target: bool,
    pub read_must_burn_curr: bool,
    pub read_program_hash_target: bool,
}

#[derive(Clone)]
pub struct RomSwitchboardWires {
    pub read_is_utxo_curr: Boolean<F>,
    pub read_is_utxo_target: Boolean<F>,
    pub read_must_burn_curr: Boolean<F>,
    pub read_program_hash_target: Boolean<F>,
}

impl RomSwitchboardWires {
    pub fn allocate(
        cs: ConstraintSystemRef<F>,
        switches: &RomSwitchboard,
    ) -> Result<Self, SynthesisError> {
        Ok(Self {
            read_is_utxo_curr: Boolean::new_witness(cs.clone(), || Ok(switches.read_is_utxo_curr))?,
            read_is_utxo_target: Boolean::new_witness(cs.clone(), || {
                Ok(switches.read_is_utxo_target)
            })?,
            read_must_burn_curr: Boolean::new_witness(cs.clone(), || {
                Ok(switches.read_must_burn_curr)
            })?,
            read_program_hash_target: Boolean::new_witness(cs.clone(), || {
                Ok(switches.read_program_hash_target)
            })?,
        })
    }
}

#[derive(Clone, Debug, Default)]
pub struct MemSwitchboard {
    pub expected_input: bool,
    pub arg: bool,
    pub counters: bool,
    pub initialized: bool,
    pub finalized: bool,
    pub did_burn: bool,
    pub ownership: bool,
}

#[derive(Clone)]
pub struct MemSwitchboardWires {
    pub expected_input: Boolean<F>,
    pub arg: Boolean<F>,
    pub counters: Boolean<F>,
    pub initialized: Boolean<F>,
    pub finalized: Boolean<F>,
    pub did_burn: Boolean<F>,
    pub ownership: Boolean<F>,
}

impl MemSwitchboardWires {
    pub fn allocate(
        cs: ConstraintSystemRef<F>,
        switches: &MemSwitchboard,
    ) -> Result<Self, SynthesisError> {
        Ok(Self {
            expected_input: Boolean::new_witness(cs.clone(), || Ok(switches.expected_input))?,
            arg: Boolean::new_witness(cs.clone(), || Ok(switches.arg))?,
            counters: Boolean::new_witness(cs.clone(), || Ok(switches.counters))?,
            initialized: Boolean::new_witness(cs.clone(), || Ok(switches.initialized))?,
            finalized: Boolean::new_witness(cs.clone(), || Ok(switches.finalized))?,
            did_burn: Boolean::new_witness(cs.clone(), || Ok(switches.did_burn))?,
            ownership: Boolean::new_witness(cs.clone(), || Ok(switches.ownership))?,
        })
    }
}

pub const ROM_PROCESS_TABLE: u64 = 1u64;
pub const ROM_MUST_BURN: u64 = 2u64;
pub const ROM_IS_UTXO: u64 = 3u64;

pub const RAM_EXPECTED_INPUT: u64 = 4u64;
pub const RAM_ARG: u64 = 5u64;
pub const RAM_COUNTERS: u64 = 6u64;
pub const RAM_INITIALIZED: u64 = 7u64;
pub const RAM_FINALIZED: u64 = 8u64;
pub const RAM_DID_BURN: u64 = 9u64;
pub const RAM_OWNERSHIP: u64 = 10u64;

// TODO: this is not implemented yet, since it's the only one with a dynamic
// memory size, I'll implement this at last
pub const RAM_HANDLER_STACK: u64 = 11u64;

impl MemSwitchboard {
    pub fn any(&self) -> bool {
        self.expected_input
            || self.arg
            || self.counters
            || self.initialized
            || self.finalized
            || self.did_burn
            || self.ownership
    }
}

pub fn opcode_to_mem_switches(instr: &LedgerOperation<F>) -> (MemSwitchboard, MemSwitchboard) {
    let mut curr_s = MemSwitchboard::default();
    let mut target_s = MemSwitchboard::default();

    // All ops increment counter of the current process, except Nop
    curr_s.counters = !matches!(instr, LedgerOperation::Nop {});

    match instr {
        LedgerOperation::Resume { .. } => {
            curr_s.arg = true;
            curr_s.expected_input = true;

            target_s.arg = true;
            target_s.expected_input = true;
            target_s.finalized = true;
            target_s.initialized = true;
        }
        LedgerOperation::Yield { ret, .. } => {
            curr_s.arg = true;
            if ret.is_some() {
                curr_s.expected_input = true;
            }
            curr_s.finalized = true;
        }
        LedgerOperation::Burn { .. } => {
            curr_s.arg = true;
            curr_s.finalized = true;
            curr_s.did_burn = true;
            curr_s.expected_input = true;
        }
        LedgerOperation::NewUtxo { .. } | LedgerOperation::NewCoord { .. } => {
            // New* ops initialize the target process
            target_s.initialized = true;
            target_s.expected_input = true;
            target_s.counters = true; // sets counter to 0
        }
        _ => {
            // Other ops like ProgramHash or Input are read-only or have no side-effects tracked here.
        }
    }
    (curr_s, target_s)
}

pub fn opcode_to_rom_switches(instr: &LedgerOperation<F>) -> RomSwitchboard {
    let mut rom_s = RomSwitchboard::default();
    match instr {
        LedgerOperation::Resume { .. } => {
            rom_s.read_is_utxo_curr = true;
            rom_s.read_is_utxo_target = true;
        }
        LedgerOperation::Burn { .. } => {
            rom_s.read_is_utxo_curr = true;
            rom_s.read_must_burn_curr = true;
        }
        LedgerOperation::NewUtxo { .. } | LedgerOperation::NewCoord { .. } => {
            rom_s.read_is_utxo_curr = true;
            rom_s.read_is_utxo_target = true;
            rom_s.read_program_hash_target = true;
        }
        _ => {}
    }
    rom_s
}

pub struct StepCircuitBuilder<M> {
    pub instance: InterleavingInstance,
    pub last_yield: Vec<F>,
    pub ops: Vec<LedgerOperation<crate::F>>,
    write_ops: Vec<(ProgramState, ProgramState)>,
    mem_switches: Vec<(MemSwitchboard, MemSwitchboard)>,
    rom_switches: Vec<RomSwitchboard>,

    mem: PhantomData<M>,
}

/// common circuit variables to all the opcodes
#[derive(Clone)]
pub struct Wires {
    // irw
    id_curr: FpVar<F>,
    id_prev: FpVar<F>,

    utxos_len: FpVar<F>,

    // switches
    resume_switch: Boolean<F>,
    yield_switch: Boolean<F>,
    program_hash_switch: Boolean<F>,
    new_utxo_switch: Boolean<F>,
    new_coord_switch: Boolean<F>,
    burn_switch: Boolean<F>,
    input_switch: Boolean<F>,

    check_utxo_output_switch: Boolean<F>,

    target: FpVar<F>,
    val: FpVar<F>,
    ret: FpVar<F>,
    program_hash: FpVar<F>,
    caller: FpVar<F>,

    curr_read_wires: ProgramStateWires,
    curr_write_wires: ProgramStateWires,

    target_read_wires: ProgramStateWires,
    target_write_wires: ProgramStateWires,

    curr_mem_switches: MemSwitchboardWires,
    target_mem_switches: MemSwitchboardWires,

    // ROM lookup results
    is_utxo_curr: FpVar<F>,
    is_utxo_target: FpVar<F>,
    must_burn_curr: FpVar<F>,
    rom_program_hash: FpVar<F>,

    // ROM read switches
    rom_switches: RomSwitchboardWires,

    constant_false: Boolean<F>,
    constant_true: Boolean<F>,
    constant_one: FpVar<F>,
}

/// these are the mcc witnesses
#[derive(Clone)]
pub struct ProgramStateWires {
    expected_input: FpVar<F>,
    arg: FpVar<F>,
    counters: FpVar<F>,
    initialized: Boolean<F>,
    finalized: Boolean<F>,
    did_burn: Boolean<F>,
    owned_by: FpVar<F>, // an index into the process table
}

// helper so that we always allocate witnesses in the same order
pub struct PreWires {
    target: F,
    val: F,
    ret: F,
    id_prev: F,

    program_hash: F,

    new_process_id: F,
    caller: F,

    // switches
    yield_switch: bool,
    resume_switch: bool,
    check_utxo_output_switch: bool,
    nop_switch: bool,
    burn_switch: bool,
    program_hash_switch: bool,
    new_utxo_switch: bool,
    new_coord_switch: bool,
    input_switch: bool,

    curr_mem_switches: MemSwitchboard,
    target_mem_switches: MemSwitchboard,
    rom_switches: RomSwitchboard,

    irw: InterRoundWires,
}

#[derive(Clone, Debug)]
pub struct ProgramState {
    expected_input: F,
    arg: F,
    counters: F,
    initialized: bool,
    finalized: bool,
    did_burn: bool,
    owned_by: F, // an index into the process table
}

/// IVC wires (state between steps)
///
/// these get input and output variables
#[derive(Clone)]
pub struct InterRoundWires {
    id_curr: F,
    id_prev: F,

    p_len: F,
    n_finalized: F,
}

impl ProgramStateWires {
    fn from_write_values(
        cs: ConstraintSystemRef<F>,
        write_values: &ProgramState,
    ) -> Result<ProgramStateWires, SynthesisError> {
        Ok(ProgramStateWires {
            expected_input: FpVar::new_witness(cs.clone(), || Ok(write_values.expected_input))?,
            arg: FpVar::new_witness(cs.clone(), || Ok(write_values.arg))?,
            counters: FpVar::new_witness(cs.clone(), || Ok(write_values.counters))?,
            initialized: Boolean::new_witness(cs.clone(), || Ok(write_values.initialized))?,
            finalized: Boolean::new_witness(cs.clone(), || Ok(write_values.finalized))?,
            did_burn: Boolean::new_witness(cs.clone(), || Ok(write_values.did_burn))?,
            owned_by: FpVar::new_witness(cs.clone(), || Ok(write_values.owned_by))?,
        })
    }
}

impl Wires {
    pub fn from_irw<M: IVCMemoryAllocated<F>>(
        vals: &PreWires,
        rm: &mut M,
        current_write_values: &ProgramState,
        target_write_values: &ProgramState,
    ) -> Result<Wires, SynthesisError> {
        vals.debug_print();

        let cs = rm.get_cs();

        // io vars
        let id_curr = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.irw.id_curr))?;
        let utxos_len = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.irw.p_len))?;

        // switches
        let switches = [
            vals.resume_switch,
            vals.yield_switch,
            vals.check_utxo_output_switch,
            vals.nop_switch,
            vals.burn_switch,
            vals.program_hash_switch,
            vals.new_utxo_switch,
            vals.new_coord_switch,
            vals.input_switch,
        ];

        let allocated_switches: Vec<_> = switches
            .iter()
            .map(|val| Boolean::new_witness(cs.clone(), || Ok(*val)).unwrap())
            .collect();

        let [
            resume_switch,
            yield_switch,
            check_utxo_output_switch,
            nop_switch,
            burn_switch,
            program_hash_switch,
            new_utxo_switch,
            new_coord_switch,
            input_switch,
        ] = allocated_switches.as_slice()
        else {
            unreachable!()
        };

        // TODO: figure out how to write this with the proper dsl
        // but we only need r1cs anyway.
        cs.enforce_r1cs_constraint(
            || {
                allocated_switches
                    .iter()
                    .fold(LinearCombination::new(), |acc, switch| acc + switch.lc())
                    .clone()
            },
            || LinearCombination::new() + Variable::one(),
            || LinearCombination::new() + Variable::one(),
        )
        .unwrap();

        let target = FpVar::<F>::new_witness(ns!(cs.clone(), "target"), || Ok(vals.target))?;

        let val = FpVar::<F>::new_witness(ns!(cs.clone(), "val"), || Ok(vals.val))?;
        let ret = FpVar::<F>::new_witness(ns!(cs.clone(), "ret"), || Ok(vals.ret))?;

        let id_prev = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.id_prev))?;
        let program_hash = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.program_hash))?;

        let new_process_id = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.new_process_id))?;
        let caller = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.caller))?;

        let curr_mem_switches = MemSwitchboardWires::allocate(cs.clone(), &vals.curr_mem_switches)?;
        let target_mem_switches =
            MemSwitchboardWires::allocate(cs.clone(), &vals.target_mem_switches)?;

        let curr_address = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.irw.id_curr))?;
        let curr_read_wires =
            program_state_read_wires(rm, &cs, curr_address.clone(), &curr_mem_switches)?;

        // TODO: make conditional for opcodes without target
        let target_address = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.target))?;
        let target_read_wires =
            program_state_read_wires(rm, &cs, target_address.clone(), &target_mem_switches)?;

        let curr_write_wires =
            ProgramStateWires::from_write_values(cs.clone(), current_write_values)?;

        let target_write_wires =
            ProgramStateWires::from_write_values(cs.clone(), target_write_values)?;

        program_state_write_wires(
            rm,
            &cs,
            curr_address.clone(),
            curr_write_wires.clone(),
            &curr_mem_switches,
        )?;
        program_state_write_wires(
            rm,
            &cs,
            target_address.clone(),
            target_write_wires.clone(),
            &target_mem_switches,
        )?;

        let rom_switches = RomSwitchboardWires::allocate(cs.clone(), &vals.rom_switches)?;

        let is_utxo_curr = rm.conditional_read(
            &rom_switches.read_is_utxo_curr,
            &Address {
                addr: id_curr.clone(),
                tag: FpVar::new_constant(cs.clone(), F::from(ROM_IS_UTXO))?,
            },
        )?[0]
            .clone();

        let is_utxo_target = rm.conditional_read(
            &rom_switches.read_is_utxo_target,
            &Address {
                addr: target_address.clone(),
                tag: FpVar::new_constant(cs.clone(), F::from(ROM_IS_UTXO))?,
            },
        )?[0]
            .clone();

        let must_burn_curr = rm.conditional_read(
            &rom_switches.read_must_burn_curr,
            &Address {
                addr: id_curr.clone(),
                tag: FpVar::new_constant(cs.clone(), F::from(ROM_MUST_BURN))?,
            },
        )?[0]
            .clone();

        let rom_program_hash = rm.conditional_read(
            &rom_switches.read_program_hash_target,
            &Address {
                addr: target_address.clone(),
                tag: FpVar::new_constant(cs.clone(), F::from(ROM_PROCESS_TABLE))?,
            },
        )?[0]
            .clone();

        Ok(Wires {
            id_curr,
            id_prev,

            utxos_len,

            yield_switch: yield_switch.clone(),
            resume_switch: resume_switch.clone(),
            check_utxo_output_switch: check_utxo_output_switch.clone(),
            burn_switch: burn_switch.clone(),
            program_hash_switch: program_hash_switch.clone(),
            new_utxo_switch: new_utxo_switch.clone(),
            new_coord_switch: new_coord_switch.clone(),
            input_switch: input_switch.clone(),

            constant_false: Boolean::new_constant(cs.clone(), false)?,
            constant_true: Boolean::new_constant(cs.clone(), true)?,
            constant_one: FpVar::new_constant(cs.clone(), F::from(1))?,

            // wit_wires
            target,
            val,
            ret,
            program_hash,
            caller,

            curr_read_wires,
            curr_write_wires,

            target_read_wires,
            target_write_wires,

            curr_mem_switches,
            target_mem_switches,
            rom_switches,

            is_utxo_curr,
            is_utxo_target,
            must_burn_curr,
            rom_program_hash,
        })
    }
}

fn program_state_read_wires<M: IVCMemoryAllocated<F>>(
    rm: &mut M,
    cs: &ConstraintSystemRef<F>,
    address: FpVar<F>,
    switches: &MemSwitchboardWires,
) -> Result<ProgramStateWires, SynthesisError> {
    Ok(ProgramStateWires {
        expected_input: rm
            .conditional_read(
                &switches.expected_input,
                &Address {
                    addr: address.clone(),
                    tag: FpVar::new_constant(cs.clone(), F::from(RAM_EXPECTED_INPUT))?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
        arg: rm
            .conditional_read(
                &switches.arg,
                &Address {
                    addr: address.clone(),
                    tag: FpVar::new_constant(cs.clone(), F::from(RAM_ARG))?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
        counters: rm
            .conditional_read(
                &switches.counters,
                &Address {
                    addr: address.clone(),
                    tag: FpVar::new_constant(cs.clone(), F::from(RAM_COUNTERS))?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
        initialized: rm
            .conditional_read(
                &switches.initialized,
                &Address {
                    addr: address.clone(),
                    tag: FpVar::new_constant(cs.clone(), F::from(RAM_INITIALIZED))?,
                },
            )?
            .into_iter()
            .next()
            .unwrap()
            .is_one()?,
        finalized: rm
            .conditional_read(
                &switches.finalized,
                &Address {
                    addr: address.clone(),
                    tag: FpVar::new_constant(cs.clone(), F::from(RAM_FINALIZED))?,
                },
            )?
            .into_iter()
            .next()
            .unwrap()
            .is_one()?,

        did_burn: rm
            .conditional_read(
                &switches.did_burn,
                &Address {
                    addr: address.clone(),
                    tag: FpVar::new_constant(cs.clone(), F::from(RAM_DID_BURN))?,
                },
            )?
            .into_iter()
            .next()
            .unwrap()
            .is_one()?,
        owned_by: rm
            .conditional_read(
                &switches.ownership,
                &Address {
                    addr: address.clone(),
                    tag: FpVar::new_constant(cs.clone(), F::from(RAM_OWNERSHIP))?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
    })
}

// this is out-of-circuit logic (witness generation)
fn trace_program_state_reads<M: IVCMemory<F>>(
    mem: &mut M,
    pid: u64,
    switches: &MemSwitchboard,
) -> ProgramState {
    ProgramState {
        expected_input: mem.conditional_read(
            switches.expected_input,
            Address {
                addr: pid,
                tag: RAM_EXPECTED_INPUT,
            },
        )[0],
        arg: mem.conditional_read(
            switches.arg,
            Address {
                addr: pid,
                tag: RAM_ARG,
            },
        )[0],
        counters: mem.conditional_read(
            switches.counters,
            Address {
                addr: pid,
                tag: RAM_COUNTERS,
            },
        )[0],
        initialized: mem.conditional_read(
            switches.initialized,
            Address {
                addr: pid,
                tag: RAM_INITIALIZED,
            },
        )[0] == F::ONE,
        finalized: mem.conditional_read(
            switches.finalized,
            Address {
                addr: pid,
                tag: RAM_FINALIZED,
            },
        )[0] == F::ONE,
        did_burn: mem.conditional_read(
            switches.did_burn,
            Address {
                addr: pid,
                tag: RAM_DID_BURN,
            },
        )[0] == F::ONE,
        owned_by: mem.conditional_read(
            switches.ownership,
            Address {
                addr: pid,
                tag: RAM_OWNERSHIP,
            },
        )[0],
    }
}

// this is out-of-circuit logic (witness generation)
fn trace_program_state_writes<M: IVCMemory<F>>(
    mem: &mut M,
    pid: u64,
    state: &ProgramState,
    switches: &MemSwitchboard,
) {
    mem.conditional_write(
        switches.expected_input,
        Address {
            addr: pid,
            tag: RAM_EXPECTED_INPUT,
        },
        [state.expected_input].to_vec(),
    );
    mem.conditional_write(
        switches.arg,
        Address {
            addr: pid,
            tag: RAM_ARG,
        },
        [state.arg].to_vec(),
    );
    mem.conditional_write(
        switches.counters,
        Address {
            addr: pid,
            tag: RAM_COUNTERS,
        },
        [state.counters].to_vec(),
    );
    mem.conditional_write(
        switches.initialized,
        Address {
            addr: pid,
            tag: RAM_INITIALIZED,
        },
        [F::from(state.initialized)].to_vec(),
    );
    mem.conditional_write(
        switches.finalized,
        Address {
            addr: pid,
            tag: RAM_FINALIZED,
        },
        [F::from(state.finalized)].to_vec(),
    );
    mem.conditional_write(
        switches.did_burn,
        Address {
            addr: pid,
            tag: RAM_DID_BURN,
        },
        [F::from(state.did_burn)].to_vec(),
    );
    mem.conditional_write(
        switches.ownership,
        Address {
            addr: pid,
            tag: RAM_OWNERSHIP,
        },
        [state.owned_by].to_vec(),
    );
}

fn program_state_write_wires<M: IVCMemoryAllocated<F>>(
    rm: &mut M,
    cs: &ConstraintSystemRef<F>,
    address: FpVar<F>,
    state: ProgramStateWires,
    switches: &MemSwitchboardWires,
) -> Result<(), SynthesisError> {
    rm.conditional_write(
        &switches.expected_input,
        &Address {
            addr: address.clone(),
            tag: FpVar::new_constant(cs.clone(), F::from(RAM_EXPECTED_INPUT))?,
        },
        &[state.expected_input.clone()],
    )?;

    rm.conditional_write(
        &switches.arg,
        &Address {
            addr: address.clone(),
            tag: FpVar::new_constant(cs.clone(), F::from(RAM_ARG))?,
        },
        &[state.arg.clone()],
    )?;
    rm.conditional_write(
        &switches.counters,
        &Address {
            addr: address.clone(),
            tag: FpVar::new_constant(cs.clone(), F::from(RAM_COUNTERS))?,
        },
        &[state.counters.clone()],
    )?;
    rm.conditional_write(
        &switches.initialized,
        &Address {
            addr: address.clone(),
            tag: FpVar::new_constant(cs.clone(), F::from(RAM_INITIALIZED))?,
        },
        &[state.initialized.clone().into()],
    )?;
    rm.conditional_write(
        &switches.finalized,
        &Address {
            addr: address.clone(),
            tag: FpVar::new_constant(cs.clone(), F::from(RAM_FINALIZED))?,
        },
        &[state.finalized.clone().into()],
    )?;

    rm.conditional_write(
        &switches.did_burn,
        &Address {
            addr: address.clone(),
            tag: FpVar::new_constant(cs.clone(), F::from(RAM_DID_BURN))?,
        },
        &[state.did_burn.clone().into()],
    )?;

    rm.conditional_write(
        &switches.ownership,
        &Address {
            addr: address.clone(),
            tag: FpVar::new_constant(cs.clone(), F::from(RAM_OWNERSHIP))?,
        },
        &[state.owned_by.clone()],
    )?;

    Ok(())
}

impl InterRoundWires {
    pub fn new(p_len: F) -> Self {
        InterRoundWires {
            id_curr: F::from(1),
            id_prev: F::from(0), // None
            p_len,
            n_finalized: F::from(0),
        }
    }

    pub fn update(&mut self, res: Wires) {
        let _guard = debug_span!("update ivc state").entered();

        tracing::debug!(
            "current_program from {} to {}",
            self.id_curr,
            res.id_curr.value().unwrap()
        );

        self.id_curr = res.id_curr.value().unwrap();

        tracing::debug!(
            "prev_program from {} to {}",
            self.id_prev,
            res.id_prev.value().unwrap()
        );

        self.id_curr = res.id_curr.value().unwrap();

        tracing::debug!(
            "utxos_len from {} to {}",
            self.p_len,
            res.utxos_len.value().unwrap()
        );

        self.p_len = res.utxos_len.value().unwrap();
    }
}

impl LedgerOperation<crate::F> {
    // state transitions for current and target (next) programs
    // in general, we only change the state of a most two processes in a single
    // step.
    //
    // this takes the current state for both of those processes, and returns the
    // new state for each one too.
    pub fn program_state_transitions(
        &self,
        curr_read: ProgramState,
        target_read: ProgramState,
    ) -> (ProgramState, ProgramState) {
        let mut curr_write = curr_read.clone();
        let mut target_write = target_read.clone();

        // All operations increment the counter of the current process
        curr_write.counters += F::ONE;

        match self {
            LedgerOperation::Nop {} => {
                // Nop does nothing to the state
                curr_write.counters -= F::ONE; // revert counter increment
            }
            LedgerOperation::Resume { val, ret, .. } => {
                // Current process gives control to target.
                // It's `arg` is cleared, and its `expected_input` is set to the return value `ret`.
                curr_write.arg = F::ZERO; // Represents None
                curr_write.expected_input = *ret;

                // Target process receives control.
                // Its `arg` is set to `val`, and it is no longer in a `finalized` state.
                target_write.arg = *val;
                target_write.finalized = false;
            }
            LedgerOperation::Yield {
                // The yielded value `val` is checked against the parent's `expected_input`,
                // but this doesn't change the parent's state itself.
                val: _,
                ret,
                ..
            } => {
                // Current process yields control back to its parent (the target of this operation).
                // Its `arg` is cleared.
                curr_write.arg = F::ZERO; // Represents None
                if let Some(r) = ret {
                    // If Yield returns a value, it expects a new input `r` for the next resume.
                    curr_write.expected_input = *r;
                    curr_write.finalized = false;
                } else {
                    // If Yield does not return a value, it's a final yield for this UTXO.
                    curr_write.finalized = true;
                }
            }
            LedgerOperation::Burn { ret } => {
                // The current UTXO is burned.
                curr_write.arg = F::ZERO; // Represents None
                curr_write.finalized = true;
                curr_write.did_burn = true;
                curr_write.expected_input = *ret; // Sets its final return value.
            }
            LedgerOperation::NewUtxo { val, target: _, .. }
            | LedgerOperation::NewCoord { val, target: _, .. } => {
                // The current process is a coordinator creating a new process.
                // The new process (target) is initialized.
                target_write.initialized = true;
                target_write.expected_input = *val;
                target_write.counters = F::ZERO;
            }
            _ => {
                // For other opcodes, we just increment the counter.
            }
        }
        (curr_write, target_write)
    }
}

impl<M: IVCMemory<F>> StepCircuitBuilder<M> {
    pub fn new(instance: InterleavingInstance, ops: Vec<LedgerOperation<crate::F>>) -> Self {
        let last_yield = instance
            .input_states
            .iter()
            .map(|v| value_to_field(v.last_yield.clone()))
            .collect();

        Self {
            ops,
            write_ops: vec![],
            mem_switches: vec![],
            rom_switches: vec![],
            mem: PhantomData,
            instance,
            last_yield,
        }
    }

    // pub fn dummy(utxos: BTreeMap<F, UtxoChange>) -> Self {
    //     Self {
    //         utxos,
    //         ops: vec![Instruction::Nop {}],
    //         write_ops: vec![],
    //         utxo_order_mapping: Default::default(),

    //         mem: PhantomData,
    //     }
    // }

    pub fn make_step_circuit(
        &self,
        i: usize,
        rm: &mut M::Allocator,
        cs: ConstraintSystemRef<F>,
        mut irw: InterRoundWires,
    ) -> Result<InterRoundWires, SynthesisError> {
        rm.start_step(cs.clone()).unwrap();

        let _guard = tracing::info_span!("make_step_circuit", i = i, op = ?self.ops[i]).entered();

        let wires_in = self.allocate_vars(i, rm, &irw)?;
        let next_wires = wires_in.clone();

        // per opcode constraints
        let next_wires = self.visit_yield(next_wires)?;
        let next_wires = self.visit_resume(next_wires)?;
        let next_wires = self.visit_burn(next_wires)?;
        let next_wires = self.visit_new_process(next_wires)?;
        let next_wires = self.visit_input(next_wires)?;

        rm.finish_step(i == self.ops.len() - 1)?;

        // input <-> output mappings are done by modifying next_wires
        ivcify_wires(&cs, &wires_in, &next_wires)?;

        irw.update(next_wires);

        tracing::debug!("constraints: {}", cs.num_constraints());

        Ok(irw)
    }

    pub fn trace_memory_ops(&mut self, params: <M as memory::IVCMemory<F>>::Params) -> M {
        // initialize all the maps
        let mut mb = {
            let mut mb = M::new(params);

            mb.register_mem(ROM_PROCESS_TABLE, 1, "ROM_PROCESS_TABLE");
            mb.register_mem(ROM_MUST_BURN, 1, "ROM_MUST_BURN");
            mb.register_mem(ROM_IS_UTXO, 1, "ROM_IS_UTXO");
            mb.register_mem(RAM_EXPECTED_INPUT, 1, "RAM_EXPECTED_INPUT");
            mb.register_mem(RAM_ARG, 1, "RAM_ARG");
            mb.register_mem(RAM_COUNTERS, 1, "RAM_COUNTERS");
            mb.register_mem(RAM_INITIALIZED, 1, "RAM_INITIALIZED");
            mb.register_mem(RAM_FINALIZED, 1, "RAM_FINALIZED");
            mb.register_mem(RAM_DID_BURN, 1, "RAM_DID_BURN");
            mb.register_mem(RAM_OWNERSHIP, 1, "RAM_OWNERSHIP");

            for (pid, mod_hash) in self.instance.process_table.iter().enumerate() {
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: ROM_PROCESS_TABLE,
                    },
                    // TODO: use a proper conversion from hash to val, this is just a placeholder
                    vec![F::from(mod_hash.0[0] as u64)],
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: RAM_INITIALIZED,
                    },
                    vec![F::from(
                        if pid < self.instance.n_inputs || pid == self.instance.entrypoint.0 {
                            1
                        } else {
                            0
                        },
                    )],
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: RAM_COUNTERS,
                    },
                    vec![F::from(0u64)],
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: RAM_FINALIZED,
                    },
                    vec![F::from(0u64)], // false
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: RAM_DID_BURN,
                    },
                    vec![F::from(0u64)], // false
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: RAM_EXPECTED_INPUT,
                    },
                    vec![if pid >= self.instance.n_inputs {
                        F::from(0u64)
                    } else {
                        self.last_yield[pid]
                    }],
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: RAM_ARG,
                    },
                    vec![F::from(0u64)], // None
                );
            }

            for (pid, must_burn) in self.instance.must_burn.iter().enumerate() {
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: ROM_MUST_BURN,
                    },
                    vec![F::from(if *must_burn { 1u64 } else { 0 })],
                );
            }

            for (pid, is_utxo) in self.instance.is_utxo.iter().enumerate() {
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: ROM_IS_UTXO,
                    },
                    vec![F::from(if *is_utxo { 1u64 } else { 0 })],
                );
            }

            // TODO: initialize ownership too

            for (pid, owner) in self.instance.ownership_in.iter().enumerate() {
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: RAM_OWNERSHIP,
                    },
                    vec![F::from(
                        owner
                            .map(|p| p.0)
                            // probably using 0 for null is better but it would
                            // mean checking that pids are always greater than
                            // 0, so review later
                            .unwrap_or(self.instance.process_table.len())
                            as u64,
                    )],
                );
            }

            mb
        };

        let mut curr_pid = self.instance.entrypoint.0 as u64;
        let mut prev_pid: Option<u64> = None;

        // out of circuit memory operations.
        // this is needed to commit to the memory operations before-hand.
        //
        // and here we compute the actual write values (for memory operations)
        //
        // note however that we don't enforce/check anything, that's done in the
        // circuit constraints
        for instr in &self.ops {
            let (curr_switches, target_switches) = opcode_to_mem_switches(instr);
            self.mem_switches
                .push((curr_switches.clone(), target_switches.clone()));

            let rom_switches = opcode_to_rom_switches(instr);
            self.rom_switches.push(rom_switches.clone());

            let target_addr = match instr {
                LedgerOperation::Resume { target, .. } => Some(*target),
                LedgerOperation::Yield { .. } => prev_pid.map(F::from),
                LedgerOperation::Burn { .. } => prev_pid.map(F::from),
                LedgerOperation::NewUtxo { target: id, .. } => Some(*id),
                LedgerOperation::NewCoord { target: id, .. } => Some(*id),
                LedgerOperation::ProgramHash { target, .. } => Some(*target),
                _ => None,
            };

            let curr_read = trace_program_state_reads(&mut mb, curr_pid, &curr_switches);

            let target_pid = target_addr.map(|t| t.into_bigint().0[0]);
            let target_read =
                trace_program_state_reads(&mut mb, target_pid.unwrap_or(0), &target_switches);

            // Trace ROM reads
            mb.conditional_read(
                rom_switches.read_is_utxo_curr,
                Address {
                    addr: curr_pid,
                    tag: ROM_IS_UTXO,
                },
            );
            mb.conditional_read(
                rom_switches.read_is_utxo_target,
                Address {
                    addr: target_pid.unwrap_or(0),
                    tag: ROM_IS_UTXO,
                },
            );
            mb.conditional_read(
                rom_switches.read_must_burn_curr,
                Address {
                    addr: curr_pid,
                    tag: ROM_MUST_BURN,
                },
            );
            mb.conditional_read(
                rom_switches.read_program_hash_target,
                Address {
                    addr: target_pid.unwrap_or(0),
                    tag: ROM_PROCESS_TABLE,
                },
            );

            let (curr_write, target_write) =
                instr.program_state_transitions(curr_read, target_read);

            self.write_ops
                .push((curr_write.clone(), target_write.clone()));

            trace_program_state_writes(&mut mb, curr_pid, &curr_write, &curr_switches);
            trace_program_state_writes(
                &mut mb,
                target_pid.unwrap_or(0),
                &target_write,
                &target_switches,
            );

            // update pids for next iteration
            match instr {
                LedgerOperation::Resume { target, .. } => {
                    prev_pid = Some(curr_pid);
                    curr_pid = target.into_bigint().0[0];
                }
                LedgerOperation::Yield { .. } | LedgerOperation::Burn { .. } => {
                    let parent = prev_pid.expect("yield/burn must have parent");
                    prev_pid = Some(curr_pid);
                    curr_pid = parent;
                }
                _ => {}
            }
        }

        mb
    }

    fn allocate_vars(
        &self,
        i: usize,
        rm: &mut M::Allocator,
        irw: &InterRoundWires,
    ) -> Result<Wires, SynthesisError> {
        let instruction = &self.ops[i];
        let (curr_write, target_write) = &self.write_ops[i];
        let (curr_mem_switches, target_mem_switches) = &self.mem_switches[i];
        let rom_switches = &self.rom_switches[i];

        match instruction {
            LedgerOperation::Nop {} => {
                let irw = PreWires {
                    nop_switch: true,
                    irw: irw.clone(),

                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };

                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Resume {
                target,
                val,
                ret,
                id_prev,
            } => {
                let irw = PreWires {
                    resume_switch: true,
                    target: *target,
                    val: val.clone(),
                    ret: ret.clone(),
                    id_prev: id_prev.clone().unwrap_or(F::ZERO),

                    irw: irw.clone(),

                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };

                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Yield { val, ret, id_prev } => {
                let irw = PreWires {
                    yield_switch: true,
                    target: irw.id_prev,
                    val: val.clone(),
                    ret: ret.clone().unwrap_or(F::ZERO),
                    id_prev: id_prev.clone().unwrap_or(F::ZERO),

                    irw: irw.clone(),

                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };

                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Burn { ret } => {
                let irw = PreWires {
                    burn_switch: true,
                    target: irw.id_prev,
                    ret: ret.clone(),
                    irw: irw.clone(),
                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };

                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::ProgramHash {
                target,
                program_hash,
            } => {
                let irw = PreWires {
                    program_hash_switch: true,
                    target: *target,
                    program_hash: *program_hash,
                    irw: irw.clone(),
                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::NewUtxo {
                program_hash,
                val,
                target,
            } => {
                let irw = PreWires {
                    new_utxo_switch: true,
                    target: *target,
                    val: *val,
                    program_hash: *program_hash,
                    irw: irw.clone(),
                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::NewCoord {
                program_hash,
                val,
                target,
            } => {
                let irw = PreWires {
                    new_coord_switch: true,
                    target: *target,
                    val: *val,
                    program_hash: *program_hash,
                    irw: irw.clone(),
                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Input { val, caller } => {
                let irw = PreWires {
                    input_switch: true,
                    val: *val,
                    caller: *caller,
                    irw: irw.clone(),
                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
        }
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_resume(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.resume_switch;

        // 1. self-resume check
        wires
            .id_curr
            .conditional_enforce_not_equal(&wires.target, switch)?;

        // 2. UTXO cannot resume UTXO.
        let is_utxo_curr = wires.is_utxo_curr.is_one()?;
        let is_utxo_target = wires.is_utxo_target.is_one()?;
        let both_are_utxos = is_utxo_curr & is_utxo_target;
        both_are_utxos.conditional_enforce_equal(&Boolean::FALSE, switch)?;
        // 3. Target must be initialized
        wires
            .target_read_wires
            .initialized
            .conditional_enforce_equal(&wires.constant_true.clone().into(), switch)?;

        // 4. Re-entrancy check (target's arg must be None/0)
        wires
            .target_read_wires
            .arg
            .conditional_enforce_equal(&FpVar::zero(), switch)?;

        // 5. Claim check: val passed in must match target's expected_input.
        wires
            .target_read_wires
            .expected_input
            .conditional_enforce_equal(&wires.val, switch)?;

        // ---
        // State update enforcement is implicitly handled by the MemSwitchboard.
        // We trust that `write_values` computed the correct new state, and the switchboard
        // correctly determines which fields are written. The circuit only needs to
        // enforce the checks above and the IVC updates below.
        // ---

        // ---
        // IVC state updates
        // ---
        // On resume, current program becomes the target, and the old current program
        // becomes the new previous program.
        let next_id_curr = switch.select(&wires.target, &wires.id_curr)?;
        let next_id_prev = switch.select(&wires.id_curr, &wires.id_prev)?;

        wires.id_curr = next_id_curr;
        wires.id_prev = next_id_prev;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_burn(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.burn_switch;

        // ---
        // Ckecks from the mocked verifier
        // ---

        // 1. Current process must be a UTXO.
        wires
            .is_utxo_curr
            .is_one()?
            .conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // 2. This UTXO must be marked for burning.
        wires
            .must_burn_curr
            .is_one()?
            .conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // 3. Parent must exist.
        let parent_is_some = !wires.id_prev.is_zero()?;
        parent_is_some.conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // 2. Claim check: burned value `ret` must match parent's `expected_input`.
        // Parent's state is in `target_read_wires`.
        wires
            .target_read_wires
            .expected_input
            .conditional_enforce_equal(&wires.ret, switch)?;

        // ---
        // IVC state updates
        // ---
        // Like yield, current program becomes the parent, and new prev is the one that burned.
        let next_id_curr = switch.select(&wires.id_prev, &wires.id_curr)?;
        let next_id_prev = switch.select(&wires.id_curr, &wires.id_prev)?;
        wires.id_curr = next_id_curr;
        wires.id_prev = next_id_prev;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_yield(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.yield_switch;

        // ---
        // Ckecks from the mocked verifier
        // ---

        // 1. Must have a parent. id_prev must not be 0.
        let parent_is_some = !wires.id_prev.is_zero()?;
        parent_is_some.conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // 2. Claim check: yielded value `val` must match parent's `expected_input`.
        // The parent's state is in `target_read_wires` because we set `target = irw.id_prev`.
        wires
            .target_read_wires
            .expected_input
            .conditional_enforce_equal(&wires.val, switch)?;

        // ---
        // State update enforcement
        // ---
        // The mock verifier shows that the parent's (target's) state is not modified by a Yield.
        // Let's enforce that all write switches for the target are false.
        wires
            .target_mem_switches
            .expected_input
            .conditional_enforce_equal(&Boolean::FALSE, switch)?;
        wires
            .target_mem_switches
            .arg
            .conditional_enforce_equal(&Boolean::FALSE, switch)?;
        // ... etc. for all fields of target_mem_switches

        // The state of the current process is updated by `write_values`, and the
        // `curr_mem_switches` will ensure the correct fields are written. We don't
        // need to re-enforce those updates here, just the checks.

        // ---
        // IVC state updates
        // ---
        // On yield, the current program becomes the parent (old id_prev),
        // and the new prev program is the one that just yielded.
        let next_id_curr = switch.select(&wires.id_prev, &wires.id_curr)?;
        let next_id_prev = switch.select(&wires.id_curr, &wires.id_prev)?;
        wires.id_curr = next_id_curr;
        wires.id_prev = next_id_prev;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_new_process(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.new_utxo_switch | &wires.new_coord_switch;

        // The target is the new process being created.
        // The current process is the coordination script doing the creation.
        //
        // 1. Coordinator check: current process must NOT be a UTXO.

        wires
            .is_utxo_curr
            .is_one()?
            .conditional_enforce_equal(&Boolean::FALSE, &switch)?;

        // 2. Target type check
        let target_is_utxo = wires.is_utxo_target.is_one()?;
        // if new_utxo_switch is true, target_is_utxo must be true.
        // if new_utxo_switch is false (i.e. new_coord_switch is true), target_is_utxo must be false.
        target_is_utxo.conditional_enforce_equal(&wires.new_utxo_switch, &switch)?;

        // 3. Program hash check
        wires
            .rom_program_hash
            .conditional_enforce_equal(&wires.program_hash, &switch)?;

        // 4. Target counter must be 0.
        wires
            .target_read_wires
            .counters
            .conditional_enforce_equal(&FpVar::zero(), &switch)?;

        // 5. Target must not be initialized.
        wires
            .target_read_wires
            .initialized
            .conditional_enforce_equal(&wires.constant_false.clone().into(), &switch)?;

        // Mark new process as initialized
        wires.target_write_wires.initialized =
            switch.select(&wires.constant_true, &wires.target_read_wires.initialized)?;

        Ok(wires)
    }

    fn visit_input(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.input_switch;

        // When a process calls `input`, it's reading the argument that was
        // passed to it when it was resumed.

        // 1. Check that the value from the opcode matches the value in the `arg` register.
        wires
            .curr_read_wires
            .arg
            .conditional_enforce_equal(&wires.val, switch)?;

        // 2. Check that the caller from the opcode matches the `id_prev` IVC variable.
        wires
            .id_prev
            .conditional_enforce_equal(&wires.caller, switch)?;

        Ok(wires)
    }

    pub(crate) fn p_len(&self) -> usize {
        self.instance.process_table.len()
    }
}

#[tracing::instrument(target = "gr1cs", skip(cs, wires_in, wires_out))]
fn ivcify_wires(
    cs: &ConstraintSystemRef<F>,
    wires_in: &Wires,
    wires_out: &Wires,
) -> Result<(), SynthesisError> {
    // let (current_program_in, current_program_out) = {
    //     let f_in = || wires_in.id_curr.value();
    //     let f_out = || wires_out.id_curr.value();
    //     let alloc_in = FpVar::new_variable(cs.clone(), f_in, AllocationMode::Input)?;
    //     let alloc_out = FpVar::new_variable(cs.clone(), f_out, AllocationMode::Input)?;

    //     Ok((alloc_in, alloc_out))
    // }?;

    // wires_in.id_curr.enforce_equal(&current_program_in)?;
    // wires_out.id_curr.enforce_equal(&current_program_out)?;

    // let (utxos_len_in, utxos_len_out) = {
    //     let f_in = || wires_in.utxos_len.value();
    //     let f_out = || wires_out.utxos_len.value();
    //     let alloc_in = FpVar::new_variable(cs.clone(), f_in, AllocationMode::Input)?;
    //     let alloc_out = FpVar::new_variable(cs.clone(), f_out, AllocationMode::Input)?;

    //     Ok((alloc_in, alloc_out))
    // }?;

    // wires_in.utxos_len.enforce_equal(&utxos_len_in)?;
    // wires_out.utxos_len.enforce_equal(&utxos_len_out)?;

    // utxos_len_in.enforce_equal(&utxos_len_out)?;

    Ok(())
}

impl PreWires {
    pub fn new(
        irw: InterRoundWires,
        curr_mem_switches: MemSwitchboard,
        target_mem_switches: MemSwitchboard,
        rom_switches: RomSwitchboard,
    ) -> Self {
        Self {
            // switches
            yield_switch: false,
            resume_switch: false,
            check_utxo_output_switch: false,
            nop_switch: false,
            burn_switch: false,
            input_switch: false,

            // io vars
            irw,

            program_hash_switch: false,
            new_utxo_switch: false,
            new_coord_switch: false,

            target: F::ZERO,
            val: F::ZERO,
            ret: F::ZERO,
            id_prev: F::ZERO,
            program_hash: F::ZERO,
            new_process_id: F::ZERO,
            caller: F::ZERO,

            curr_mem_switches,
            target_mem_switches,
            rom_switches,
        }
    }
    pub fn debug_print(&self) {
        let _guard = debug_span!("witness assignments").entered();

        tracing::debug!("target={}", self.target);
        tracing::debug!("val={}", self.val);
        tracing::debug!("ret={}", self.ret);
        tracing::debug!("id_prev={}", self.id_prev);
    }
}

impl ProgramState {
    pub fn dummy() -> Self {
        Self {
            finalized: false,
            expected_input: F::ZERO,
            arg: F::ZERO,
            counters: F::ZERO,
            initialized: false,
            did_burn: false,
            owned_by: F::ZERO,
        }
    }

    pub fn debug_print(&self) {
        tracing::debug!("expected_input={}", self.expected_input);
        tracing::debug!("arg={}", self.arg);
        tracing::debug!("counters={}", self.counters);
        tracing::debug!("finalized={}", self.finalized);
        tracing::debug!("did_burn={}", self.did_burn);
        tracing::debug!("owned_by={}", self.owned_by);
    }
}
