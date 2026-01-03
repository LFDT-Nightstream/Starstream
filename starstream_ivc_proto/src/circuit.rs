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
use std::ops::{Mul, Not};
use tracing::debug_span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryTag {
    // ROM tags
    ProcessTable = 1,
    MustBurn = 2,
    IsUtxo = 3,

    // RAM tags
    ExpectedInput = 4,
    Activation = 5,
    Counters = 6,
    Initialized = 7,
    Finalized = 8,
    DidBurn = 9,
    Ownership = 10,
    Init = 11,
    RefArena = 12,
    HandlerStack = 13,
}

impl From<MemoryTag> for u64 {
    fn from(tag: MemoryTag) -> u64 {
        tag as u64
    }
}

impl From<MemoryTag> for F {
    fn from(tag: MemoryTag) -> F {
        F::from(tag as u64)
    }
}

impl MemoryTag {
    pub fn allocate(&self, cs: ConstraintSystemRef<F>) -> Result<FpVar<F>, SynthesisError> {
        FpVar::new_constant(cs, F::from(*self))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramStateTag {
    ExpectedInput,
    Activation,
    Init,
    Counters,
    Initialized,
    Finalized,
    DidBurn,
    Ownership,
}

impl From<ProgramStateTag> for MemoryTag {
    fn from(tag: ProgramStateTag) -> MemoryTag {
        match tag {
            ProgramStateTag::ExpectedInput => MemoryTag::ExpectedInput,
            ProgramStateTag::Activation => MemoryTag::Activation,
            ProgramStateTag::Init => MemoryTag::Init,
            ProgramStateTag::Counters => MemoryTag::Counters,
            ProgramStateTag::Initialized => MemoryTag::Initialized,
            ProgramStateTag::Finalized => MemoryTag::Finalized,
            ProgramStateTag::DidBurn => MemoryTag::DidBurn,
            ProgramStateTag::Ownership => MemoryTag::Ownership,
        }
    }
}

impl From<ProgramStateTag> for u64 {
    fn from(tag: ProgramStateTag) -> u64 {
        let memory_tag: MemoryTag = tag.into();
        memory_tag.into()
    }
}

struct OpcodeConfig {
    mem_switches_curr: MemSwitchboard,
    mem_switches_target: MemSwitchboard,
    rom_switches: RomSwitchboard,
    execution_switches: ExecutionSwitches<bool>,
    pre_wire_values: PreWireValues,
}

#[derive(Clone)]
struct ExecutionSwitches<T> {
    resume: T,
    yield_op: T,
    burn: T,
    program_hash: T,
    new_utxo: T,
    new_coord: T,
    activation: T,
    init: T,
    bind: T,
    unbind: T,
    new_ref: T,
    get: T,
    nop: T,
}

struct PreWireValues {
    target: F,
    val: F,
    ret: F,
    program_hash: F,
    new_process_id: F,
    caller: F,
    ret_is_some: bool,
    id_prev_is_some: bool,
    id_prev_value: F,
}

impl Default for ExecutionSwitches<bool> {
    fn default() -> Self {
        Self {
            resume: false,
            yield_op: false,
            burn: false,
            program_hash: false,
            new_utxo: false,
            new_coord: false,
            activation: false,
            init: false,
            bind: false,
            unbind: false,
            new_ref: false,
            get: false,
            nop: false,
        }
    }
}

impl Default for PreWireValues {
    fn default() -> Self {
        Self {
            target: F::ZERO,
            val: F::ZERO,
            ret: F::ZERO,
            program_hash: F::ZERO,
            new_process_id: F::ZERO,
            caller: F::ZERO,
            ret_is_some: false,
            id_prev_is_some: false,
            id_prev_value: F::ZERO,
        }
    }
}

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

#[derive(Clone, Debug, Default)]
pub struct MemSwitchboard {
    pub expected_input: bool,
    pub activation: bool,
    pub init: bool,
    pub counters: bool,
    pub initialized: bool,
    pub finalized: bool,
    pub did_burn: bool,
    pub ownership: bool,
}

#[derive(Clone)]
pub struct MemSwitchboardWires {
    pub expected_input: Boolean<F>,
    pub activation: Boolean<F>,
    pub init: Boolean<F>,
    pub counters: Boolean<F>,
    pub initialized: Boolean<F>,
    pub finalized: Boolean<F>,
    pub did_burn: Boolean<F>,
    pub ownership: Boolean<F>,
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
    id_prev_is_some: Boolean<F>,
    id_prev_value: FpVar<F>,
    ref_arena_stack_ptr: FpVar<F>,

    p_len: FpVar<F>,

    switches: ExecutionSwitches<Boolean<F>>,

    target: FpVar<F>,
    val: FpVar<F>,
    ret: FpVar<F>,
    program_hash: FpVar<F>,
    caller: FpVar<F>,
    ret_is_some: Boolean<F>,

    curr_read_wires: ProgramStateWires,
    curr_write_wires: ProgramStateWires,

    target_read_wires: ProgramStateWires,
    target_write_wires: ProgramStateWires,

    curr_mem_switches: MemSwitchboardWires,
    target_mem_switches: MemSwitchboardWires,

    ref_arena_read: FpVar<F>,

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
    activation: FpVar<F>,
    init: FpVar<F>,
    counters: FpVar<F>,
    initialized: Boolean<F>,
    finalized: Boolean<F>,
    did_burn: Boolean<F>,
    ownership: FpVar<F>, // an index into the process table
}

// helper so that we always allocate witnesses in the same order
pub struct PreWires {
    target: F,
    val: F,
    ret: F,

    program_hash: F,

    caller: F,

    // switches
    yield_switch: bool,
    resume_switch: bool,
    nop_switch: bool,
    burn_switch: bool,
    program_hash_switch: bool,
    new_utxo_switch: bool,
    new_coord_switch: bool,
    activation_switch: bool,
    init_switch: bool,
    bind_switch: bool,
    unbind_switch: bool,
    new_ref_switch: bool,
    get_switch: bool,

    curr_mem_switches: MemSwitchboard,
    target_mem_switches: MemSwitchboard,
    rom_switches: RomSwitchboard,

    irw: InterRoundWires,

    id_prev_is_some: bool,
    id_prev_value: F,
    ret_is_some: bool,
}

#[derive(Clone, Debug)]
pub struct ProgramState {
    expected_input: F,
    activation: F,
    init: F,
    counters: F,
    initialized: bool,
    finalized: bool,
    did_burn: bool,
    ownership: F, // an index into the process table
}

/// IVC wires (state between steps)
///
/// these get input and output variables
#[derive(Clone)]
pub struct InterRoundWires {
    id_curr: F,
    id_prev_is_some: bool,
    id_prev_value: F,
    ref_arena_counter: F,

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
            activation: FpVar::new_witness(cs.clone(), || Ok(write_values.activation))?,
            init: FpVar::new_witness(cs.clone(), || Ok(write_values.init))?,
            counters: FpVar::new_witness(cs.clone(), || Ok(write_values.counters))?,
            initialized: Boolean::new_witness(cs.clone(), || Ok(write_values.initialized))?,
            finalized: Boolean::new_witness(cs.clone(), || Ok(write_values.finalized))?,
            did_burn: Boolean::new_witness(cs.clone(), || Ok(write_values.did_burn))?,
            ownership: FpVar::new_witness(cs.clone(), || Ok(write_values.ownership))?,
        })
    }
}

macro_rules! define_program_state_operations {
    ($(($field:ident, $tag:ident, $field_type:ident)),* $(,)?) => {
        // Out-of-circuit version
        fn trace_program_state_writes<M: IVCMemory<F>>(
            mem: &mut M,
            pid: u64,
            state: &ProgramState,
            switches: &MemSwitchboard,
        ) {
            $(
                mem.conditional_write(
                    switches.$field,
                    Address {
                        addr: pid,
                        tag: ProgramStateTag::$tag.into(),
                    },
                    [define_program_state_operations!(@convert_to_f state.$field, $field_type)].to_vec(),
                );
            )*
        }

        // In-circuit version
        fn program_state_write_wires<M: IVCMemoryAllocated<F>>(
            rm: &mut M,
            cs: &ConstraintSystemRef<F>,
            address: FpVar<F>,
            state: ProgramStateWires,
            switches: &MemSwitchboardWires,
        ) -> Result<(), SynthesisError> {
            $(
                rm.conditional_write(
                    &switches.$field,
                    &Address {
                        addr: address.clone(),
                        tag: MemoryTag::from(ProgramStateTag::$tag).allocate(cs.clone())?,
                    },
                    &[state.$field.clone().into()],
                )?;
            )*
            Ok(())
        }

        // Out-of-circuit read version
        fn trace_program_state_reads<M: IVCMemory<F>>(
            mem: &mut M,
            pid: u64,
            switches: &MemSwitchboard,
        ) -> ProgramState {
            ProgramState {
                $(
                    $field: define_program_state_operations!(@convert_from_f
                        mem.conditional_read(
                            switches.$field,
                            Address {
                                addr: pid,
                                tag: ProgramStateTag::$tag.into(),
                            },
                        )[0], $field_type),
                )*
            }
        }

        // Just a helper for totality checking
        //
        // this will generate a compiler error if the macro is not called with all variants
        #[allow(dead_code)]
        fn _check_program_state_totality(tag: ProgramStateTag) {
            match tag {
                $(
                    ProgramStateTag::$tag => {},
                )*
            }
        }
    };

    (@convert_to_f $value:expr, field) => { $value };
    (@convert_to_f $value:expr, bool) => { F::from($value) };

    (@convert_from_f $value:expr, field) => { $value };
    (@convert_from_f $value:expr, bool) => { $value == F::ONE };
}

define_program_state_operations!(
    (expected_input, ExpectedInput, field),
    (activation, Activation, field),
    (init, Init, field),
    (counters, Counters, field),
    (initialized, Initialized, bool),
    (finalized, Finalized, bool),
    (did_burn, DidBurn, bool),
    (ownership, Ownership, field),
);

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
        let p_len = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.irw.p_len))?;
        let id_prev_is_some = Boolean::new_witness(cs.clone(), || Ok(vals.irw.id_prev_is_some))?;
        let id_prev_value = FpVar::new_witness(cs.clone(), || Ok(vals.irw.id_prev_value))?;
        let ref_arena_stack_ptr =
            FpVar::new_witness(cs.clone(), || Ok(vals.irw.ref_arena_counter))?;

        // switches
        let switches = [
            vals.resume_switch,
            vals.yield_switch,
            vals.nop_switch,
            vals.burn_switch,
            vals.program_hash_switch,
            vals.new_utxo_switch,
            vals.new_coord_switch,
            vals.activation_switch,
            vals.init_switch,
            vals.bind_switch,
            vals.unbind_switch,
            vals.new_ref_switch,
            vals.get_switch,
        ];

        let allocated_switches: Vec<_> = switches
            .iter()
            .map(|val| Boolean::new_witness(cs.clone(), || Ok(*val)).unwrap())
            .collect();

        let [
            resume_switch,
            yield_switch,
            nop_switch,
            burn_switch,
            program_hash_switch,
            new_utxo_switch,
            new_coord_switch,
            activation_switch,
            init_switch,
            bind_switch,
            unbind_switch,
            new_ref_switch,
            get_switch,
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

        let ret_is_some = Boolean::new_witness(cs.clone(), || Ok(vals.ret_is_some))?;
        let program_hash = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.program_hash))?;

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
                tag: MemoryTag::IsUtxo.allocate(cs.clone())?,
            },
        )?[0]
            .clone();

        let is_utxo_target = rm.conditional_read(
            &rom_switches.read_is_utxo_target,
            &Address {
                addr: target_address.clone(),
                tag: MemoryTag::IsUtxo.allocate(cs.clone())?,
            },
        )?[0]
            .clone();

        let must_burn_curr = rm.conditional_read(
            &rom_switches.read_must_burn_curr,
            &Address {
                addr: id_curr.clone(),
                tag: MemoryTag::MustBurn.allocate(cs.clone())?,
            },
        )?[0]
            .clone();

        let rom_program_hash = rm.conditional_read(
            &rom_switches.read_program_hash_target,
            &Address {
                addr: target_address.clone(),
                tag: MemoryTag::ProcessTable.allocate(cs.clone())?,
            },
        )?[0]
            .clone();

        let ref_arena_read = rm.conditional_read(
            &(get_switch | new_ref_switch),
            &Address {
                tag: MemoryTag::RefArena.allocate(cs.clone())?,
                addr: get_switch.select(&val, &ret)?,
            },
        )?[0]
            .clone();

        Ok(Wires {
            id_curr,
            id_prev_is_some,
            id_prev_value,
            ref_arena_stack_ptr,

            p_len,

            switches: ExecutionSwitches {
                yield_op: yield_switch.clone(),
                resume: resume_switch.clone(),
                burn: burn_switch.clone(),
                program_hash: program_hash_switch.clone(),
                new_utxo: new_utxo_switch.clone(),
                new_coord: new_coord_switch.clone(),
                activation: activation_switch.clone(),
                init: init_switch.clone(),
                bind: bind_switch.clone(),
                unbind: unbind_switch.clone(),
                new_ref: new_ref_switch.clone(),
                get: get_switch.clone(),
                nop: nop_switch.clone(),
            },

            constant_false: Boolean::new_constant(cs.clone(), false)?,
            constant_true: Boolean::new_constant(cs.clone(), true)?,
            constant_one: FpVar::new_constant(cs.clone(), F::from(1))?,

            // wit_wires
            target,
            val,
            ret,
            program_hash,
            caller,
            ret_is_some,

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
            ref_arena_read,
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
                    tag: MemoryTag::ExpectedInput.allocate(cs.clone())?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
        activation: rm
            .conditional_read(
                &switches.activation,
                &Address {
                    addr: address.clone(),
                    tag: MemoryTag::Activation.allocate(cs.clone())?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
        init: rm
            .conditional_read(
                &switches.init,
                &Address {
                    addr: address.clone(),
                    tag: MemoryTag::Init.allocate(cs.clone())?,
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
                    tag: MemoryTag::Counters.allocate(cs.clone())?,
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
                    tag: MemoryTag::Initialized.allocate(cs.clone())?,
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
                    tag: MemoryTag::Finalized.allocate(cs.clone())?,
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
                    tag: MemoryTag::DidBurn.allocate(cs.clone())?,
                },
            )?
            .into_iter()
            .next()
            .unwrap()
            .is_one()?,
        ownership: rm
            .conditional_read(
                &switches.ownership,
                &Address {
                    addr: address.clone(),
                    tag: MemoryTag::Ownership.allocate(cs.clone())?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
    })
}

impl InterRoundWires {
    pub fn new(p_len: F, entrypoint: u64) -> Self {
        InterRoundWires {
            id_curr: F::from(entrypoint),
            id_prev_is_some: false,
            id_prev_value: F::ZERO,
            p_len,
            n_finalized: F::from(0),
            ref_arena_counter: F::ZERO,
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
            "prev_program from ({}, {}) to ({}, {})",
            self.id_prev_is_some,
            self.id_prev_value,
            res.id_prev_is_some.value().unwrap(),
            res.id_prev_value.value().unwrap(),
        );

        self.id_prev_is_some = res.id_prev_is_some.value().unwrap();
        self.id_prev_value = res.id_prev_value.value().unwrap();

        tracing::debug!(
            "utxos_len from {} to {}",
            self.p_len,
            res.p_len.value().unwrap()
        );

        self.p_len = res.p_len.value().unwrap();

        tracing::debug!(
            "ref_arena_counter from {} to {}",
            self.ref_arena_counter,
            res.ref_arena_stack_ptr.value().unwrap()
        );

        self.ref_arena_counter = res.ref_arena_stack_ptr.value().unwrap();
    }
}

impl LedgerOperation<crate::F> {
    // Generate complete opcode configuration
    fn get_config(&self, irw: &InterRoundWires) -> OpcodeConfig {
        let mut config = OpcodeConfig {
            mem_switches_curr: MemSwitchboard::default(),
            mem_switches_target: MemSwitchboard::default(),
            rom_switches: RomSwitchboard::default(),
            execution_switches: ExecutionSwitches::default(),
            pre_wire_values: PreWireValues::default(),
        };

        // All ops increment counter of the current process, except Nop
        config.mem_switches_curr.counters = !matches!(self, LedgerOperation::Nop {});

        match self {
            LedgerOperation::Nop {} => {
                config.execution_switches.nop = true;
            }
            LedgerOperation::Resume {
                target,
                val,
                ret,
                id_prev,
            } => {
                config.execution_switches.resume = true;

                config.mem_switches_curr.activation = true;
                config.mem_switches_curr.expected_input = true;

                config.mem_switches_target.activation = true;
                config.mem_switches_target.expected_input = true;
                config.mem_switches_target.finalized = true;
                config.mem_switches_target.initialized = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;

                config.pre_wire_values.target = *target;
                config.pre_wire_values.val = *val;
                config.pre_wire_values.ret = *ret;
                config.pre_wire_values.id_prev_is_some = id_prev.is_some();
                config.pre_wire_values.id_prev_value = id_prev.unwrap_or_default();
            }
            LedgerOperation::Yield { val, ret, id_prev } => {
                config.execution_switches.yield_op = true;

                config.mem_switches_curr.activation = true;
                if ret.is_some() {
                    config.mem_switches_curr.expected_input = true;
                }
                config.mem_switches_curr.finalized = true;

                config.pre_wire_values.target = irw.id_prev_value;
                config.pre_wire_values.val = *val;
                config.pre_wire_values.ret = ret.unwrap_or_default();
                config.pre_wire_values.ret_is_some = ret.is_some();
                config.pre_wire_values.id_prev_is_some = id_prev.is_some();
                config.pre_wire_values.id_prev_value = id_prev.unwrap_or_default();
            }
            LedgerOperation::Burn { ret } => {
                config.execution_switches.burn = true;

                config.mem_switches_curr.activation = true;
                config.mem_switches_curr.finalized = true;
                config.mem_switches_curr.did_burn = true;
                config.mem_switches_curr.expected_input = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_must_burn_curr = true;

                config.pre_wire_values.target = irw.id_prev_value;
                config.pre_wire_values.ret = *ret;
                config.pre_wire_values.id_prev_is_some = irw.id_prev_is_some;
                config.pre_wire_values.id_prev_value = irw.id_prev_value;
            }
            LedgerOperation::ProgramHash {
                target,
                program_hash,
            } => {
                config.execution_switches.program_hash = true;

                config.pre_wire_values.target = *target;
                config.pre_wire_values.program_hash = *program_hash;
            }
            LedgerOperation::NewUtxo {
                program_hash,
                val,
                target,
            } => {
                config.execution_switches.new_utxo = true;

                config.mem_switches_target.initialized = true;
                config.mem_switches_target.init = true;
                config.mem_switches_target.counters = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;
                config.rom_switches.read_program_hash_target = true;

                config.pre_wire_values.target = *target;
                config.pre_wire_values.val = *val;
                config.pre_wire_values.program_hash = *program_hash;
            }
            LedgerOperation::NewCoord {
                program_hash,
                val,
                target,
            } => {
                config.execution_switches.new_coord = true;

                config.mem_switches_target.initialized = true;
                config.mem_switches_target.init = true;
                config.mem_switches_target.counters = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;
                config.rom_switches.read_program_hash_target = true;

                config.pre_wire_values.target = *target;
                config.pre_wire_values.val = *val;
                config.pre_wire_values.program_hash = *program_hash;
            }
            LedgerOperation::Activation { val, caller } => {
                config.execution_switches.activation = true;

                config.mem_switches_curr.activation = true;

                config.pre_wire_values.val = *val;
                config.pre_wire_values.caller = *caller;
            }
            LedgerOperation::Init { val, caller } => {
                config.execution_switches.init = true;

                config.mem_switches_curr.init = true;

                config.pre_wire_values.val = *val;
                config.pre_wire_values.caller = *caller;
            }
            LedgerOperation::Bind { owner_id } => {
                config.execution_switches.bind = true;

                config.mem_switches_target.initialized = true;
                config.mem_switches_curr.ownership = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;

                config.pre_wire_values.target = *owner_id;
            }
            LedgerOperation::Unbind { token_id } => {
                config.execution_switches.unbind = true;

                config.mem_switches_target.ownership = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;

                config.pre_wire_values.target = *token_id;
            }
            LedgerOperation::NewRef { val, ret } => {
                config.execution_switches.new_ref = true;

                config.pre_wire_values.val = *val;
                config.pre_wire_values.ret = *ret;
            }
            LedgerOperation::Get { reff, ret } => {
                config.execution_switches.get = true;

                config.pre_wire_values.val = *reff;
                config.pre_wire_values.ret = *ret;
            }
        }

        config
    }

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

        match dbg!(self) {
            LedgerOperation::Nop {} => {
                // Nop does nothing to the state
                curr_write.counters -= F::ONE; // revert counter increment
            }
            LedgerOperation::Resume { val, ret, .. } => {
                // Current process gives control to target.
                // It's `arg` is cleared, and its `expected_input` is set to the return value `ret`.
                curr_write.activation = F::ZERO; // Represents None
                curr_write.expected_input = *ret;

                // Target process receives control.
                // Its `arg` is set to `val`, and it is no longer in a `finalized` state.
                target_write.activation = *val;
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
                curr_write.activation = F::ZERO; // Represents None
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
                curr_write.activation = F::ZERO; // Represents None
                curr_write.finalized = true;
                curr_write.did_burn = true;
                curr_write.expected_input = *ret; // Sets its final return value.
            }
            LedgerOperation::NewUtxo { val, target: _, .. }
            | LedgerOperation::NewCoord { val, target: _, .. } => {
                // The current process is a coordinator creating a new process.
                // The new process (target) is initialized.
                target_write.initialized = true;
                target_write.init = *val;
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
        let next_wires = self.visit_activation(next_wires)?;
        let next_wires = self.visit_init(next_wires)?;
        let next_wires = self.visit_bind(next_wires)?;
        let next_wires = self.visit_unbind(next_wires)?;
        let next_wires = self.visit_new_ref(next_wires)?;
        let next_wires = self.visit_get(next_wires)?;

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

            mb.register_mem(MemoryTag::ProcessTable.into(), 1, "ROM_PROCESS_TABLE");
            mb.register_mem(MemoryTag::MustBurn.into(), 1, "ROM_MUST_BURN");
            mb.register_mem(MemoryTag::IsUtxo.into(), 1, "ROM_IS_UTXO");
            mb.register_mem(MemoryTag::ExpectedInput.into(), 1, "RAM_EXPECTED_INPUT");
            mb.register_mem(MemoryTag::Activation.into(), 1, "RAM_ACTIVATION");
            mb.register_mem(MemoryTag::Init.into(), 1, "RAM_INIT");
            mb.register_mem(MemoryTag::Counters.into(), 1, "RAM_COUNTERS");
            mb.register_mem(MemoryTag::Initialized.into(), 1, "RAM_INITIALIZED");
            mb.register_mem(MemoryTag::Finalized.into(), 1, "RAM_FINALIZED");
            mb.register_mem(MemoryTag::DidBurn.into(), 1, "RAM_DID_BURN");
            mb.register_mem(MemoryTag::RefArena.into(), 1, "RAM_REF_ARENA");
            mb.register_mem(MemoryTag::Ownership.into(), 1, "RAM_OWNERSHIP");

            for (pid, mod_hash) in self.instance.process_table.iter().enumerate() {
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::ProcessTable.into(),
                    },
                    // TODO: use a proper conversion from hash to val, this is just a placeholder
                    vec![F::from(mod_hash.0[0] as u64)],
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::Initialized.into(),
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
                        tag: MemoryTag::Counters.into(),
                    },
                    vec![F::from(0u64)],
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::Finalized.into(),
                    },
                    vec![F::from(0u64)], // false
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::DidBurn.into(),
                    },
                    vec![F::from(0u64)], // false
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::ExpectedInput.into(),
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
                        tag: MemoryTag::Activation.into(),
                    },
                    vec![F::from(0u64)], // None
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::Init.into(),
                    },
                    vec![F::from(0u64)], // None
                );
            }

            for (pid, must_burn) in self.instance.must_burn.iter().enumerate() {
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::MustBurn.into(),
                    },
                    vec![F::from(if *must_burn { 1u64 } else { 0 })],
                );
            }

            for (pid, is_utxo) in self.instance.is_utxo.iter().enumerate() {
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::IsUtxo.into(),
                    },
                    vec![F::from(if *is_utxo { 1u64 } else { 0 })],
                );
            }

            for (pid, owner) in self.instance.ownership_in.iter().enumerate() {
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::Ownership.into(),
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

        // We need a dummy IRW for the trace phase since we don't have the actual IRW yet
        let dummy_irw = InterRoundWires::new(
            F::from(self.p_len() as u64),
            self.instance.entrypoint.0 as u64,
        );

        for instr in &self.ops {
            let (curr_switches, target_switches) = opcode_to_mem_switches(instr, &dummy_irw);
            self.mem_switches
                .push((curr_switches.clone(), target_switches.clone()));

            let rom_switches = opcode_to_rom_switches(instr, &dummy_irw);
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
                    tag: MemoryTag::IsUtxo.into(),
                },
            );
            mb.conditional_read(
                rom_switches.read_is_utxo_target,
                Address {
                    addr: target_pid.unwrap_or(0),
                    tag: MemoryTag::IsUtxo.into(),
                },
            );
            mb.conditional_read(
                rom_switches.read_must_burn_curr,
                Address {
                    addr: curr_pid,
                    tag: MemoryTag::MustBurn.into(),
                },
            );
            mb.conditional_read(
                rom_switches.read_program_hash_target,
                Address {
                    addr: target_pid.unwrap_or(0),
                    tag: MemoryTag::ProcessTable.into(),
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

        for instr in &self.ops {
            let ref_read_arena_address = match instr {
                LedgerOperation::NewRef { val, ret } => {
                    // we never pop from this, actually
                    mb.init(
                        dbg!(Address {
                            tag: MemoryTag::RefArena.into(),
                            addr: ret.into_bigint().0[0],
                        }),
                        vec![val.clone()],
                    );

                    Some(ret)
                }
                LedgerOperation::Get { reff, ret: _ } => Some(reff),
                _ => None,
            };

            mb.conditional_read(
                ref_read_arena_address.is_some(),
                Address {
                    tag: MemoryTag::RefArena.into(),
                    addr: ref_read_arena_address
                        .map(|addr| addr.into_bigint().0[0])
                        .unwrap_or(0),
                },
            );
        }

        let current_steps = self.ops.len();
        if let Some(missing) = mb.required_steps().checked_sub(current_steps) {
            tracing::debug!("padding with {missing} Nop operations for scan");
            self.ops
                .extend(std::iter::repeat_n(LedgerOperation::Nop {}, missing));
        }

        mb
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
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
                    id_prev_is_some: id_prev.is_some(),
                    id_prev_value: id_prev.unwrap_or_default(),
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
                    target: irw.id_prev_value,
                    val: val.clone(),
                    ret: ret.unwrap_or_default(),
                    ret_is_some: ret.is_some(),
                    id_prev_is_some: id_prev.is_some(),
                    id_prev_value: id_prev.unwrap_or_default(),
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
                    target: irw.id_prev_value,
                    ret: ret.clone(),
                    id_prev_is_some: irw.id_prev_is_some,
                    id_prev_value: irw.id_prev_value,
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
            LedgerOperation::Activation { val, caller } => {
                let irw = PreWires {
                    activation_switch: true,
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
            LedgerOperation::Init { val, caller } => {
                let irw = PreWires {
                    init_switch: true,
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
            LedgerOperation::Bind { owner_id } => {
                let irw = PreWires {
                    bind_switch: true,
                    target: *owner_id,
                    irw: irw.clone(),
                    // TODO: it feels like this can be refactored out, since it
                    // seems to be the same on all branches
                    ..PreWires::new(
                        irw.clone(),
                        curr_mem_switches.clone(),
                        target_mem_switches.clone(),
                        rom_switches.clone(),
                    )
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Unbind { token_id } => {
                let irw = PreWires {
                    unbind_switch: true,
                    target: *token_id,
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
            LedgerOperation::NewRef { val, ret } => {
                let irw = PreWires {
                    val: *val,
                    ret: *ret,
                    new_ref_switch: true,
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
            LedgerOperation::Get { reff, ret } => {
                let irw = PreWires {
                    val: *reff,
                    ret: *ret,
                    get_switch: true,
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
        let switch = &wires.switches.resume;

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
            .activation
            .conditional_enforce_equal(&FpVar::zero(), switch)?;

        // 5. Claim check: val passed in must match target's expected_input.
        wires
            .target_read_wires
            .expected_input
            .conditional_enforce_equal(&wires.val, switch)?;

        // ---
        // IVC state updates
        // ---
        // On resume, current program becomes the target, and the old current program
        // becomes the new previous program.
        let next_id_curr = switch.select(&wires.target, &wires.id_curr)?;
        let next_id_prev_is_some = switch.select(&Boolean::TRUE, &wires.id_prev_is_some)?;
        let next_id_prev_value = switch.select(&wires.id_curr, &wires.id_prev_value)?;

        wires.id_curr = next_id_curr;
        wires.id_prev_is_some = next_id_prev_is_some;
        wires.id_prev_value = next_id_prev_value;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_burn(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.burn;

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
        wires
            .id_prev_is_some
            .conditional_enforce_equal(&Boolean::TRUE, switch)?;

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
        let next_id_curr = switch.select(&wires.id_prev_value, &wires.id_curr)?;
        let next_id_prev_is_some = switch.select(&Boolean::TRUE, &wires.id_prev_is_some)?;
        let next_id_prev_value = switch.select(&wires.id_curr, &wires.id_prev_value)?;
        wires.id_curr = next_id_curr;
        wires.id_prev_is_some = next_id_prev_is_some;
        wires.id_prev_value = next_id_prev_value;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_yield(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.yield_op;

        // 1. Must have a parent.
        wires
            .id_prev_is_some
            .conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // 2. Claim check: yielded value `val` must match parent's `expected_input`.
        // The parent's state is in `target_read_wires` because we set `target = irw.id_prev_value`.
        wires
            .target_read_wires
            .expected_input
            .conditional_enforce_equal(&wires.val, &(switch & (&wires.ret_is_some)))?;

        // ---
        // State update enforcement
        // ---
        // The state of the current process is updated by `write_values`.
        // The `finalized` state depends on whether this is the last yield.
        // `finalized` is true IFF `ret` is None.
        wires
            .curr_write_wires
            .finalized
            .conditional_enforce_equal(&wires.ret_is_some.clone().not(), switch)?;

        // The next `expected_input` should be `ret_value` if `ret` is Some, and 0 otherwise.
        let new_expected_input = wires.ret_is_some.select(&wires.ret, &FpVar::zero())?;
        wires
            .curr_write_wires
            .expected_input
            .conditional_enforce_equal(&new_expected_input, switch)?;

        // ---
        // IVC state updates
        // ---
        // On yield, the current program becomes the parent (old id_prev),
        // and the new prev program is the one that just yielded.
        let next_id_curr = switch.select(&wires.id_prev_value, &wires.id_curr)?;
        let next_id_prev_is_some = switch.select(&Boolean::TRUE, &wires.id_prev_is_some)?;
        let next_id_prev_value = switch.select(&wires.id_curr, &wires.id_prev_value)?;
        wires.id_curr = next_id_curr;
        wires.id_prev_is_some = next_id_prev_is_some;
        wires.id_prev_value = next_id_prev_value;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_new_process(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.new_utxo | &wires.switches.new_coord;

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
        target_is_utxo.conditional_enforce_equal(&wires.switches.new_utxo, &switch)?;

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
        // TODO: There is no need to have this asignment, actually
        wires.target_write_wires.initialized =
            switch.select(&wires.constant_true, &wires.target_read_wires.initialized)?;

        wires.target_write_wires.init = switch.select(&wires.val, &wires.target_read_wires.init)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_activation(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.activation;

        // When a process calls `input`, it's reading the argument that was
        // passed to it when it was resumed.

        // 1. Check that the value from the opcode matches the value in the `arg` register.
        wires
            .curr_read_wires
            .activation
            .conditional_enforce_equal(&wires.val, switch)?;

        // 2. Check that the caller from the opcode matches the `id_prev` IVC variable.
        wires
            .id_prev_value
            .conditional_enforce_equal(&wires.caller, switch)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_init(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.init;

        // When a process calls `input`, it's reading the argument that was
        // passed to it when it was resumed.

        // 1. Check that the value from the opcode matches the value in the `arg` register.
        wires
            .curr_read_wires
            .init
            .conditional_enforce_equal(&wires.val, switch)?;

        // 2. Check that the caller from the opcode matches the `id_prev` IVC variable.
        wires
            .id_prev_value
            .conditional_enforce_equal(&wires.caller, switch)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_bind(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.bind;

        // curr is the token (or the utxo bound to the target)
        let is_utxo_curr = wires.is_utxo_curr.is_one()?;
        let is_utxo_target = wires.is_utxo_target.is_one()?;

        // we don't need to check if the token is initialized because
        // we can't resume an unitialized token anyway
        let is_initialized_target = &wires.target_read_wires.initialized;

        (is_utxo_curr & is_utxo_target & is_initialized_target)
            .conditional_enforce_equal(&wires.constant_true, switch)?;

        wires
            .curr_read_wires
            .ownership
            .conditional_enforce_equal(&wires.p_len, switch)?;

        // TODO: no need to have this assignment, probably
        wires.curr_write_wires.ownership =
            switch.select(&wires.target, &wires.curr_read_wires.ownership)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_unbind(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.unbind;

        let is_utxo_curr = wires.is_utxo_curr.is_one()?;
        let is_utxo_target = wires.is_utxo_target.is_one()?;

        (is_utxo_curr & is_utxo_target).conditional_enforce_equal(&wires.constant_true, switch)?;

        // only the owner can unbind
        wires
            .target_read_wires
            .ownership
            .conditional_enforce_equal(&wires.id_curr, switch)?;

        // p_len is a sentinel for None
        // TODO: no need to assign
        wires.target_write_wires.ownership =
            switch.select(&wires.p_len, &wires.curr_read_wires.ownership)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_new_ref(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.new_ref;

        wires
            .ret
            .conditional_enforce_equal(&wires.ref_arena_stack_ptr, switch)?;

        wires
            .val
            .conditional_enforce_equal(&wires.ref_arena_read, switch)?;

        wires.ref_arena_stack_ptr = switch.select(
            &(&wires.ref_arena_stack_ptr + &wires.constant_one),
            &wires.ref_arena_stack_ptr,
        )?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_get(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.get;

        // TODO: prove that reff is < ref_arena_stack_ptr ?
        // or is it not needed? since we never decrease?
        //
        // the problem is that this seems to require range checks
        //
        // but technically it shouldn't be possible to ask for a value that is
        // not allocated yet (even if in zk we can allocate everything from the beginning
        // since we don't pop)

        wires
            .ret
            .conditional_enforce_equal(&wires.ref_arena_read, switch)?;

        Ok(wires)
    }

    pub(crate) fn p_len(&self) -> usize {
        self.instance.process_table.len()
    }
}

pub fn opcode_to_mem_switches(
    instr: &LedgerOperation<F>,
    irw: &InterRoundWires,
) -> (MemSwitchboard, MemSwitchboard) {
    let config = instr.get_config(irw);
    (config.mem_switches_curr, config.mem_switches_target)
}

pub fn opcode_to_rom_switches(instr: &LedgerOperation<F>, irw: &InterRoundWires) -> RomSwitchboard {
    let config = instr.get_config(irw);
    config.rom_switches
}

#[tracing::instrument(target = "gr1cs", skip_all)]
fn ivcify_wires(
    _cs: &ConstraintSystemRef<F>,
    _wires_in: &Wires,
    _wires_out: &Wires,
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
            nop_switch: false,
            burn_switch: false,
            activation_switch: false,
            init_switch: false,
            bind_switch: false,
            unbind_switch: false,
            new_ref_switch: false,
            get_switch: false,

            // io vars
            irw,

            program_hash_switch: false,
            new_utxo_switch: false,
            new_coord_switch: false,

            target: F::ZERO,
            val: F::ZERO,
            ret: F::ZERO,
            program_hash: F::ZERO,
            caller: F::ZERO,
            ret_is_some: false,

            id_prev_is_some: false,
            id_prev_value: F::ZERO,

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
        tracing::debug!("id_prev=({}, {})", self.id_prev_is_some, self.id_prev_value);
    }
}

impl MemSwitchboardWires {
    pub fn allocate(
        cs: ConstraintSystemRef<F>,
        switches: &MemSwitchboard,
    ) -> Result<Self, SynthesisError> {
        Ok(Self {
            expected_input: Boolean::new_witness(cs.clone(), || Ok(switches.expected_input))?,
            activation: Boolean::new_witness(cs.clone(), || Ok(switches.activation))?,
            init: Boolean::new_witness(cs.clone(), || Ok(switches.init))?,
            counters: Boolean::new_witness(cs.clone(), || Ok(switches.counters))?,
            initialized: Boolean::new_witness(cs.clone(), || Ok(switches.initialized))?,
            finalized: Boolean::new_witness(cs.clone(), || Ok(switches.finalized))?,
            did_burn: Boolean::new_witness(cs.clone(), || Ok(switches.did_burn))?,
            ownership: Boolean::new_witness(cs.clone(), || Ok(switches.ownership))?,
        })
    }
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

impl ProgramState {
    pub fn dummy() -> Self {
        Self {
            finalized: false,
            expected_input: F::ZERO,
            activation: F::ZERO,
            init: F::ZERO,
            counters: F::ZERO,
            initialized: false,
            did_burn: false,
            ownership: F::ZERO,
        }
    }

    pub fn debug_print(&self) {
        tracing::debug!("expected_input={}", self.expected_input);
        tracing::debug!("activation={}", self.activation);
        tracing::debug!("init={}", self.init);
        tracing::debug!("counters={}", self.counters);
        tracing::debug!("finalized={}", self.finalized);
        tracing::debug!("did_burn={}", self.did_burn);
        tracing::debug!("ownership={}", self.ownership);
    }
}
