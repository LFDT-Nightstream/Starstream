use crate::memory::twist_and_shout::Lanes;
use crate::memory::{self, Address, IVCMemory, MemType};
use crate::value_to_field;
use crate::{F, LedgerOperation, memory::IVCMemoryAllocated};
use ark_ff::{AdditiveGroup, Field as _, PrimeField};
use ark_r1cs_std::fields::FieldVar;
use ark_r1cs_std::{
    GR1CSVar as _, alloc::AllocVar as _, eq::EqGadget, fields::fp::FpVar, prelude::Boolean,
};
use ark_relations::{
    gr1cs::{ConstraintSystemRef, LinearCombination, SynthesisError, Variable},
    ns,
};
use starstream_mock_ledger::InterleavingInstance;
use std::collections::{BTreeMap, BTreeSet};
use std::marker::PhantomData;
use std::ops::Not;
use tracing::debug_span;

#[derive(Debug, Clone)]
struct InterfaceResolver {
    mapping: BTreeMap<F, usize>,
}

impl InterfaceResolver {
    fn new(ops: &[LedgerOperation<F>]) -> Self {
        let mut unique_interfaces = BTreeSet::new();
        for op in ops.iter() {
            match op {
                LedgerOperation::InstallHandler { interface_id } => {
                    unique_interfaces.insert(*interface_id);
                }
                LedgerOperation::UninstallHandler { interface_id } => {
                    unique_interfaces.insert(*interface_id);
                }
                LedgerOperation::GetHandlerFor { interface_id, .. } => {
                    unique_interfaces.insert(*interface_id);
                }
                _ => (),
            }
        }

        let mapping = unique_interfaces
            .iter()
            .enumerate()
            .map(|(index, interface_id)| (*interface_id, index))
            .collect();

        Self { mapping }
    }

    fn get_index(&self, interface_id: F) -> usize {
        *self.mapping.get(&interface_id).unwrap_or(&0)
    }

    fn get_interface_index_field(&self, interface_id: F) -> F {
        F::from(self.get_index(interface_id) as u64)
    }

    fn interfaces(&self) -> Vec<F> {
        let mut interfaces = vec![F::ZERO; self.mapping.len()];
        for (interface_id, index) in &self.mapping {
            interfaces[*index] = *interface_id;
        }
        interfaces
    }
}

#[derive(Clone)]
struct HandlerState {
    handler_stack_node_process: FpVar<F>,
    interface_rom_read: FpVar<F>,
}

#[derive(Clone, Debug, Default)]
pub struct HandlerSwitchboard {
    pub read_interface: bool,
    pub read_head: bool,
    pub read_node: bool,
    pub write_node: bool,
    pub write_head: bool,
}

#[derive(Clone)]
pub struct HandlerSwitchboardWires {
    pub read_interface: Boolean<F>,
    pub read_head: Boolean<F>,
    pub read_node: Boolean<F>,
    pub write_node: Boolean<F>,
    pub write_head: Boolean<F>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryTag {
    // ROM tags
    ProcessTable = 1,
    MustBurn = 2,
    IsUtxo = 3,
    Interfaces = 4,

    // RAM tags
    ExpectedInput = 5,
    Activation = 6,
    Counters = 7,
    Initialized = 8,
    Finalized = 9,
    DidBurn = 10,
    Ownership = 11,
    Init = 12,
    RefArena = 13,
    HandlerStackArenaProcess = 14,
    HandlerStackArenaNextPtr = 15,
    HandlerStackHeads = 16,
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
    handler_switches: HandlerSwitchboard,
    execution_switches: ExecutionSwitches<bool>,
    opcode_var_values: OpcodeVarValues,
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
    ref_push: T,
    get: T,
    install_handler: T,
    uninstall_handler: T,
    get_handler_for: T,
    nop: T,
}
impl ExecutionSwitches<bool> {
    fn nop() -> Self {
        Self {
            nop: true,
            ..Self::default()
        }
    }

    fn resume() -> Self {
        Self {
            resume: true,
            ..Self::default()
        }
    }

    fn yield_op() -> Self {
        Self {
            yield_op: true,
            ..Self::default()
        }
    }

    fn burn() -> Self {
        Self {
            burn: true,
            ..Self::default()
        }
    }

    fn program_hash() -> Self {
        Self {
            program_hash: true,
            ..Self::default()
        }
    }

    fn new_utxo() -> Self {
        Self {
            new_utxo: true,
            ..Self::default()
        }
    }

    fn new_coord() -> Self {
        Self {
            new_coord: true,
            ..Self::default()
        }
    }

    fn activation() -> Self {
        Self {
            activation: true,
            ..Self::default()
        }
    }

    fn init() -> Self {
        Self {
            init: true,
            ..Self::default()
        }
    }

    fn bind() -> Self {
        Self {
            bind: true,
            ..Self::default()
        }
    }

    fn unbind() -> Self {
        Self {
            unbind: true,
            ..Self::default()
        }
    }

    fn new_ref() -> Self {
        Self {
            new_ref: true,
            ..Self::default()
        }
    }

    fn ref_push() -> Self {
        Self {
            ref_push: true,
            ..Self::default()
        }
    }

    fn get() -> Self {
        Self {
            get: true,
            ..Self::default()
        }
    }

    fn install_handler() -> Self {
        Self {
            install_handler: true,
            ..Self::default()
        }
    }

    fn uninstall_handler() -> Self {
        Self {
            uninstall_handler: true,
            ..Self::default()
        }
    }

    fn get_handler_for() -> Self {
        Self {
            get_handler_for: true,
            ..Self::default()
        }
    }

    /// Allocates circuit variables for the switches and enforces exactly one is true
    fn allocate_and_constrain(
        &self,
        cs: ConstraintSystemRef<F>,
    ) -> Result<ExecutionSwitches<Boolean<F>>, SynthesisError> {
        let switches = [
            self.resume,
            self.yield_op,
            self.nop,
            self.burn,
            self.program_hash,
            self.new_utxo,
            self.new_coord,
            self.activation,
            self.init,
            self.bind,
            self.unbind,
            self.new_ref,
            self.ref_push,
            self.get,
            self.install_handler,
            self.uninstall_handler,
            self.get_handler_for,
        ];

        let allocated_switches: Vec<_> = switches
            .iter()
            .map(|val| Boolean::new_witness(cs.clone(), || Ok(*val)).unwrap())
            .collect();

        // Enforce exactly one switch is true
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

        let [
            resume,
            yield_op,
            nop,
            burn,
            program_hash,
            new_utxo,
            new_coord,
            activation,
            init,
            bind,
            unbind,
            new_ref,
            ref_push,
            get,
            install_handler,
            uninstall_handler,
            get_handler_for,
        ] = allocated_switches.as_slice()
        else {
            unreachable!()
        };

        Ok(ExecutionSwitches {
            resume: resume.clone(),
            yield_op: yield_op.clone(),
            nop: nop.clone(),
            burn: burn.clone(),
            program_hash: program_hash.clone(),
            new_utxo: new_utxo.clone(),
            new_coord: new_coord.clone(),
            activation: activation.clone(),
            init: init.clone(),
            bind: bind.clone(),
            unbind: unbind.clone(),
            new_ref: new_ref.clone(),
            ref_push: ref_push.clone(),
            get: get.clone(),
            install_handler: install_handler.clone(),
            uninstall_handler: uninstall_handler.clone(),
            get_handler_for: get_handler_for.clone(),
        })
    }
}

struct OpcodeVarValues {
    target: F,
    val: F,
    ret: F,
    offset: F,
    size: F,
    program_hash: F,
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
            ref_push: false,
            get: false,
            install_handler: false,
            uninstall_handler: false,
            get_handler_for: false,
            nop: false,
        }
    }
}

impl Default for OpcodeVarValues {
    fn default() -> Self {
        Self {
            target: F::ZERO,
            val: F::ZERO,
            ret: F::ZERO,
            offset: F::ZERO,
            size: F::ZERO,
            program_hash: F::ZERO,
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
    handler_switches: Vec<HandlerSwitchboard>,
    interface_resolver: InterfaceResolver,

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
    handler_stack_ptr: FpVar<F>,

    ref_building_remaining: FpVar<F>,
    ref_building_ptr: FpVar<F>,

    p_len: FpVar<F>,

    switches: ExecutionSwitches<Boolean<F>>,

    target: FpVar<F>,
    val: FpVar<F>,
    ret: FpVar<F>,
    offset: FpVar<F>,
    size: FpVar<F>,
    program_hash: FpVar<F>,
    caller: FpVar<F>,
    ret_is_some: Boolean<F>,

    curr_read_wires: ProgramStateWires,
    curr_write_wires: ProgramStateWires,

    target_read_wires: ProgramStateWires,
    target_write_wires: ProgramStateWires,

    ref_arena_read: FpVar<F>,
    handler_state: HandlerState,

    // ROM lookup results
    is_utxo_curr: FpVar<F>,
    is_utxo_target: FpVar<F>,
    must_burn_curr: FpVar<F>,
    rom_program_hash: FpVar<F>,

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
    offset: F,
    size: F,

    program_hash: F,

    caller: F,
    interface_index: F,

    switches: ExecutionSwitches<bool>,

    curr_mem_switches: MemSwitchboard,
    target_mem_switches: MemSwitchboard,
    rom_switches: RomSwitchboard,
    handler_switches: HandlerSwitchboard,

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
    handler_stack_counter: F,

    ref_building_remaining: F,
    ref_building_ptr: F,

    p_len: F,
    _n_finalized: F,
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
    // IMPORTANT: no rust branches in this function, since the purpose of this
    // is to get the exact same layout for all the opcodes
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
        let handler_stack_counter =
            FpVar::new_witness(cs.clone(), || Ok(vals.irw.handler_stack_counter))?;

        let ref_building_remaining =
            FpVar::new_witness(cs.clone(), || Ok(vals.irw.ref_building_remaining))?;
        let ref_building_ptr = FpVar::new_witness(cs.clone(), || Ok(vals.irw.ref_building_ptr))?;

        // Allocate switches and enforce exactly one is true
        let switches = vals.switches.allocate_and_constrain(cs.clone())?;

        let target = FpVar::<F>::new_witness(ns!(cs.clone(), "target"), || Ok(vals.target))?;

        let val = FpVar::<F>::new_witness(ns!(cs.clone(), "val"), || Ok(vals.val))?;
        let ret = FpVar::<F>::new_witness(ns!(cs.clone(), "ret"), || Ok(vals.ret))?;
        let offset = FpVar::<F>::new_witness(ns!(cs.clone(), "offset"), || Ok(vals.offset))?;
        let size = FpVar::<F>::new_witness(ns!(cs.clone(), "size"), || Ok(vals.size))?;

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

        // addr = ref + offset
        let get_addr = &val + &offset;

        let ref_arena_read = rm.conditional_read(
            &switches.get,
            &Address {
                tag: MemoryTag::RefArena.allocate(cs.clone())?,
                addr: get_addr,
            },
        )?[0]
            .clone();

        // We also need to write for RefPush.
        // Address for write: ref_building_ptr
        let push_addr = ref_building_ptr.clone();

        rm.conditional_write(
            &switches.ref_push,
            &Address {
                tag: MemoryTag::RefArena.allocate(cs.clone())?,
                addr: push_addr,
            },
            &[val.clone()],
        )?;

        let handler_switches =
            HandlerSwitchboardWires::allocate(cs.clone(), &vals.handler_switches)?;

        let interface_index_var = FpVar::new_witness(cs.clone(), || Ok(vals.interface_index))?;

        let interface_rom_read = rm.conditional_read(
            &handler_switches.read_interface,
            &Address {
                tag: MemoryTag::Interfaces.allocate(cs.clone())?,
                addr: interface_index_var.clone(),
            },
        )?[0]
            .clone();

        let handler_stack_head_read = rm.conditional_read(
            &handler_switches.read_head,
            &Address {
                tag: MemoryTag::HandlerStackHeads.allocate(cs.clone())?,
                addr: interface_index_var.clone(),
            },
        )?[0]
            .clone();

        let handler_stack_node_process = rm.conditional_read(
            &handler_switches.read_node,
            &Address {
                tag: MemoryTag::HandlerStackArenaProcess.allocate(cs.clone())?,
                addr: handler_stack_head_read.clone(),
            },
        )?[0]
            .clone();

        let handler_stack_node_next = rm.conditional_read(
            &handler_switches.read_node,
            &Address {
                tag: MemoryTag::HandlerStackArenaNextPtr.allocate(cs.clone())?,
                addr: handler_stack_head_read.clone(),
            },
        )?[0]
            .clone();

        rm.conditional_write(
            &handler_switches.write_node,
            &Address {
                tag: MemoryTag::HandlerStackArenaProcess.allocate(cs.clone())?,
                addr: handler_stack_counter.clone(),
            },
            &[handler_switches
                .write_node
                .select(&id_curr, &FpVar::new_constant(cs.clone(), F::ZERO)?)?], // process_id
        )?;

        rm.conditional_write(
            &handler_switches.write_node,
            &Address {
                tag: MemoryTag::HandlerStackArenaNextPtr.allocate(cs.clone())?,
                addr: handler_stack_counter.clone(),
            },
            &[handler_switches.write_node.select(
                &handler_stack_head_read,
                &FpVar::new_constant(cs.clone(), F::ZERO)?,
            )?], // next_ptr (old head)
        )?;

        rm.conditional_write(
            &handler_switches.write_head,
            &Address {
                tag: MemoryTag::HandlerStackHeads.allocate(cs.clone())?,
                addr: interface_index_var.clone(),
            },
            &[handler_switches.write_node.select(
                &handler_stack_counter, // install: new node becomes head
                &handler_stack_node_next,
            )?],
        )?;

        let handler_state = HandlerState {
            handler_stack_node_process,
            interface_rom_read: interface_rom_read.clone(),
        };

        Ok(Wires {
            id_curr,
            id_prev_is_some,
            id_prev_value,
            ref_arena_stack_ptr,
            handler_stack_ptr: handler_stack_counter,

            ref_building_remaining,
            ref_building_ptr,

            p_len,

            switches,

            constant_false: Boolean::new_constant(cs.clone(), false)?,
            constant_true: Boolean::new_constant(cs.clone(), true)?,
            constant_one: FpVar::new_constant(cs.clone(), F::from(1))?,

            // wit_wires
            target,
            val,
            ret,
            offset,
            size,
            program_hash,
            caller,
            ret_is_some,

            curr_read_wires,
            curr_write_wires,

            target_read_wires,
            target_write_wires,

            is_utxo_curr,
            is_utxo_target,
            must_burn_curr,
            rom_program_hash,
            ref_arena_read,
            handler_state,
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
            _n_finalized: F::from(0),
            ref_arena_counter: F::ZERO,
            handler_stack_counter: F::ZERO,
            ref_building_remaining: F::ZERO,
            ref_building_ptr: F::ZERO,
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

        tracing::debug!(
            "handler_stack_counter from {} to {}",
            self.handler_stack_counter,
            res.handler_stack_ptr.value().unwrap()
        );

        self.handler_stack_counter = res.handler_stack_ptr.value().unwrap();

        self.ref_building_remaining = res.ref_building_remaining.value().unwrap();
        self.ref_building_ptr = res.ref_building_ptr.value().unwrap();
    }
}

impl LedgerOperation<crate::F> {
    fn get_config(&self, irw: &InterRoundWires) -> OpcodeConfig {
        let mut config = OpcodeConfig {
            mem_switches_curr: MemSwitchboard::default(),
            mem_switches_target: MemSwitchboard::default(),
            rom_switches: RomSwitchboard::default(),
            handler_switches: HandlerSwitchboard::default(),
            execution_switches: ExecutionSwitches::default(),
            opcode_var_values: OpcodeVarValues::default(),
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

                config.opcode_var_values.target = *target;
                config.opcode_var_values.val = *val;
                config.opcode_var_values.ret = *ret;
                config.opcode_var_values.id_prev_is_some = id_prev.is_some();
                config.opcode_var_values.id_prev_value = id_prev.unwrap_or_default();
            }
            LedgerOperation::Yield { val, ret, id_prev } => {
                config.execution_switches.yield_op = true;

                config.mem_switches_curr.activation = true;
                if ret.is_some() {
                    config.mem_switches_curr.expected_input = true;
                }
                config.mem_switches_curr.finalized = true;

                config.opcode_var_values.target = irw.id_prev_value;
                config.opcode_var_values.val = *val;
                config.opcode_var_values.ret = ret.unwrap_or_default();
                config.opcode_var_values.ret_is_some = ret.is_some();
                config.opcode_var_values.id_prev_is_some = id_prev.is_some();
                config.opcode_var_values.id_prev_value = id_prev.unwrap_or_default();
            }
            LedgerOperation::Burn { ret } => {
                config.execution_switches.burn = true;

                config.mem_switches_curr.activation = true;
                config.mem_switches_curr.finalized = true;
                config.mem_switches_curr.did_burn = true;
                config.mem_switches_curr.expected_input = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_must_burn_curr = true;

                config.opcode_var_values.target = irw.id_prev_value;
                config.opcode_var_values.ret = *ret;
                config.opcode_var_values.id_prev_is_some = irw.id_prev_is_some;
                config.opcode_var_values.id_prev_value = irw.id_prev_value;
            }
            LedgerOperation::ProgramHash {
                target,
                program_hash,
            } => {
                config.execution_switches.program_hash = true;

                config.opcode_var_values.target = *target;
                config.opcode_var_values.program_hash = *program_hash;
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

                config.opcode_var_values.target = *target;
                config.opcode_var_values.val = *val;
                config.opcode_var_values.program_hash = *program_hash;
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

                config.opcode_var_values.target = *target;
                config.opcode_var_values.val = *val;
                config.opcode_var_values.program_hash = *program_hash;
            }
            LedgerOperation::Activation { val, caller } => {
                config.execution_switches.activation = true;

                config.mem_switches_curr.activation = true;

                config.opcode_var_values.val = *val;
                config.opcode_var_values.caller = *caller;
            }
            LedgerOperation::Init { val, caller } => {
                config.execution_switches.init = true;

                config.mem_switches_curr.init = true;

                config.opcode_var_values.val = *val;
                config.opcode_var_values.caller = *caller;
            }
            LedgerOperation::Bind { owner_id } => {
                config.execution_switches.bind = true;

                config.mem_switches_target.initialized = true;
                config.mem_switches_curr.ownership = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;

                config.opcode_var_values.target = *owner_id;
            }
            LedgerOperation::Unbind { token_id } => {
                config.execution_switches.unbind = true;

                config.mem_switches_target.ownership = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;

                config.opcode_var_values.target = *token_id;
            }
            LedgerOperation::NewRef { size, ret } => {
                config.execution_switches.new_ref = true;

                config.opcode_var_values.size = *size;
                config.opcode_var_values.ret = *ret;
            }
            LedgerOperation::RefPush { val } => {
                config.execution_switches.ref_push = true;

                config.opcode_var_values.val = *val;
            }
            LedgerOperation::Get { reff, offset, ret } => {
                config.execution_switches.get = true;

                config.opcode_var_values.val = *reff;
                config.opcode_var_values.offset = *offset;
                config.opcode_var_values.ret = *ret;
            }
            LedgerOperation::InstallHandler { interface_id } => {
                config.execution_switches.install_handler = true;
                config.rom_switches.read_is_utxo_curr = true;

                config.handler_switches.read_interface = true;
                config.handler_switches.read_head = true;
                config.handler_switches.write_node = true;
                config.handler_switches.write_head = true;

                config.opcode_var_values.val = *interface_id;
            }
            LedgerOperation::UninstallHandler { interface_id } => {
                config.execution_switches.uninstall_handler = true;
                config.rom_switches.read_is_utxo_curr = true;

                config.handler_switches.read_interface = true;
                config.handler_switches.read_head = true;
                config.handler_switches.read_node = true;
                config.handler_switches.write_head = true;

                config.opcode_var_values.val = *interface_id;
            }
            LedgerOperation::GetHandlerFor {
                interface_id,
                handler_id,
            } => {
                config.execution_switches.get_handler_for = true;

                config.handler_switches.read_interface = true;
                config.handler_switches.read_head = true;
                config.handler_switches.read_node = true;

                config.opcode_var_values.val = *interface_id;
                config.opcode_var_values.ret = *handler_id;
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

        match self {
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

        let interface_resolver = InterfaceResolver::new(&ops);

        Self {
            ops,
            write_ops: vec![],
            mem_switches: vec![],
            rom_switches: vec![],
            handler_switches: vec![],
            interface_resolver,
            mem: PhantomData,
            instance,
            last_yield,
        }
    }

    pub fn make_step_circuit(
        &self,
        i: usize,
        rm: &mut M::Allocator,
        cs: ConstraintSystemRef<F>,
        mut irw: InterRoundWires,
    ) -> Result<
        (
            InterRoundWires,
            <M::Allocator as IVCMemoryAllocated<F>>::FinishStepPayload,
        ),
        SynthesisError,
    > {
        rm.start_step(cs.clone()).unwrap();

        let _guard = tracing::info_span!("make_step_circuit", i = i, op = ?self.ops[i]).entered();

        let wires_in = self.allocate_vars(i, rm, &irw)?;
        let next_wires = wires_in.clone();

        // per opcode constraints
        let next_wires = self.visit_yield(next_wires)?;
        let next_wires = self.visit_resume(next_wires)?;
        let next_wires = self.visit_burn(next_wires)?;
        let next_wires = self.visit_program_hash(next_wires)?;
        let next_wires = self.visit_new_process(next_wires)?;
        let next_wires = self.visit_activation(next_wires)?;
        let next_wires = self.visit_init(next_wires)?;
        let next_wires = self.visit_bind(next_wires)?;
        let next_wires = self.visit_unbind(next_wires)?;
        let next_wires = self.visit_new_ref(next_wires)?;
        let next_wires = self.visit_ref_push(next_wires)?;
        let next_wires = self.visit_get_ref(next_wires)?;
        let next_wires = self.visit_install_handler(next_wires)?;
        let next_wires = self.visit_uninstall_handler(next_wires)?;
        let next_wires = self.visit_get_handler_for(next_wires)?;

        let mem_step_data = rm.finish_step(i == self.ops.len() - 1)?;

        // input <-> output mappings are done by modifying next_wires
        ivcify_wires(&cs, &wires_in, &next_wires)?;

        // Enforce global invariant: If building ref, must be RefPush
        let is_building = wires_in.ref_building_remaining.is_zero()?.not();
        is_building.enforce_equal(&wires_in.switches.ref_push)?;

        irw.update(next_wires);

        tracing::debug!("constraints: {}", cs.num_constraints());

        Ok((irw, mem_step_data))
    }

    pub fn trace_memory_ops(&mut self, params: <M as memory::IVCMemory<F>>::Params) -> M {
        // initialize all the maps
        let mut mb = {
            let mut mb = M::new(params);

            register_memory_segments(&mut mb);

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

        // Initialize handler memory using simplified approach
        self.init_handler_memory(&mut mb);

        // out of circuit memory operations.
        // this is needed to commit to the memory operations before-hand.
        //
        // and here we compute the actual write values (for memory operations)
        //
        // note however that we don't enforce/check anything, that's done in the
        // circuit constraints

        // Initialize IRW for the trace phase and update it as we process each operation
        let mut irw = InterRoundWires::new(
            F::from(self.p_len() as u64),
            self.instance.entrypoint.0 as u64,
        );

        for instr in &self.ops {
            let config = instr.get_config(&irw);

            let curr_switches = config.mem_switches_curr;
            let target_switches = config.mem_switches_target;
            let rom_switches = config.rom_switches;
            let handler_switches = config.handler_switches;

            self.mem_switches
                .push((curr_switches.clone(), target_switches.clone()));

            self.rom_switches.push(rom_switches.clone());
            self.handler_switches.push(handler_switches.clone());

            let target_addr = match instr {
                LedgerOperation::Resume { target, .. } => Some(*target),
                LedgerOperation::Yield { .. } => irw.id_prev_is_some.then_some(irw.id_prev_value),
                LedgerOperation::Burn { .. } => irw.id_prev_is_some.then_some(irw.id_prev_value),
                LedgerOperation::NewUtxo { target: id, .. } => Some(*id),
                LedgerOperation::NewCoord { target: id, .. } => Some(*id),
                LedgerOperation::ProgramHash { target, .. } => Some(*target),
                _ => None,
            };

            let curr_read =
                trace_program_state_reads(&mut mb, irw.id_curr.into_bigint().0[0], &curr_switches);

            let target_pid = target_addr.map(|t| t.into_bigint().0[0]);
            let target_read =
                trace_program_state_reads(&mut mb, target_pid.unwrap_or(0), &target_switches);

            // Trace ROM reads
            mb.conditional_read(
                rom_switches.read_is_utxo_curr,
                Address {
                    addr: irw.id_curr.into_bigint().0[0],
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
                    addr: irw.id_curr.into_bigint().0[0],
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

            trace_program_state_writes(
                &mut mb,
                irw.id_curr.into_bigint().0[0],
                &curr_write,
                &curr_switches,
            );
            trace_program_state_writes(
                &mut mb,
                target_pid.unwrap_or(0),
                &target_write,
                &target_switches,
            );

            // update pids for next iteration
            match instr {
                LedgerOperation::Resume { target, .. } => {
                    irw.id_prev_is_some = true;
                    irw.id_prev_value = irw.id_curr;
                    irw.id_curr = *target;
                }
                LedgerOperation::Yield { .. } | LedgerOperation::Burn { .. } => {
                    irw.id_curr = irw.id_prev_value;
                    irw.id_prev_is_some = true;
                    irw.id_prev_value = irw.id_curr;
                }
                _ => {}
            }
        }

        let mut ref_building_id = F::ZERO;
        let mut ref_building_offset = F::ZERO;

        for instr in &self.ops {
            match instr {
                LedgerOperation::NewRef { size: _, ret } => {
                    ref_building_id = *ret;
                    ref_building_offset = F::ZERO;
                }
                LedgerOperation::RefPush { val } => {
                    let addr =
                        ref_building_id.into_bigint().0[0] + ref_building_offset.into_bigint().0[0];

                    mb.conditional_write(
                        true,
                        Address {
                            tag: MemoryTag::RefArena.into(),
                            addr,
                        },
                        vec![*val],
                    );

                    ref_building_offset += F::ONE;
                }
                LedgerOperation::Get {
                    reff,
                    offset,
                    ret: _,
                } => {
                    let addr = reff.into_bigint().0[0] + offset.into_bigint().0[0];
                    mb.conditional_read(
                        true,
                        Address {
                            tag: MemoryTag::RefArena.into(),
                            addr,
                        },
                    );
                }
                _ => {}
            }
        }

        // Handler stack memory operations - always perform for uniform circuit
        self.trace_handler_stack_mem_opcodes(&mut mb);

        let current_steps = self.ops.len();
        if let Some(missing) = mb.required_steps().checked_sub(current_steps) {
            tracing::debug!("padding with {missing} Nop operations for scan");
            self.ops
                .extend(std::iter::repeat_n(LedgerOperation::Nop {}, missing));
        }

        mb
    }

    fn init_handler_memory(&self, mem: &mut M) {
        let interfaces = self.interface_resolver.interfaces();

        // Initialize Interfaces ROM and HandlerStackHeads
        for (index, interface_id) in interfaces.iter().enumerate() {
            mem.init(
                Address {
                    addr: index as u64,
                    tag: MemoryTag::Interfaces.into(),
                },
                vec![*interface_id],
            );

            mem.init(
                Address {
                    addr: index as u64,
                    tag: MemoryTag::HandlerStackHeads.into(),
                },
                vec![F::ZERO], // null pointer (empty stack)
            );
        }

        // Pre-allocate arena nodes for all InstallHandler operations
        let install_count = self
            .ops
            .iter()
            .filter(|op| matches!(op, LedgerOperation::InstallHandler { .. }))
            .count();

        for i in 0..install_count {
            mem.init(
                Address {
                    addr: i as u64,
                    tag: MemoryTag::HandlerStackArenaProcess.into(),
                },
                vec![F::ZERO], // process_id
            );
            mem.init(
                Address {
                    addr: i as u64,
                    tag: MemoryTag::HandlerStackArenaNextPtr.into(),
                },
                vec![F::ZERO], // next_ptr
            );
        }
    }

    fn trace_handler_stack_mem_opcodes(&mut self, mb: &mut M) {
        let mut irw = InterRoundWires::new(
            F::from(self.p_len() as u64),
            self.instance.entrypoint.0 as u64,
        );

        for instr in self.ops.iter() {
            let config = instr.get_config(&irw);

            // Get interface index for handler operations
            let interface_index = match instr {
                LedgerOperation::InstallHandler { interface_id } => self
                    .interface_resolver
                    .get_interface_index_field(*interface_id),
                LedgerOperation::UninstallHandler { interface_id } => self
                    .interface_resolver
                    .get_interface_index_field(*interface_id),
                LedgerOperation::GetHandlerFor { interface_id, .. } => self
                    .interface_resolver
                    .get_interface_index_field(*interface_id),
                _ => F::ZERO,
            };

            // Trace handler memory operations directly
            mb.conditional_read(
                config.handler_switches.read_interface,
                Address {
                    tag: MemoryTag::Interfaces.into(),
                    addr: interface_index.into_bigint().0[0],
                },
            );

            let current_head = mb.conditional_read(
                config.handler_switches.read_head,
                Address {
                    tag: MemoryTag::HandlerStackHeads.into(),
                    addr: interface_index.into_bigint().0[0],
                },
            )[0];

            let _node_process = mb.conditional_read(
                config.handler_switches.read_node,
                Address {
                    tag: MemoryTag::HandlerStackArenaProcess.into(),
                    addr: current_head.into_bigint().0[0],
                },
            )[0];

            let node_next = mb.conditional_read(
                config.handler_switches.read_node,
                Address {
                    tag: MemoryTag::HandlerStackArenaNextPtr.into(),
                    addr: current_head.into_bigint().0[0],
                },
            )[0];

            mb.conditional_write(
                config.handler_switches.write_node,
                Address {
                    tag: MemoryTag::HandlerStackArenaProcess.into(),
                    addr: irw.handler_stack_counter.into_bigint().0[0],
                },
                if config.handler_switches.write_node {
                    vec![irw.id_curr]
                } else {
                    vec![F::ZERO]
                },
            );

            mb.conditional_write(
                config.handler_switches.write_node,
                Address {
                    tag: MemoryTag::HandlerStackArenaNextPtr.into(),
                    addr: irw.handler_stack_counter.into_bigint().0[0],
                },
                if config.handler_switches.write_node {
                    vec![current_head]
                } else {
                    vec![F::ZERO]
                },
            );

            mb.conditional_write(
                config.handler_switches.write_head,
                Address {
                    tag: MemoryTag::HandlerStackHeads.into(),
                    addr: interface_index.into_bigint().0[0],
                },
                vec![if config.handler_switches.write_node {
                    irw.handler_stack_counter
                } else if config.handler_switches.write_head {
                    node_next
                } else {
                    F::ZERO
                }],
            );

            // Update IRW for next iteration if this is install_handler
            if config.execution_switches.install_handler {
                irw.handler_stack_counter += F::ONE;
            }
        }
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
        let handler_switches = &self.handler_switches[i];

        // Compute interface index for handler operations
        let interface_index = match instruction {
            LedgerOperation::InstallHandler { interface_id } => self
                .interface_resolver
                .get_interface_index_field(*interface_id),
            LedgerOperation::UninstallHandler { interface_id } => self
                .interface_resolver
                .get_interface_index_field(*interface_id),
            LedgerOperation::GetHandlerFor { interface_id, .. } => self
                .interface_resolver
                .get_interface_index_field(*interface_id),
            _ => F::ZERO,
        };

        let default = PreWires::new(
            irw.clone(),
            curr_mem_switches.clone(),
            target_mem_switches.clone(),
            rom_switches.clone(),
            handler_switches.clone(),
            interface_index,
        );

        match instruction {
            LedgerOperation::Nop {} => {
                let irw = PreWires {
                    switches: ExecutionSwitches::nop(),
                    ..default
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
                    switches: ExecutionSwitches::resume(),
                    target: *target,
                    val: *val,
                    ret: *ret,
                    id_prev_is_some: id_prev.is_some(),
                    id_prev_value: id_prev.unwrap_or_default(),
                    ..default
                };

                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Yield { val, ret, id_prev } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::yield_op(),
                    target: irw.id_prev_value,
                    val: *val,
                    ret: ret.unwrap_or_default(),
                    ret_is_some: ret.is_some(),
                    id_prev_is_some: id_prev.is_some(),
                    id_prev_value: id_prev.unwrap_or_default(),
                    ..default
                };

                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Burn { ret } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::burn(),
                    target: irw.id_prev_value,
                    ret: *ret,
                    id_prev_is_some: irw.id_prev_is_some,
                    id_prev_value: irw.id_prev_value,
                    ..default
                };

                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::ProgramHash {
                target,
                program_hash,
            } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::program_hash(),
                    target: *target,
                    program_hash: *program_hash,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::NewUtxo {
                program_hash,
                val,
                target,
            } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::new_utxo(),
                    target: *target,
                    val: *val,
                    program_hash: *program_hash,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::NewCoord {
                program_hash,
                val,
                target,
            } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::new_coord(),
                    target: *target,
                    val: *val,
                    program_hash: *program_hash,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Activation { val, caller } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::activation(),
                    val: *val,
                    caller: *caller,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Init { val, caller } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::init(),
                    val: *val,
                    caller: *caller,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Bind { owner_id } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::bind(),
                    target: *owner_id,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Unbind { token_id } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::unbind(),
                    target: *token_id,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::NewRef { size, ret } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::new_ref(),
                    size: *size,
                    ret: *ret,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::RefPush { val } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::ref_push(),
                    val: *val,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::Get { reff, offset, ret } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::get(),
                    val: *reff,
                    offset: *offset,
                    ret: *ret,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::InstallHandler { interface_id } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::install_handler(),
                    val: *interface_id,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::UninstallHandler { interface_id } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::uninstall_handler(),
                    val: *interface_id,
                    ..default
                };
                Wires::from_irw(&irw, rm, curr_write, target_write)
            }
            LedgerOperation::GetHandlerFor {
                interface_id,
                handler_id,
            } => {
                let irw = PreWires {
                    switches: ExecutionSwitches::get_handler_for(),
                    val: *interface_id,
                    ret: *handler_id,
                    ..default
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
            .conditional_enforce_equal(&wires.constant_true, switch)?;

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
            .conditional_enforce_equal(&wires.constant_false, &switch)?;

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

        // 1. Must not be building
        wires
            .ref_building_remaining
            .is_zero()?
            .conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // 2. Ret must be fresh ID
        wires
            .ret
            .conditional_enforce_equal(&wires.ref_arena_stack_ptr, switch)?;

        // 3. Init building state
        // remaining = size
        wires.ref_building_remaining = switch.select(&wires.size, &wires.ref_building_remaining)?;
        // ptr = ret
        wires.ref_building_ptr = switch.select(&wires.ret, &wires.ref_building_ptr)?;

        // 4. Increment stack ptr by size
        wires.ref_arena_stack_ptr = switch.select(
            &(&wires.ref_arena_stack_ptr + &wires.size),
            &wires.ref_arena_stack_ptr,
        )?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_ref_push(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.ref_push;

        let is_building = wires.ref_building_remaining.is_zero()?.not();
        is_building.conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // Update state
                // remaining -= 1
                let next_remaining = &wires.ref_building_remaining - &wires.constant_one;
                wires.ref_building_remaining = switch.select(&next_remaining, &wires.ref_building_remaining)?;
        
                // ptr += 1
                let next_ptr = &wires.ref_building_ptr + &wires.constant_one;
                wires.ref_building_ptr = switch.select(&next_ptr, &wires.ref_building_ptr)?;
        
                Ok(wires)
            }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_get_ref(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.get;

        wires
            .ret
            .conditional_enforce_equal(&wires.ref_arena_read, switch)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_program_hash(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.program_hash;

        // Check that the program hash from the opcode matches the ROM lookup result
        wires
            .program_hash
            .conditional_enforce_equal(&wires.rom_program_hash, switch)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_install_handler(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.install_handler;

        // Only coordination scripts can install handlers
        wires
            .is_utxo_curr
            .is_one()?
            .conditional_enforce_equal(&Boolean::FALSE, switch)?;

        // Verify that Interfaces[interface_index] == interface_id
        // This ensures the interface index witness is correct
        wires
            .handler_state
            .interface_rom_read
            .conditional_enforce_equal(&wires.val, switch)?;

        // Update handler stack counter (allocate new node)
        wires.handler_stack_ptr = switch.select(
            &(&wires.handler_stack_ptr + &wires.constant_one),
            &wires.handler_stack_ptr,
        )?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_uninstall_handler(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.uninstall_handler;

        // Only coordination scripts can uninstall handlers
        wires
            .is_utxo_curr
            .is_one()?
            .conditional_enforce_equal(&Boolean::FALSE, switch)?;

        // Verify that Interfaces[interface_index] == interface_id
        // This ensures the interface index witness is correct
        wires
            .handler_state
            .interface_rom_read
            .conditional_enforce_equal(&wires.val, switch)?;

        // Read the node at current head: should contain (process_id, next_ptr)
        let node_process = &wires.handler_state.handler_stack_node_process;

        // Verify the process_id in the node matches the current process (only installer can uninstall)
        wires
            .id_curr
            .conditional_enforce_equal(node_process, switch)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_get_handler_for(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.get_handler_for;

        // Verify that Interfaces[interface_index] == interface_id
        // This ensures the interface index witness is correct
        wires
            .handler_state
            .interface_rom_read
            .conditional_enforce_equal(&wires.val, switch)?;

        // Read the node at current head: should contain (process_id, next_ptr)
        let node_process = &wires.handler_state.handler_stack_node_process;

        // The process_id in the node IS the handler_id we want to return
        wires.ret.conditional_enforce_equal(node_process, switch)?;

        Ok(wires)
    }

    pub(crate) fn p_len(&self) -> usize {
        self.instance.process_table.len()
    }
}

fn register_memory_segments<M: IVCMemory<F>>(mb: &mut M) {
    mb.register_mem(
        MemoryTag::ProcessTable.into(),
        1,
        MemType::Rom,
        "ROM_PROCESS_TABLE",
    );
    mb.register_mem(MemoryTag::MustBurn.into(), 1, MemType::Rom, "ROM_MUST_BURN");
    mb.register_mem_with_lanes(
        MemoryTag::IsUtxo.into(),
        1,
        MemType::Rom,
        Lanes(2),
        "ROM_IS_UTXO",
    );
    mb.register_mem(
        MemoryTag::Interfaces.into(),
        1,
        MemType::Rom,
        "ROM_INTERFACES",
    );

    mb.register_mem(MemoryTag::RefArena.into(), 1, MemType::Ram, "RAM_REF_ARENA");

    mb.register_mem_with_lanes(
        MemoryTag::ExpectedInput.into(),
        1,
        MemType::Ram,
        Lanes(2),
        "RAM_EXPECTED_INPUT",
    );
    mb.register_mem_with_lanes(
        MemoryTag::Activation.into(),
        1,
        MemType::Ram,
        Lanes(2),
        "RAM_ACTIVATION",
    );
    mb.register_mem_with_lanes(
        MemoryTag::Init.into(),
        1,
        MemType::Ram,
        Lanes(2),
        "RAM_INIT",
    );
    mb.register_mem_with_lanes(
        MemoryTag::Counters.into(),
        1,
        MemType::Ram,
        Lanes(2),
        "RAM_COUNTERS",
    );
    mb.register_mem_with_lanes(
        MemoryTag::Initialized.into(),
        1,
        MemType::Ram,
        Lanes(2),
        "RAM_INITIALIZED",
    );
    mb.register_mem_with_lanes(
        MemoryTag::Finalized.into(),
        1,
        MemType::Ram,
        Lanes(2),
        "RAM_FINALIZED",
    );
    mb.register_mem_with_lanes(
        MemoryTag::DidBurn.into(),
        1,
        MemType::Ram,
        Lanes(2),
        "RAM_DID_BURN",
    );
    mb.register_mem_with_lanes(
        MemoryTag::Ownership.into(),
        1,
        MemType::Ram,
        Lanes(2),
        "RAM_OWNERSHIP",
    );
    mb.register_mem(
        MemoryTag::HandlerStackArenaProcess.into(),
        1,
        MemType::Ram,
        "RAM_HANDLER_STACK_ARENA_PROCESS",
    );
    mb.register_mem(
        MemoryTag::HandlerStackArenaNextPtr.into(),
        1,
        MemType::Ram,
        "RAM_HANDLER_STACK_ARENA_NEXT_PTR",
    );
    mb.register_mem(
        MemoryTag::HandlerStackHeads.into(),
        1,
        MemType::Ram,
        "RAM_HANDLER_STACK_HEADS",
    );
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
        handler_switches: HandlerSwitchboard,
        interface_index: F,
    ) -> Self {
        Self {
            switches: ExecutionSwitches::default(),
            irw,
            target: F::ZERO,
            val: F::ZERO,
            ret: F::ZERO,
            offset: F::ZERO,
            size: F::ZERO,
            program_hash: F::ZERO,
            caller: F::ZERO,
            interface_index,
            ret_is_some: false,
            id_prev_is_some: false,
            id_prev_value: F::ZERO,
            curr_mem_switches,
            target_mem_switches,
            rom_switches,
            handler_switches,
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

impl HandlerSwitchboardWires {
    pub fn allocate(
        cs: ConstraintSystemRef<F>,
        switches: &HandlerSwitchboard,
    ) -> Result<Self, SynthesisError> {
        Ok(Self {
            read_interface: Boolean::new_witness(cs.clone(), || Ok(switches.read_interface))?,
            read_head: Boolean::new_witness(cs.clone(), || Ok(switches.read_head))?,
            read_node: Boolean::new_witness(cs.clone(), || Ok(switches.read_node))?,
            write_node: Boolean::new_witness(cs.clone(), || Ok(switches.write_node))?,
            write_head: Boolean::new_witness(cs.clone(), || Ok(switches.write_head))?,
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
