use crate::abi::{self, ArgName, OPCODE_ARG_COUNT};
use crate::ledger_operation::{REF_GET_BATCH_SIZE, REF_PUSH_BATCH_SIZE};
use crate::memory::{self, Address, IVCMemory, MemType};
pub use crate::memory_tags::MemoryTag;
use crate::program_state::{
    ProgramState, ProgramStateWires, program_state_read_wires, program_state_write_wires,
    trace_program_state_reads, trace_program_state_writes,
};
use crate::handler_stack_gadget::{handler_stack_access_wires, trace_handler_stack_ops};
use crate::ref_arena_gadget::{ref_arena_access_wires, ref_arena_read_size, trace_ref_arena_ops};
use crate::switchboard::{
    HandlerSwitchboard, HandlerSwitchboardWires, MemSwitchboard, MemSwitchboardWires,
    RomSwitchboard, RomSwitchboardWires,
};
use crate::{
    F, OptionalF, OptionalFpVar, ledger_operation::LedgerOperation, memory::IVCMemoryAllocated,
};
use ark_ff::{AdditiveGroup, Field as _, PrimeField};
use ark_r1cs_std::fields::FieldVar;
use ark_r1cs_std::{
    GR1CSVar as _, alloc::AllocVar as _, eq::EqGadget, fields::fp::FpVar, prelude::Boolean,
};
use ark_relations::{
    gr1cs::{ConstraintSystemRef, LinearCombination, SynthesisError, Variable},
    ns,
};
use starstream_interleaving_spec::{EffectDiscriminant, InterleavingInstance};
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

struct OpcodeConfig {
    mem_switches_curr: MemSwitchboard,
    mem_switches_target: MemSwitchboard,
    rom_switches: RomSwitchboard,
    handler_switches: HandlerSwitchboard,
    execution_switches: ExecutionSwitches<bool>,
    opcode_args: [F; OPCODE_ARG_COUNT],
    opcode_discriminant: F,
}

#[derive(Clone)]
pub(crate) struct ExecutionSwitches<T> {
    pub(crate) resume: T,
    pub(crate) yield_op: T,
    pub(crate) burn: T,
    pub(crate) program_hash: T,
    pub(crate) new_utxo: T,
    pub(crate) new_coord: T,
    pub(crate) activation: T,
    pub(crate) init: T,
    pub(crate) bind: T,
    pub(crate) unbind: T,
    pub(crate) new_ref: T,
    pub(crate) ref_push: T,
    pub(crate) get: T,
    pub(crate) ref_write: T,
    pub(crate) install_handler: T,
    pub(crate) uninstall_handler: T,
    pub(crate) get_handler_for: T,
    pub(crate) nop: T,
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

    fn ref_write() -> Self {
        Self {
            ref_write: true,
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
        opcode_discriminant: &FpVar<F>,
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
            self.ref_write,
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
            ref_write,
            install_handler,
            uninstall_handler,
            get_handler_for,
        ] = allocated_switches.as_slice()
        else {
            unreachable!()
        };

        let terms = [
            (resume, EffectDiscriminant::Resume as u64),
            (yield_op, EffectDiscriminant::Yield as u64),
            (burn, EffectDiscriminant::Burn as u64),
            (program_hash, EffectDiscriminant::ProgramHash as u64),
            (new_utxo, EffectDiscriminant::NewUtxo as u64),
            (new_coord, EffectDiscriminant::NewCoord as u64),
            (activation, EffectDiscriminant::Activation as u64),
            (init, EffectDiscriminant::Init as u64),
            (bind, EffectDiscriminant::Bind as u64),
            (unbind, EffectDiscriminant::Unbind as u64),
            (new_ref, EffectDiscriminant::NewRef as u64),
            (ref_push, EffectDiscriminant::RefPush as u64),
            (get, EffectDiscriminant::RefGet as u64),
            (ref_write, EffectDiscriminant::RefWrite as u64),
            (install_handler, EffectDiscriminant::InstallHandler as u64),
            (
                uninstall_handler,
                EffectDiscriminant::UninstallHandler as u64,
            ),
            (get_handler_for, EffectDiscriminant::GetHandlerFor as u64),
        ];

        let expected_opcode = terms.iter().fold(FpVar::zero(), |acc, (switch, disc)| {
            acc + FpVar::from((*switch).clone()) * F::from(*disc)
        });

        expected_opcode.enforce_equal(opcode_discriminant)?;

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
            ref_write: ref_write.clone(),
            install_handler: install_handler.clone(),
            uninstall_handler: uninstall_handler.clone(),
            get_handler_for: get_handler_for.clone(),
        })
    }
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
            ref_write: false,
            install_handler: false,
            uninstall_handler: false,
            get_handler_for: false,
            nop: false,
        }
    }
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

// OptionalF/OptionalFpVar live in optional.rs

/// common circuit variables to all the opcodes
#[derive(Clone)]
pub struct Wires {
    // irw
    id_curr: FpVar<F>,
    id_prev: OptionalFpVar<F>,
    ref_arena_stack_ptr: FpVar<F>,
    handler_stack_ptr: FpVar<F>,

    ref_building_remaining: FpVar<F>,
    ref_building_ptr: FpVar<F>,

    switches: ExecutionSwitches<Boolean<F>>,

    opcode_args: [FpVar<F>; OPCODE_ARG_COUNT],
    ret_is_some: Boolean<F>,

    curr_read_wires: ProgramStateWires,
    curr_write_wires: ProgramStateWires,

    target_read_wires: ProgramStateWires,
    target_write_wires: ProgramStateWires,

    ref_arena_read: [FpVar<F>; REF_GET_BATCH_SIZE],
    handler_state: HandlerState,

    // ROM lookup results
    is_utxo_curr: FpVar<F>,
    is_utxo_target: FpVar<F>,
    must_burn_curr: FpVar<F>,
    rom_program_hash: [FpVar<F>; 4],

    constant_false: Boolean<F>,
    constant_true: Boolean<F>,
}

// helper so that we always allocate witnesses in the same order
pub struct PreWires {
    interface_index: F,

    switches: ExecutionSwitches<bool>,

    opcode_args: [F; OPCODE_ARG_COUNT],
    opcode_discriminant: F,

    curr_mem_switches: MemSwitchboard,
    target_mem_switches: MemSwitchboard,
    rom_switches: RomSwitchboard,
    handler_switches: HandlerSwitchboard,

    irw: InterRoundWires,
    ret_is_some: bool,
}

/// IVC wires (state between steps)
///
/// these get input and output variables
#[derive(Clone)]
pub struct InterRoundWires {
    id_curr: F,
    id_prev: OptionalF<F>,
    ref_arena_counter: F,
    handler_stack_counter: F,

    ref_building_remaining: F,
    ref_building_ptr: F,
}

#[derive(Clone, Copy, Debug)]
pub struct IvcWireIndices {
    pub id_curr: usize,
    pub id_prev: usize,
    pub ref_arena_stack_ptr: usize,
    pub handler_stack_ptr: usize,
    pub ref_building_remaining: usize,
    pub ref_building_ptr: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct IvcWireLayout {
    pub input: IvcWireIndices,
    pub output: IvcWireIndices,
}

impl IvcWireLayout {
    pub const FIELD_COUNT: usize = 6;

    pub fn input_indices(&self) -> [usize; Self::FIELD_COUNT] {
        [
            self.input.id_curr,
            self.input.id_prev,
            self.input.ref_arena_stack_ptr,
            self.input.handler_stack_ptr,
            self.input.ref_building_remaining,
            self.input.ref_building_ptr,
        ]
    }

    pub fn output_indices(&self) -> [usize; Self::FIELD_COUNT] {
        [
            self.output.id_curr,
            self.output.id_prev,
            self.output.ref_arena_stack_ptr,
            self.output.handler_stack_ptr,
            self.output.ref_building_remaining,
            self.output.ref_building_ptr,
        ]
    }
}

impl Wires {
    fn arg(&self, kind: ArgName) -> FpVar<F> {
        self.opcode_args[kind.idx()].clone()
    }

    fn id_prev_is_some(&self) -> Result<Boolean<F>, SynthesisError> {
        self.id_prev.is_some()
    }

    fn id_prev_value(&self) -> Result<FpVar<F>, SynthesisError> {
        self.id_prev.decode_or_zero()
    }

    // IMPORTANT: no rust branches in this function, since the purpose of this
    // is to get the exact same layout for all the opcodes
    #[tracing::instrument(target = "gr1cs", skip_all)]
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
        let id_prev = OptionalFpVar::new(FpVar::new_witness(cs.clone(), || {
            Ok(vals.irw.id_prev.encoded())
        })?);
        let ref_arena_stack_ptr =
            FpVar::new_witness(cs.clone(), || Ok(vals.irw.ref_arena_counter))?;
        let handler_stack_counter =
            FpVar::new_witness(cs.clone(), || Ok(vals.irw.handler_stack_counter))?;

        let ref_building_remaining =
            FpVar::new_witness(cs.clone(), || Ok(vals.irw.ref_building_remaining))?;
        let ref_building_ptr = FpVar::new_witness(cs.clone(), || Ok(vals.irw.ref_building_ptr))?;

        let opcode_args_cs = ns!(cs.clone(), "opcode_args");
        let opcode_args_vec = (0..OPCODE_ARG_COUNT)
            .map(|i| FpVar::<F>::new_witness(opcode_args_cs.clone(), || Ok(vals.opcode_args[i])))
            .collect::<Result<Vec<_>, _>>()?;
        let opcode_args: [FpVar<F>; OPCODE_ARG_COUNT] =
            opcode_args_vec.try_into().expect("opcode args length");
        let opcode_discriminant =
            FpVar::<F>::new_witness(cs.clone(), || Ok(vals.opcode_discriminant))?;

        // Allocate switches and enforce exactly one is true
        let switches = vals
            .switches
            .allocate_and_constrain(cs.clone(), &opcode_discriminant)?;

        let constant_false = Boolean::new_constant(cs.clone(), false)?;

        let target = opcode_args[ArgName::Target.idx()].clone();
        let val = opcode_args[ArgName::Val.idx()].clone();
        let offset = opcode_args[ArgName::Offset.idx()].clone();

        let ret_is_some = Boolean::new_witness(cs.clone(), || Ok(vals.ret_is_some))?;

        let curr_mem_switches = MemSwitchboardWires::allocate(cs.clone(), &vals.curr_mem_switches)?;
        let target_mem_switches =
            MemSwitchboardWires::allocate(cs.clone(), &vals.target_mem_switches)?;

        let curr_address = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.irw.id_curr))?;
        let curr_read_wires =
            program_state_read_wires(rm, &cs, curr_address.clone(), &curr_mem_switches)?;

        let id_prev_value = id_prev.decode_or_zero()?;
        let target_address = switches.yield_op.select(&id_prev_value, &target)?;
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

        let mut rom_program_hash_vec = Vec::with_capacity(4);
        let process_table_stride = FpVar::new_constant(cs.clone(), F::from(4))?;
        for i in 0..4 {
            let offset = FpVar::new_constant(cs.clone(), F::from(i as u64))?;
            let addr = &target_address * &process_table_stride + offset;
            let value = rm.conditional_read(
                &rom_switches.read_program_hash_target,
                &Address {
                    addr,
                    tag: MemoryTag::ProcessTable.allocate(cs.clone())?,
                },
            )?[0]
                .clone();
            rom_program_hash_vec.push(value);
        }
        let rom_program_hash: [FpVar<F>; 4] = rom_program_hash_vec
            .try_into()
            .expect("rom program hash length");

        let handler_switches =
            HandlerSwitchboardWires::allocate(cs.clone(), &vals.handler_switches)?;

        let interface_index_var = FpVar::new_witness(cs.clone(), || Ok(vals.interface_index))?;

        let handler_reads = handler_stack_access_wires(
            cs.clone(),
            rm,
            &handler_switches,
            &interface_index_var,
            &handler_stack_counter,
            &id_curr,
        )?;

        let handler_state = HandlerState {
            handler_stack_node_process: handler_reads.handler_stack_node_process,
            interface_rom_read: handler_reads.interface_rom_read,
        };

        // ref arena wires
        let ref_arena_read = {
            let ref_size_read = ref_arena_read_size(cs.clone(), rm, &switches, &opcode_args, &val)?;

            ref_arena_access_wires(
                cs.clone(),
                rm,
                &switches,
                &opcode_args,
                &ref_building_ptr,
                &ref_building_remaining,
                &val,
                &offset,
                &ref_size_read,
            )?
        };

        let should_trace = switches.nop.clone().not();
        trace_ic_wires(
            id_curr.clone(),
            rm,
            &cs,
            &should_trace,
            &opcode_discriminant,
            &opcode_args,
        )?;

        Ok(Wires {
            id_curr,
            id_prev,
            ref_arena_stack_ptr,
            handler_stack_ptr: handler_stack_counter,

            ref_building_remaining,
            ref_building_ptr,
            switches,

            constant_false,
            constant_true: Boolean::new_constant(cs.clone(), true)?,

            // wit_wires
            opcode_args,
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

impl InterRoundWires {
    pub fn new(entrypoint: u64) -> Self {
        InterRoundWires {
            id_curr: F::from(entrypoint),
            id_prev: OptionalF::none(),
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

        let res_id_prev = res.id_prev.value().unwrap();
        tracing::debug!(
            "prev_program from {} to {}",
            self.id_prev.encoded(),
            res_id_prev
        );

        self.id_prev = OptionalF::from_encoded(res_id_prev);

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
    fn get_config(&self) -> OpcodeConfig {
        let mut config = OpcodeConfig {
            mem_switches_curr: MemSwitchboard::default(),
            mem_switches_target: MemSwitchboard::default(),
            rom_switches: RomSwitchboard::default(),
            handler_switches: HandlerSwitchboard::default(),
            execution_switches: ExecutionSwitches::default(),
            opcode_args: [F::ZERO; OPCODE_ARG_COUNT],
            opcode_discriminant: F::ZERO,
        };

        // All ops increment counter of the current process, except Nop
        config.mem_switches_curr.counters = !matches!(self, LedgerOperation::Nop {});

        config.opcode_discriminant = abi::opcode_discriminant(self);

        match self {
            LedgerOperation::Nop {} => {
                config.execution_switches.nop = true;
            }
            LedgerOperation::Resume { .. } => {
                config.execution_switches.resume = true;

                config.mem_switches_curr.activation = true;
                config.mem_switches_curr.expected_input = true;
                config.mem_switches_curr.expected_resumer = true;

                config.mem_switches_target.activation = true;
                config.mem_switches_target.expected_input = true;
                config.mem_switches_target.expected_resumer = true;
                config.mem_switches_target.finalized = true;
                config.mem_switches_target.initialized = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;
            }
            LedgerOperation::Yield { ret: _ret, .. } => {
                config.execution_switches.yield_op = true;

                config.mem_switches_curr.activation = true;
                config.mem_switches_curr.expected_input = true;
                config.mem_switches_curr.expected_resumer = true;
                config.mem_switches_curr.finalized = true;

                config.mem_switches_target.expected_input = true;
                config.mem_switches_target.expected_resumer = true;
            }
            LedgerOperation::Burn { .. } => {
                config.execution_switches.burn = true;

                config.mem_switches_curr.activation = true;
                config.mem_switches_curr.finalized = true;
                config.mem_switches_curr.did_burn = true;
                config.mem_switches_curr.expected_input = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_must_burn_curr = true;
            }
            LedgerOperation::ProgramHash { .. } => {
                config.execution_switches.program_hash = true;

                config.rom_switches.read_program_hash_target = true;
            }
            LedgerOperation::NewUtxo { .. } => {
                config.execution_switches.new_utxo = true;

                config.mem_switches_target.initialized = true;
                config.mem_switches_target.init = true;
                config.mem_switches_target.counters = true;
                config.mem_switches_target.expected_input = true;
                config.mem_switches_target.expected_resumer = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;
                config.rom_switches.read_program_hash_target = true;
            }
            LedgerOperation::NewCoord { .. } => {
                config.execution_switches.new_coord = true;

                config.mem_switches_target.initialized = true;
                config.mem_switches_target.init = true;
                config.mem_switches_target.counters = true;
                config.mem_switches_target.expected_input = true;
                config.mem_switches_target.expected_resumer = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;
                config.rom_switches.read_program_hash_target = true;
            }
            LedgerOperation::Activation { .. } => {
                config.execution_switches.activation = true;

                config.mem_switches_curr.activation = true;
            }
            LedgerOperation::Init { .. } => {
                config.execution_switches.init = true;

                config.mem_switches_curr.init = true;
            }
            LedgerOperation::Bind { .. } => {
                config.execution_switches.bind = true;

                config.mem_switches_target.initialized = true;
                config.mem_switches_curr.ownership = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;
            }
            LedgerOperation::Unbind { .. } => {
                config.execution_switches.unbind = true;

                config.mem_switches_target.ownership = true;

                config.rom_switches.read_is_utxo_curr = true;
                config.rom_switches.read_is_utxo_target = true;
            }
            LedgerOperation::NewRef { .. } => {
                config.execution_switches.new_ref = true;
            }
            LedgerOperation::RefPush { .. } => {
                config.execution_switches.ref_push = true;
            }
            LedgerOperation::RefGet { .. } => {
                config.execution_switches.get = true;
            }
            LedgerOperation::RefWrite { .. } => {
                config.execution_switches.ref_write = true;
            }
            LedgerOperation::InstallHandler { .. } => {
                config.execution_switches.install_handler = true;
                config.rom_switches.read_is_utxo_curr = true;

                config.handler_switches.read_interface = true;
                config.handler_switches.read_head = true;
                config.handler_switches.write_node = true;
                config.handler_switches.write_head = true;
            }
            LedgerOperation::UninstallHandler { .. } => {
                config.execution_switches.uninstall_handler = true;
                config.rom_switches.read_is_utxo_curr = true;

                config.handler_switches.read_interface = true;
                config.handler_switches.read_head = true;
                config.handler_switches.read_node = true;
                config.handler_switches.write_head = true;
            }
            LedgerOperation::GetHandlerFor { .. } => {
                config.execution_switches.get_handler_for = true;

                config.handler_switches.read_interface = true;
                config.handler_switches.read_head = true;
                config.handler_switches.read_node = true;
            }
        }

        config.opcode_args = abi::opcode_args(self);

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
            LedgerOperation::Resume {
                val, ret, id_prev, ..
            } => {
                // Current process gives control to target.
                // It's `arg` is cleared, and its `expected_input` is set to the return value `ret`.
                curr_write.activation = F::ZERO; // Represents None
                curr_write.expected_input = OptionalF::new(*ret);
                curr_write.expected_resumer = *id_prev;

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
                id_prev,
                ..
            } => {
                // Current process yields control back to its parent (the target of this operation).
                // Its `arg` is cleared.
                curr_write.activation = F::ZERO; // Represents None
                if let Some(r) = ret {
                    // If Yield returns a value, it expects a new input `r` for the next resume.
                    curr_write.expected_input = OptionalF::new(*r);
                    curr_write.finalized = false;
                } else {
                    // If Yield does not return a value, it's a final yield for this UTXO.
                    curr_write.expected_input = OptionalF::none();
                    curr_write.finalized = true;
                }
                curr_write.expected_resumer = *id_prev;
            }
            LedgerOperation::Burn { ret } => {
                // The current UTXO is burned.
                curr_write.activation = F::ZERO; // Represents None
                curr_write.finalized = true;
                curr_write.did_burn = true;
                curr_write.expected_input = OptionalF::new(*ret); // Sets its final return value.
            }
            LedgerOperation::NewUtxo { val, target: _, .. }
            | LedgerOperation::NewCoord { val, target: _, .. } => {
                // The current process is a coordinator creating a new process.
                // The new process (target) is initialized.
                target_write.initialized = true;
                target_write.init = *val;
                target_write.counters = F::ZERO;
                target_write.expected_input = OptionalF::none();
                target_write.expected_resumer = OptionalF::none();
            }
            LedgerOperation::Bind { owner_id } => {
                curr_write.ownership = OptionalF::new(*owner_id);
            }
            LedgerOperation::Unbind { .. } => {
                target_write.ownership = OptionalF::none();
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
            .map(|v| abi::value_to_field(v.last_yield.clone()))
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
        compute_ivc_layout: bool,
    ) -> Result<
        (
            InterRoundWires,
            <M::Allocator as IVCMemoryAllocated<F>>::FinishStepPayload,
            Option<IvcWireLayout>,
        ),
        SynthesisError,
    > {
        rm.start_step(cs.clone()).unwrap();

        let _guard =
            tracing::info_span!("make_step_circuit", i = i, pid = ?irw.id_curr, op = ?self.ops[i])
                .entered();

        if !matches!(&self.ops[i], &LedgerOperation::Nop {}) {
            tracing::info!("synthesizing step");
        }

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
        let next_wires = self.visit_ref_get(next_wires)?;
        let next_wires = self.visit_ref_write(next_wires)?;
        let next_wires = self.visit_install_handler(next_wires)?;
        let next_wires = self.visit_uninstall_handler(next_wires)?;
        let next_wires = self.visit_get_handler_for(next_wires)?;

        let mem_step_data = rm.finish_step(i == self.ops.len() - 1)?;

        // input <-> output mappings are done by modifying next_wires
        let ivc_layout = if compute_ivc_layout {
            Some(ivc_wires(&cs, &wires_in, &next_wires)?)
        } else {
            None
        };

        // Enforce global invariant: If building ref, must be RefPush
        let is_building = wires_in.ref_building_remaining.is_zero()?.not();
        is_building.enforce_equal(&wires_in.switches.ref_push)?;

        irw.update(next_wires);

        Ok((irw, mem_step_data, ivc_layout))
    }

    pub fn trace_memory_ops(&mut self, params: <M as memory::IVCMemory<F>>::Params) -> M {
        // initialize all the maps
        let mut mb = {
            let mut mb = M::new(params);

            register_memory_segments(&mut mb);

            for (pid, mod_hash) in self.instance.process_table.iter().enumerate() {
                let hash_fields = abi::encode_hash_as_fields(*mod_hash);
                for (lane, field) in hash_fields.iter().enumerate() {
                    let addr = (pid * 4) + lane;
                    mb.init(
                        Address {
                            addr: addr as u64,
                            tag: MemoryTag::ProcessTable.into(),
                        },
                        vec![*field],
                    );
                }

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
                    vec![OptionalF::none().encoded()],
                );

                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::ExpectedResumer.into(),
                    },
                    vec![OptionalF::none().encoded()],
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

                for offset in 0..4 {
                    let addr = (pid * 4) + offset;
                    mb.init(
                        Address {
                            addr: addr as u64,
                            tag: MemoryTag::TraceCommitments.into(),
                        },
                        vec![F::ZERO],
                    );
                }
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
                let encoded_owner = owner
                    .map(|p| OptionalF::new(F::from(p.0 as u64)).encoded())
                    .unwrap_or_else(|| OptionalF::none().encoded());
                mb.init(
                    Address {
                        addr: pid as u64,
                        tag: MemoryTag::Ownership.into(),
                    },
                    vec![encoded_owner],
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
        let mut irw = InterRoundWires::new(self.instance.entrypoint.0 as u64);

        let mut ref_building_id = F::ZERO;
        let mut ref_building_offset = F::ZERO;
        let mut ref_building_remaining = F::ZERO;

        for instr in &self.ops {
            if !matches!(instr, LedgerOperation::Nop {}) {
                tracing::info!("mem tracing instr {:?}", &instr);
            }

            let config = instr.get_config();

            trace_ic(irw.id_curr.into_bigint().0[0] as usize, &mut mb, &config);

            let curr_switches = config.mem_switches_curr;
            let target_switches = config.mem_switches_target;
            let rom_switches = config.rom_switches;
            let handler_switches = config.handler_switches;

            self.mem_switches
                .push((curr_switches.clone(), target_switches.clone()));

            self.rom_switches.push(rom_switches.clone());
            self.handler_switches.push(handler_switches.clone());

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

            let _handler_reads = trace_handler_stack_ops(
                &mut mb,
                &handler_switches,
                &interface_index,
                &irw.handler_stack_counter,
                &irw.id_curr,
            );

            if config.execution_switches.install_handler {
                irw.handler_stack_counter += F::ONE;
            }

            trace_ref_arena_ops(
                &mut mb,
                &mut ref_building_id,
                &mut ref_building_offset,
                &mut ref_building_remaining,
                instr,
            );

            let target_addr = match instr {
                LedgerOperation::Resume { target, .. } => Some(*target),
                LedgerOperation::Yield { .. } => irw.id_prev.to_option(),
                LedgerOperation::Burn { .. } => irw.id_prev.to_option(),
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
            let target_pid_value = target_pid.unwrap_or(0);
            for lane in 0..4 {
                let addr = (target_pid_value * 4) + lane;
                mb.conditional_read(
                    rom_switches.read_program_hash_target,
                    Address {
                        addr: addr as u64,
                        tag: MemoryTag::ProcessTable.into(),
                    },
                );
            }

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
            trace_program_state_writes(&mut mb, target_pid_value, &target_write, &target_switches);

            // update pids for next iteration
            match instr {
                LedgerOperation::Resume { target, .. } => {
                    irw.id_prev = OptionalF::new(irw.id_curr);
                    irw.id_curr = *target;
                }
                LedgerOperation::Yield { .. } | LedgerOperation::Burn { .. } => {
                    let old_curr = irw.id_curr;
                    irw.id_curr = irw.id_prev.decode_or_zero();
                    irw.id_prev = OptionalF::new(old_curr);
                }
                _ => {}
            }

            mb.finish_step();
        }

        let current_steps = self.ops.len();
        if let Some(missing) = mb.required_steps().checked_sub(current_steps) {
            tracing::debug!("padding with {missing} Nop operations for scan");
            self.ops
                .extend(std::iter::repeat_n(LedgerOperation::Nop {}, missing));

            // TODO: we probably want to do this before the main loop
            for _ in 0..missing {
                mb.finish_step();
            }
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

        let mut default = PreWires::new(
            irw.clone(),
            curr_mem_switches.clone(),
            target_mem_switches.clone(),
            rom_switches.clone(),
            handler_switches.clone(),
            interface_index,
            abi::opcode_discriminant(instruction),
        );
        default.opcode_args = abi::opcode_args(instruction);

        let prewires = match instruction {
            LedgerOperation::Nop {} => PreWires {
                switches: ExecutionSwitches::nop(),
                ..default
            },
            LedgerOperation::Resume { .. } => PreWires {
                switches: ExecutionSwitches::resume(),
                ..default
            },
            LedgerOperation::Yield { ret, .. } => PreWires {
                switches: ExecutionSwitches::yield_op(),
                ret_is_some: ret.is_some(),
                ..default
            },
            LedgerOperation::Burn { .. } => PreWires {
                switches: ExecutionSwitches::burn(),
                ..default
            },
            LedgerOperation::ProgramHash { .. } => PreWires {
                switches: ExecutionSwitches::program_hash(),
                ..default
            },
            LedgerOperation::NewUtxo { .. } => PreWires {
                switches: ExecutionSwitches::new_utxo(),
                ..default
            },
            LedgerOperation::NewCoord { .. } => PreWires {
                switches: ExecutionSwitches::new_coord(),
                ..default
            },
            LedgerOperation::Activation { .. } => PreWires {
                switches: ExecutionSwitches::activation(),
                ..default
            },
            LedgerOperation::Init { .. } => PreWires {
                switches: ExecutionSwitches::init(),
                ..default
            },
            LedgerOperation::Bind { .. } => PreWires {
                switches: ExecutionSwitches::bind(),
                ..default
            },
            LedgerOperation::Unbind { .. } => PreWires {
                switches: ExecutionSwitches::unbind(),
                ..default
            },
            LedgerOperation::NewRef { .. } => PreWires {
                switches: ExecutionSwitches::new_ref(),
                ..default
            },
            LedgerOperation::RefPush { .. } => PreWires {
                switches: ExecutionSwitches::ref_push(),
                ..default
            },
            LedgerOperation::RefGet { .. } => PreWires {
                switches: ExecutionSwitches::get(),
                ..default
            },
            LedgerOperation::RefWrite { .. } => PreWires {
                switches: ExecutionSwitches::ref_write(),
                ..default
            },
            LedgerOperation::InstallHandler { .. } => PreWires {
                switches: ExecutionSwitches::install_handler(),
                ..default
            },
            LedgerOperation::UninstallHandler { .. } => PreWires {
                switches: ExecutionSwitches::uninstall_handler(),
                ..default
            },
            LedgerOperation::GetHandlerFor { .. } => PreWires {
                switches: ExecutionSwitches::get_handler_for(),
                ..default
            },
        };

        Wires::from_irw(&prewires, rm, curr_write, target_write)
    }
    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_resume(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.resume;

        // 1. self-resume check
        wires
            .id_curr
            .conditional_enforce_not_equal(&wires.arg(ArgName::Target), switch)?;

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

        // 5. Claim check: val passed in must match target's expected_input (if set).
        let expected_input_is_some = wires.target_read_wires.expected_input.is_some()?;
        let expected_input_value = wires.target_read_wires.expected_input.decode_or_zero()?;
        expected_input_value.conditional_enforce_equal(
            &wires.arg(ArgName::Val),
            &(switch & expected_input_is_some),
        )?;

        // 6. Resumer check: current process must match target's expected_resumer (if set).
        let expected_resumer_is_some = wires.target_read_wires.expected_resumer.is_some()?;
        let expected_resumer_value = wires.target_read_wires.expected_resumer.decode_or_zero()?;
        expected_resumer_value
            .conditional_enforce_equal(&wires.id_curr, &(switch & expected_resumer_is_some))?;

        // 7. Store expected resumer for the current process.
        wires
            .curr_write_wires
            .expected_resumer
            .encoded()
            .conditional_enforce_equal(&wires.arg(ArgName::IdPrev), switch)?;

        // ---
        // IVC state updates
        // ---
        // On resume, current program becomes the target, and the old current program
        // becomes the new previous program.
        let next_id_curr = switch.select(&wires.arg(ArgName::Target), &wires.id_curr)?;
        let next_id_prev = OptionalFpVar::select_encoded(
            switch,
            &OptionalFpVar::from_pid(&wires.id_curr),
            &wires.id_prev,
        )?;

        wires.id_curr = next_id_curr;
        wires.id_prev = next_id_prev;

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
            .id_prev_is_some()?
            .conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // 2. Claim check: burned value `ret` must match parent's `expected_input` (if set).
        // Parent's state is in `target_read_wires`.
        let expected_input_is_some = wires.target_read_wires.expected_input.is_some()?;
        let expected_input_value = wires.target_read_wires.expected_input.decode_or_zero()?;
        expected_input_value.conditional_enforce_equal(
            &wires.arg(ArgName::Ret),
            &(switch & expected_input_is_some),
        )?;

        // ---
        // IVC state updates
        // ---
        // Like yield, current program becomes the parent, and new prev is the one that burned.
        let prev_value = wires.id_prev_value()?;
        let next_id_curr = switch.select(&prev_value, &wires.id_curr)?;
        let next_id_prev = OptionalFpVar::select_encoded(
            switch,
            &OptionalFpVar::from_pid(&wires.id_curr),
            &wires.id_prev,
        )?;
        wires.id_curr = next_id_curr;
        wires.id_prev = next_id_prev;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_yield(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.yield_op;

        // 1. Must have a parent.
        wires
            .id_prev_is_some()?
            .conditional_enforce_equal(&Boolean::TRUE, switch)?;

        // 2. Claim check: yielded value `val` must match parent's `expected_input`.
        // The parent's state is in `target_read_wires` because we set `target = irw.id_prev`.
        let expected_input_is_some = wires.target_read_wires.expected_input.is_some()?;
        let expected_input_value = wires.target_read_wires.expected_input.decode_or_zero()?;
        expected_input_value.conditional_enforce_equal(
            &wires.arg(ArgName::Val),
            &(switch & expected_input_is_some),
        )?;

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

        // The next `expected_input` is `ret_value` if `ret` is Some, and None otherwise.
        let new_expected_input_encoded = wires
            .ret_is_some
            .select(&(&wires.arg(ArgName::Ret) + FpVar::one()), &FpVar::zero())?;
        wires
            .curr_write_wires
            .expected_input
            .encoded()
            .conditional_enforce_equal(&new_expected_input_encoded, switch)?;

        // The next expected resumer is the provided id_prev.
        wires
            .curr_write_wires
            .expected_resumer
            .encoded()
            .conditional_enforce_equal(&wires.arg(ArgName::IdPrev), switch)?;

        // ---
        // IVC state updates
        // ---
        // On yield, the current program becomes the parent (old id_prev),
        // and the new prev program is the one that just yielded.
        let prev_value = wires.id_prev_value()?;
        let next_id_curr = switch.select(&prev_value, &wires.id_curr)?;
        let next_id_prev = OptionalFpVar::select_encoded(
            switch,
            &OptionalFpVar::from_pid(&wires.id_curr),
            &wires.id_prev,
        )?;
        wires.id_curr = next_id_curr;
        wires.id_prev = next_id_prev;

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
        let program_hash_args = [
            ArgName::ProgramHash0,
            ArgName::ProgramHash1,
            ArgName::ProgramHash2,
            ArgName::ProgramHash3,
        ];
        for (i, arg) in program_hash_args.iter().enumerate() {
            wires.rom_program_hash[i].conditional_enforce_equal(&wires.arg(*arg), &switch)?;
        }

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

        wires
            .target_write_wires
            .initialized
            .conditional_enforce_equal(&wires.constant_true, &switch)?;

        wires.target_write_wires.init =
            switch.select(&wires.arg(ArgName::Val), &wires.target_read_wires.init)?;

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
            .conditional_enforce_equal(&wires.arg(ArgName::Val), switch)?;

        // 2. Check that the caller from the opcode matches the `id_prev` IVC variable.
        wires
            .id_prev_value()?
            .conditional_enforce_equal(&wires.arg(ArgName::Caller), switch)?;

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
            .conditional_enforce_equal(&wires.arg(ArgName::Val), switch)?;

        // 2. Check that the caller from the opcode matches the `id_prev` IVC variable.
        wires
            .id_prev_value()?
            .conditional_enforce_equal(&wires.arg(ArgName::Caller), switch)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_bind(&self, wires: Wires) -> Result<Wires, SynthesisError> {
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
            .is_some()?
            .conditional_enforce_equal(&wires.constant_false, switch)?;

        let owner_id_encoded = &wires.arg(ArgName::OwnerId) + FpVar::one();
        wires
            .curr_write_wires
            .ownership
            .encoded()
            .conditional_enforce_equal(&owner_id_encoded, switch)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_unbind(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.unbind;

        let is_utxo_curr = wires.is_utxo_curr.is_one()?;
        let is_utxo_target = wires.is_utxo_target.is_one()?;

        (is_utxo_curr & is_utxo_target).conditional_enforce_equal(&wires.constant_true, switch)?;

        // only the owner can unbind
        let id_curr_encoded = &wires.id_curr + FpVar::one();
        wires
            .target_read_wires
            .ownership
            .encoded()
            .conditional_enforce_equal(&id_curr_encoded, switch)?;

        wires
            .target_write_wires
            .ownership
            .encoded()
            .conditional_enforce_equal(&FpVar::zero(), switch)?;

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
            .arg(ArgName::Ret)
            .conditional_enforce_equal(&wires.ref_arena_stack_ptr, switch)?;

        // 3. Init building state
        // remaining = size
        wires.ref_building_remaining =
            switch.select(&wires.arg(ArgName::Size), &wires.ref_building_remaining)?;
        // ptr = ret
        wires.ref_building_ptr =
            switch.select(&wires.arg(ArgName::Ret), &wires.ref_building_ptr)?;

        // 4. Increment stack ptr by size
        let size = wires.arg(ArgName::Size);
        wires.ref_arena_stack_ptr = switch.select(
            &(&wires.ref_arena_stack_ptr
                + size * FpVar::Constant(F::from(REF_PUSH_BATCH_SIZE as u64))),
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
        // remaining -= 1 word
        let next_remaining = &wires.ref_building_remaining - FpVar::one();
        wires.ref_building_remaining =
            switch.select(&next_remaining, &wires.ref_building_remaining)?;

        // ptr += 4 elems
        let inc = FpVar::one() + FpVar::one() + FpVar::one() + FpVar::one();
        let next_ptr = &wires.ref_building_ptr + inc;
        wires.ref_building_ptr = switch.select(&next_ptr, &wires.ref_building_ptr)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_ref_get(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.get;

        let expected = [
            wires.opcode_args[ArgName::PackedRef0.idx()].clone(),
            wires.opcode_args[ArgName::PackedRef2.idx()].clone(),
            wires.opcode_args[ArgName::PackedRef4.idx()].clone(),
            wires.opcode_args[ArgName::PackedRef5.idx()].clone(),
        ];

        for i in 0..REF_GET_BATCH_SIZE {
            expected[i].conditional_enforce_equal(&wires.ref_arena_read[i], switch)?;
        }

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_ref_write(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        // Plumbing only: circuit semantics for in-place ref writes are not enforced yet.
        let _switch = &wires.switches.ref_write;
        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn visit_program_hash(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.switches.program_hash;

        let program_hash_args = [
            ArgName::ProgramHash0,
            ArgName::ProgramHash1,
            ArgName::ProgramHash2,
            ArgName::ProgramHash3,
        ];
        for (i, arg) in program_hash_args.iter().enumerate() {
            wires
                .arg(*arg)
                .conditional_enforce_equal(&wires.rom_program_hash[i], switch)?;
        }

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
            .conditional_enforce_equal(&wires.arg(ArgName::InterfaceId), switch)?;

        // Update handler stack counter (allocate new node)
        wires.handler_stack_ptr = switch.select(
            &(&wires.handler_stack_ptr + FpVar::one()),
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
            .conditional_enforce_equal(&wires.arg(ArgName::InterfaceId), switch)?;

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
            .conditional_enforce_equal(&wires.arg(ArgName::InterfaceId), switch)?;

        // Read the node at current head: should contain (process_id, next_ptr)
        let node_process = &wires.handler_state.handler_stack_node_process;

        // The process_id in the node IS the handler_id we want to return
        wires
            .arg(ArgName::Ret)
            .conditional_enforce_equal(node_process, switch)?;

        Ok(wires)
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
    mb.register_mem(MemoryTag::IsUtxo.into(), 1, MemType::Rom, "ROM_IS_UTXO");
    mb.register_mem(
        MemoryTag::Interfaces.into(),
        1,
        MemType::Rom,
        "ROM_INTERFACES",
    );
    mb.register_mem(MemoryTag::RefArena.into(), 1, MemType::Ram, "RAM_REF_ARENA");
    mb.register_mem(MemoryTag::RefSizes.into(), 1, MemType::Ram, "RAM_REF_SIZES");
    mb.register_mem(
        MemoryTag::ExpectedInput.into(),
        1,
        MemType::Ram,
        "RAM_EXPECTED_INPUT",
    );
    mb.register_mem(
        MemoryTag::ExpectedResumer.into(),
        1,
        MemType::Ram,
        "RAM_EXPECTED_RESUMER",
    );
    mb.register_mem(
        MemoryTag::Activation.into(),
        1,
        MemType::Ram,
        "RAM_ACTIVATION",
    );
    mb.register_mem(MemoryTag::Init.into(), 1, MemType::Ram, "RAM_INIT");
    mb.register_mem(MemoryTag::Counters.into(), 1, MemType::Ram, "RAM_COUNTERS");
    mb.register_mem(
        MemoryTag::Initialized.into(),
        1,
        MemType::Ram,
        "RAM_INITIALIZED",
    );
    mb.register_mem(
        MemoryTag::Finalized.into(),
        1,
        MemType::Ram,
        "RAM_FINALIZED",
    );
    mb.register_mem(MemoryTag::DidBurn.into(), 1, MemType::Ram, "RAM_DID_BURN");
    mb.register_mem(
        MemoryTag::Ownership.into(),
        1,
        MemType::Ram,
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
    mb.register_mem(
        MemoryTag::TraceCommitments.into(),
        1,
        MemType::Ram,
        "RAM_TRACE_COMMITMENTS",
    );
}

#[tracing::instrument(target = "gr1cs", skip_all)]
fn ivc_wires(
    cs: &ConstraintSystemRef<F>,
    wires_in: &Wires,
    wires_out: &Wires,
) -> Result<IvcWireLayout, SynthesisError> {
    let input = IvcWireIndices {
        id_curr: fpvar_witness_index(cs, &wires_in.id_curr)?,
        id_prev: fpvar_witness_index(cs, &wires_in.id_prev.encoded())?,
        ref_arena_stack_ptr: fpvar_witness_index(cs, &wires_in.ref_arena_stack_ptr)?,
        handler_stack_ptr: fpvar_witness_index(cs, &wires_in.handler_stack_ptr)?,
        ref_building_remaining: fpvar_witness_index(cs, &wires_in.ref_building_remaining)?,
        ref_building_ptr: fpvar_witness_index(cs, &wires_in.ref_building_ptr)?,
    };

    let output = IvcWireIndices {
        id_curr: fpvar_witness_index(cs, &wires_out.id_curr)?,
        id_prev: fpvar_witness_index(cs, &wires_out.id_prev.encoded())?,
        ref_arena_stack_ptr: fpvar_witness_index(cs, &wires_out.ref_arena_stack_ptr)?,
        handler_stack_ptr: fpvar_witness_index(cs, &wires_out.handler_stack_ptr)?,
        ref_building_remaining: fpvar_witness_index(cs, &wires_out.ref_building_remaining)?,
        ref_building_ptr: fpvar_witness_index(cs, &wires_out.ref_building_ptr)?,
    };

    Ok(IvcWireLayout { input, output })
}

fn fpvar_witness_index(
    cs: &ConstraintSystemRef<F>,
    var: &FpVar<F>,
) -> Result<usize, SynthesisError> {
    let witness_offset = cs.num_instance_variables();
    match var {
        FpVar::Var(alloc) => {
            let full_index = alloc
                .variable
                .get_variable_index(witness_offset)
                .ok_or(SynthesisError::AssignmentMissing)?;
            if alloc.variable.is_instance() {
                return Err(SynthesisError::AssignmentMissing);
            }
            Ok(full_index - witness_offset)
        }
        FpVar::Constant(_) => Err(SynthesisError::AssignmentMissing),
    }
}

impl PreWires {
    pub fn new(
        irw: InterRoundWires,
        curr_mem_switches: MemSwitchboard,
        target_mem_switches: MemSwitchboard,
        rom_switches: RomSwitchboard,
        handler_switches: HandlerSwitchboard,
        interface_index: F,
        opcode_discriminant: F,
    ) -> Self {
        Self {
            switches: ExecutionSwitches::default(),
            irw,
            interface_index,
            ret_is_some: false,
            opcode_args: [F::ZERO; OPCODE_ARG_COUNT],
            opcode_discriminant,
            curr_mem_switches,
            target_mem_switches,
            rom_switches,
            handler_switches,
        }
    }

    pub fn arg(&self, kind: ArgName) -> F {
        self.opcode_args[kind.idx()]
    }

    pub fn debug_print(&self) {
        let _guard = debug_span!("witness assignments").entered();

        tracing::debug!("target={}", self.arg(ArgName::Target));
        tracing::debug!("val={}", self.arg(ArgName::Val));
        tracing::debug!("ret={}", self.arg(ArgName::Ret));
        tracing::debug!("id_prev={}", self.irw.id_prev.encoded());
    }
}

fn trace_ic<M: IVCMemory<F>>(curr_pid: usize, mb: &mut M, config: &OpcodeConfig) {
    if config.execution_switches.nop {
        return;
    }

    let mut concat_data = [F::ZERO; 12];

    for i in 0..4 {
        let addr = (curr_pid * 4) + i;

        concat_data[i] = mb.conditional_read(
            true,
            Address {
                tag: MemoryTag::TraceCommitments.into(),
                addr: addr as u64,
            },
        )[0];
    }

    concat_data[4] = config.opcode_discriminant;
    for i in 0..OPCODE_ARG_COUNT {
        concat_data[i + 5] = config.opcode_args[i];
    }

    let new_commitment = ark_poseidon2::compress_12_trace(&concat_data).unwrap();

    for i in 0..4 {
        let addr = (curr_pid * 4) + i;

        mb.conditional_write(
            true,
            Address {
                addr: addr as u64,
                tag: MemoryTag::TraceCommitments.into(),
            },
            vec![new_commitment[i]],
        );
    }
}

fn trace_ic_wires<M: IVCMemoryAllocated<F>>(
    id_curr: FpVar<F>,
    rm: &mut M,
    cs: &ConstraintSystemRef<F>,
    should_trace: &Boolean<F>,
    opcode_discriminant: &FpVar<F>,
    opcode_args: &[FpVar<F>; OPCODE_ARG_COUNT],
) -> Result<(), SynthesisError> {
    let mut current_commitment = vec![];

    let mut addresses = vec![];

    for i in 0..4 {
        let offset = FpVar::new_constant(cs.clone(), F::from(i as u64))?;
        let addr = &(id_curr.clone() * FpVar::new_constant(cs.clone(), F::from(4))?) + &offset;
        let address = Address {
            tag: MemoryTag::TraceCommitments.allocate(cs.clone())?,
            addr,
        };

        addresses.push(address.clone());

        let rv = rm.conditional_read(should_trace, &address)?[0].clone();
        current_commitment.push(rv);
    }

    let compress_input = [
        current_commitment[0].clone(),
        current_commitment[1].clone(),
        current_commitment[2].clone(),
        current_commitment[3].clone(),
        opcode_discriminant.clone(),
        opcode_args[0].clone(),
        opcode_args[1].clone(),
        opcode_args[2].clone(),
        opcode_args[3].clone(),
        opcode_args[4].clone(),
        opcode_args[5].clone(),
        opcode_args[6].clone(),
    ];

    let new_commitment = ark_poseidon2::compress_12(&compress_input)?;

    for i in 0..4 {
        rm.conditional_write(should_trace, &addresses[i], &[new_commitment[i].clone()])?;
    }

    Ok(())
}
