use std::collections::{BTreeMap, BTreeSet};

use crate::opcode_dsl::{OpcodeDsl, OpcodeSynthDsl, OpcodeTraceDsl};
use crate::switchboard::{HandlerSwitchboard, HandlerSwitchboardWires};
use crate::{F, LedgerOperation};
use crate::{memory::IVCMemory, memory::IVCMemoryAllocated};
use ark_ff::AdditiveGroup as _;
use ark_r1cs_std::fields::fp::FpVar;
use ark_r1cs_std::prelude::Boolean;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
use starstream_interleaving_spec::{RamMemoryTag, RomMemoryTag};

#[derive(Clone)]
pub struct HandlerSwitches<B> {
    pub read_interface: B,
    pub read_head: B,
    pub read_node: B,
    pub write_node: B,
    pub write_head: B,
}

impl From<&HandlerSwitchboard> for HandlerSwitches<bool> {
    fn from(s: &HandlerSwitchboard) -> Self {
        Self {
            read_interface: s.read_interface,
            read_head: s.read_head,
            read_node: s.read_node,
            write_node: s.write_node,
            write_head: s.write_head,
        }
    }
}

impl From<&HandlerSwitchboardWires> for HandlerSwitches<Boolean<F>> {
    fn from(s: &HandlerSwitchboardWires) -> Self {
        Self {
            read_interface: s.read_interface.clone(),
            read_head: s.read_head.clone(),
            read_node: s.read_node.clone(),
            write_node: s.write_node.clone(),
            write_head: s.write_head.clone(),
        }
    }
}

pub struct HandlerStackReads<V> {
    pub interface_rom_read: [V; 4],
    pub handler_stack_node_process: V,
}

fn handler_stack_ops<D: OpcodeDsl>(
    dsl: &mut D,
    switches: &HandlerSwitches<D::Bool>,
    interface_index: &D::Val,
    handler_stack_counter: &D::Val,
    id_curr: &D::Val,
) -> Result<HandlerStackReads<D::Val>, D::Error> {
    let four = dsl.const_u64(4)?;
    let interface_base_addr = dsl.mul(interface_index, &four)?;
    let read_interface_limb = |dsl: &mut D, limb: u64| -> Result<D::Val, D::Error> {
        let offset = dsl.const_u64(limb)?;
        let addr = dsl.add(&interface_base_addr, &offset)?;
        dsl.read(&switches.read_interface, RomMemoryTag::Interfaces, &addr)
    };
    let interface_rom_read = [
        read_interface_limb(dsl, 0)?,
        read_interface_limb(dsl, 1)?,
        read_interface_limb(dsl, 2)?,
        read_interface_limb(dsl, 3)?,
    ];
    let handler_stack_head_read = dsl.read(
        &switches.read_head,
        RamMemoryTag::HandlerStackHeads,
        interface_index,
    )?;
    let handler_stack_node_process = dsl.read(
        &switches.read_node,
        RamMemoryTag::HandlerStackArenaProcess,
        &handler_stack_head_read,
    )?;
    let handler_stack_node_next = dsl.read(
        &switches.read_node,
        RamMemoryTag::HandlerStackArenaNextPtr,
        &handler_stack_head_read,
    )?;

    let zero = dsl.zero();
    let node_process = dsl.select(&switches.write_node, id_curr, &zero)?;
    dsl.write(
        &switches.write_node,
        RamMemoryTag::HandlerStackArenaProcess,
        handler_stack_counter,
        &node_process,
    )?;

    let node_next = dsl.select(&switches.write_node, &handler_stack_head_read, &zero)?;
    dsl.write(
        &switches.write_node,
        RamMemoryTag::HandlerStackArenaNextPtr,
        handler_stack_counter,
        &node_next,
    )?;

    let head_val = dsl.select(
        &switches.write_node,
        handler_stack_counter,
        &handler_stack_node_next,
    )?;
    dsl.write(
        &switches.write_head,
        RamMemoryTag::HandlerStackHeads,
        interface_index,
        &head_val,
    )?;

    Ok(HandlerStackReads {
        interface_rom_read,
        handler_stack_node_process,
    })
}

pub fn trace_handler_stack_ops<M: IVCMemory<F>>(
    mb: &mut M,
    switches: &HandlerSwitchboard,
    interface_index: &F,
    handler_stack_counter: &F,
    id_curr: &F,
) -> HandlerStackReads<F> {
    let mut dsl = OpcodeTraceDsl { mb };
    let switches = HandlerSwitches::from(switches);
    handler_stack_ops(
        &mut dsl,
        &switches,
        interface_index,
        handler_stack_counter,
        id_curr,
    )
    .expect("trace handler stack ops")
}

pub fn handler_stack_access_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    switches: &HandlerSwitchboardWires,
    interface_index: &ark_r1cs_std::fields::fp::FpVar<F>,
    handler_stack_counter: &ark_r1cs_std::fields::fp::FpVar<F>,
    id_curr: &ark_r1cs_std::fields::fp::FpVar<F>,
) -> Result<HandlerStackReads<ark_r1cs_std::fields::fp::FpVar<F>>, SynthesisError> {
    let mut dsl = OpcodeSynthDsl { cs, rm };
    let switches = HandlerSwitches::from(switches);
    handler_stack_ops(
        &mut dsl,
        &switches,
        interface_index,
        handler_stack_counter,
        id_curr,
    )
}

#[derive(Debug, Clone)]
pub(crate) struct InterfaceResolver {
    mapping: BTreeMap<[F; 4], usize>,
}

impl InterfaceResolver {
    pub(crate) fn new(ops: &[LedgerOperation<F>]) -> Self {
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
                LedgerOperation::CallEffectHandler { interface_id, .. } => {
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

    pub(crate) fn get_index(&self, interface_id: [F; 4]) -> usize {
        *self.mapping.get(&interface_id).unwrap_or(&0)
    }

    pub(crate) fn get_interface_index_field(&self, interface_id: [F; 4]) -> F {
        F::from(self.get_index(interface_id) as u64)
    }

    pub(crate) fn interfaces(&self) -> Vec<[F; 4]> {
        let mut interfaces = vec![[F::ZERO; 4]; self.mapping.len()];
        for (interface_id, index) in &self.mapping {
            interfaces[*index] = *interface_id;
        }
        interfaces
    }
}

#[derive(Clone)]
pub(crate) struct HandlerState {
    pub(crate) handler_stack_node_process: FpVar<F>,
    pub(crate) interface_rom_read: [FpVar<F>; 4],
}
