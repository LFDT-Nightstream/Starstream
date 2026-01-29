use crate::F;
use crate::opcode_dsl::{OpcodeDsl, OpcodeSynthDsl, OpcodeTraceDsl};
use crate::switchboard::{HandlerSwitchboard, HandlerSwitchboardWires};
use crate::{circuit::MemoryTag, memory::IVCMemory, memory::IVCMemoryAllocated};
use ark_r1cs_std::prelude::Boolean;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};

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
    pub interface_rom_read: V,
    pub handler_stack_node_process: V,
}

fn handler_stack_ops<D: OpcodeDsl>(
    dsl: &mut D,
    switches: &HandlerSwitches<D::Bool>,
    interface_index: &D::Val,
    handler_stack_counter: &D::Val,
    id_curr: &D::Val,
) -> Result<HandlerStackReads<D::Val>, D::Error> {
    let interface_rom_read = dsl.read(
        &switches.read_interface,
        MemoryTag::Interfaces,
        interface_index,
    )?;
    let handler_stack_head_read = dsl.read(
        &switches.read_head,
        MemoryTag::HandlerStackHeads,
        interface_index,
    )?;
    let handler_stack_node_process = dsl.read(
        &switches.read_node,
        MemoryTag::HandlerStackArenaProcess,
        &handler_stack_head_read,
    )?;
    let handler_stack_node_next = dsl.read(
        &switches.read_node,
        MemoryTag::HandlerStackArenaNextPtr,
        &handler_stack_head_read,
    )?;

    let zero = dsl.zero();
    let node_process = dsl.select(&switches.write_node, id_curr, &zero)?;
    dsl.write(
        &switches.write_node,
        MemoryTag::HandlerStackArenaProcess,
        handler_stack_counter,
        &node_process,
    )?;

    let node_next = dsl.select(&switches.write_node, &handler_stack_head_read, &zero)?;
    dsl.write(
        &switches.write_node,
        MemoryTag::HandlerStackArenaNextPtr,
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
        MemoryTag::HandlerStackHeads,
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
