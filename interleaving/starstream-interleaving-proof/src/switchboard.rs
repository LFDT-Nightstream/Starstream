use crate::F;
use ark_r1cs_std::alloc::AllocVar;
use ark_r1cs_std::prelude::Boolean;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};

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
pub struct MemSwitchboard<B> {
    pub expected_input: B,
    pub expected_resumer: B,
    pub on_yield: B,
    pub yield_to: B,
    pub activation: B,
    pub init: B,
    pub init_caller: B,
    pub counters: B,
    pub initialized: B,
    pub finalized: B,
    pub did_burn: B,
    pub ownership: B,
}

pub type MemSwitchboardBool = MemSwitchboard<bool>;
pub type MemSwitchboardWires = MemSwitchboard<Boolean<F>>;

#[derive(Clone, Debug, Default)]
pub struct HandlerSwitchboard {
    pub read_interface: bool,
    pub read_head: bool,
    pub read_node: bool,
    pub write_node: bool,
    pub write_head: bool,
}

#[derive(Clone, Debug, Default)]
pub struct RefArenaSwitchboard {
    pub ref_sizes_write: bool,
    pub ref_sizes_read: bool,
    pub ref_arena_read: bool,
    pub ref_arena_write: bool,
    pub ref_arena_write_is_push: bool,
}

#[derive(Clone)]
pub struct HandlerSwitchboardWires {
    pub read_interface: Boolean<F>,
    pub read_head: Boolean<F>,
    pub read_node: Boolean<F>,
    pub write_node: Boolean<F>,
    pub write_head: Boolean<F>,
}

#[derive(Clone)]
pub struct RefArenaSwitchboardWires {
    pub ref_sizes_write: Boolean<F>,
    pub ref_sizes_read: Boolean<F>,
    pub ref_arena_read: Boolean<F>,
    pub ref_arena_write: Boolean<F>,
    pub ref_arena_write_is_push: Boolean<F>,
}

impl MemSwitchboardWires {
    pub fn allocate(
        cs: ConstraintSystemRef<F>,
        switches: &MemSwitchboardBool,
    ) -> Result<Self, SynthesisError> {
        Ok(Self {
            expected_input: Boolean::new_witness(cs.clone(), || Ok(switches.expected_input))?,
            expected_resumer: Boolean::new_witness(cs.clone(), || Ok(switches.expected_resumer))?,
            on_yield: Boolean::new_witness(cs.clone(), || Ok(switches.on_yield))?,
            yield_to: Boolean::new_witness(cs.clone(), || Ok(switches.yield_to))?,
            activation: Boolean::new_witness(cs.clone(), || Ok(switches.activation))?,
            init: Boolean::new_witness(cs.clone(), || Ok(switches.init))?,
            init_caller: Boolean::new_witness(cs.clone(), || Ok(switches.init_caller))?,
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

impl RefArenaSwitchboardWires {
    pub fn allocate(
        cs: ConstraintSystemRef<F>,
        switches: &RefArenaSwitchboard,
    ) -> Result<Self, SynthesisError> {
        Ok(Self {
            ref_sizes_write: Boolean::new_witness(cs.clone(), || Ok(switches.ref_sizes_write))?,
            ref_sizes_read: Boolean::new_witness(cs.clone(), || Ok(switches.ref_sizes_read))?,
            ref_arena_read: Boolean::new_witness(cs.clone(), || Ok(switches.ref_arena_read))?,
            ref_arena_write: Boolean::new_witness(cs.clone(), || Ok(switches.ref_arena_write))?,
            ref_arena_write_is_push: Boolean::new_witness(cs, || {
                Ok(switches.ref_arena_write_is_push)
            })?,
        })
    }
}
