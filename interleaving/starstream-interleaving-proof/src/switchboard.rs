use crate::F;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
use ark_r1cs_std::alloc::AllocVar;
use ark_r1cs_std::prelude::Boolean;

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
