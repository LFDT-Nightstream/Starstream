use crate::F;
use crate::memory::{Address, IVCMemory, IVCMemoryAllocated};
use crate::memory_tags::{MemoryTag, ProgramStateTag};
use crate::optional::{OptionalF, OptionalFpVar};
use crate::switchboard::{MemSwitchboard, MemSwitchboardWires};
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

macro_rules! define_program_state_operations {
    ($(($field:ident, $tag:ident, $field_type:ident)),* $(,)?) => {
        // Out-of-circuit version
        pub fn trace_program_state_writes<M: IVCMemory<F>>(
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
        pub fn program_state_write_wires<M: IVCMemoryAllocated<F>>(
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
                    &[define_program_state_operations!(@convert_to_fpvar state.$field, $field_type)],
                )?;
            )*
            Ok(())
        }

        // Out-of-circuit read version
        pub fn trace_program_state_reads<M: IVCMemory<F>>(
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
    (@convert_to_f $value:expr, optional) => { $value.encoded() };

    (@convert_from_f $value:expr, field) => { $value };
    (@convert_from_f $value:expr, bool) => { $value == F::ONE };
    (@convert_from_f $value:expr, optional) => { OptionalF::from_encoded($value) };

    (@convert_to_fpvar $value:expr, field) => { $value.clone().into() };
    (@convert_to_fpvar $value:expr, bool) => { $value.clone().into() };
    (@convert_to_fpvar $value:expr, optional) => { $value.encoded() };
}

define_program_state_operations!(
    (expected_input, ExpectedInput, optional),
    (expected_resumer, ExpectedResumer, optional),
    (activation, Activation, field),
    (init, Init, field),
    (counters, Counters, field),
    (initialized, Initialized, bool),
    (finalized, Finalized, bool),
    (did_burn, DidBurn, bool),
    (ownership, Ownership, optional),
);

pub fn program_state_read_wires<M: IVCMemoryAllocated<F>>(
    rm: &mut M,
    cs: &ConstraintSystemRef<F>,
    address: FpVar<F>,
    switches: &MemSwitchboardWires,
) -> Result<ProgramStateWires, SynthesisError> {
    Ok(ProgramStateWires {
        expected_input: OptionalFpVar::new(
            rm.conditional_read(
                &switches.expected_input,
                &Address {
                    addr: address.clone(),
                    tag: MemoryTag::ExpectedInput.allocate(cs.clone())?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
        ),
        expected_resumer: OptionalFpVar::new(
            rm.conditional_read(
                &switches.expected_resumer,
                &Address {
                    addr: address.clone(),
                    tag: MemoryTag::ExpectedResumer.allocate(cs.clone())?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
        ),
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
        ownership: OptionalFpVar::new(
            rm.conditional_read(
                &switches.ownership,
                &Address {
                    addr: address.clone(),
                    tag: MemoryTag::Ownership.allocate(cs.clone())?,
                },
            )?
            .into_iter()
            .next()
            .unwrap(),
        ),
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
