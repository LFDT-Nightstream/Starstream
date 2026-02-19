use ark_r1cs_std::{
    alloc::AllocVar as _,
    eq::EqGadget as _,
    fields::{FieldVar as _, fp::FpVar},
    prelude::Boolean,
};
use ark_relations::gr1cs::{ConstraintSystemRef, LinearCombination, SynthesisError, Variable};
use starstream_interleaving_spec::EffectDiscriminant;

use crate::F;

#[derive(Clone)]
pub(crate) struct ExecutionSwitches<T> {
    pub(crate) resume: T,
    pub(crate) call_effect_handler: T,
    pub(crate) yield_op: T,
    pub(crate) return_op: T,
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
    /// Allocates circuit variables for the switches and enforces exactly one is true
    pub(crate) fn allocate_and_constrain(
        &self,
        cs: ConstraintSystemRef<F>,
        opcode_discriminant: &FpVar<F>,
    ) -> Result<ExecutionSwitches<Boolean<F>>, SynthesisError> {
        let switches = [
            self.resume,
            self.call_effect_handler,
            self.yield_op,
            self.return_op,
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
            call_effect_handler,
            yield_op,
            return_op,
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
            (
                call_effect_handler,
                EffectDiscriminant::CallEffectHandler as u64,
            ),
            (yield_op, EffectDiscriminant::Yield as u64),
            (return_op, EffectDiscriminant::Return as u64),
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
            call_effect_handler: call_effect_handler.clone(),
            yield_op: yield_op.clone(),
            return_op: return_op.clone(),
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

    pub(crate) fn nop() -> Self {
        Self {
            nop: true,
            ..Self::default()
        }
    }

    pub(crate) fn resume() -> Self {
        Self {
            resume: true,
            ..Self::default()
        }
    }

    pub(crate) fn call_effect_handler() -> Self {
        Self {
            call_effect_handler: true,
            ..Self::default()
        }
    }

    pub(crate) fn yield_op() -> Self {
        Self {
            yield_op: true,
            ..Self::default()
        }
    }

    pub(crate) fn return_op() -> Self {
        Self {
            return_op: true,
            ..Self::default()
        }
    }

    pub(crate) fn burn() -> Self {
        Self {
            burn: true,
            ..Self::default()
        }
    }

    pub(crate) fn program_hash() -> Self {
        Self {
            program_hash: true,
            ..Self::default()
        }
    }

    pub(crate) fn new_utxo() -> Self {
        Self {
            new_utxo: true,
            ..Self::default()
        }
    }

    pub(crate) fn new_coord() -> Self {
        Self {
            new_coord: true,
            ..Self::default()
        }
    }

    pub(crate) fn activation() -> Self {
        Self {
            activation: true,
            ..Self::default()
        }
    }

    pub(crate) fn init() -> Self {
        Self {
            init: true,
            ..Self::default()
        }
    }

    pub(crate) fn bind() -> Self {
        Self {
            bind: true,
            ..Self::default()
        }
    }

    pub(crate) fn unbind() -> Self {
        Self {
            unbind: true,
            ..Self::default()
        }
    }

    pub(crate) fn new_ref() -> Self {
        Self {
            new_ref: true,
            ..Self::default()
        }
    }

    pub(crate) fn ref_push() -> Self {
        Self {
            ref_push: true,
            ..Self::default()
        }
    }

    pub(crate) fn get() -> Self {
        Self {
            get: true,
            ..Self::default()
        }
    }

    pub(crate) fn ref_write() -> Self {
        Self {
            ref_write: true,
            ..Self::default()
        }
    }

    pub(crate) fn install_handler() -> Self {
        Self {
            install_handler: true,
            ..Self::default()
        }
    }

    pub(crate) fn uninstall_handler() -> Self {
        Self {
            uninstall_handler: true,
            ..Self::default()
        }
    }

    pub(crate) fn get_handler_for() -> Self {
        Self {
            get_handler_for: true,
            ..Self::default()
        }
    }
}

impl Default for ExecutionSwitches<bool> {
    fn default() -> Self {
        Self {
            resume: false,
            call_effect_handler: false,
            yield_op: false,
            return_op: false,
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
