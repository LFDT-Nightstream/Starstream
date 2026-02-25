use crate::F;
use crate::opcode_dsl::OpcodeDsl;
use ark_r1cs_std::{eq::EqGadget as _, fields::fp::FpVar, prelude::Boolean};
use ark_relations::gr1cs::SynthesisError;
use starstream_interleaving_spec::RamMemoryTag;

pub fn coroutine_args_ops<D: OpcodeDsl>(
    dsl: &mut D,
    activation_cond: &D::Bool,
    init_cond: &D::Bool,
    addr: &D::Val,
) -> Result<(D::Val, D::Val), D::Error> {
    let activation = dsl.read(activation_cond, RamMemoryTag::Activation, addr)?;
    let init = dsl.read(init_cond, RamMemoryTag::Init, addr)?;
    Ok((activation, init))
}

pub fn check_activation(
    switch: &Boolean<F>,
    activation: &FpVar<F>,
    arg_val: &FpVar<F>,
    expected_caller: &FpVar<F>,
    arg_caller: &FpVar<F>,
) -> Result<(), SynthesisError> {
    activation.conditional_enforce_equal(arg_val, switch)?;
    expected_caller.conditional_enforce_equal(arg_caller, switch)?;
    Ok(())
}

pub fn check_init(
    switch: &Boolean<F>,
    init: &FpVar<F>,
    arg_val: &FpVar<F>,
    init_caller: &FpVar<F>,
    arg_caller: &FpVar<F>,
) -> Result<(), SynthesisError> {
    init.conditional_enforce_equal(arg_val, switch)?;
    init_caller.conditional_enforce_equal(arg_caller, switch)?;
    Ok(())
}
