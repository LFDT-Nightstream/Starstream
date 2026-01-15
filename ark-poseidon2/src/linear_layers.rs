//! Linear layer implementations for Poseidon2 R1CS gadget

use crate::{F, goldilocks::matrix_diag_8_goldilocks, math::mds_light_permutation};
use ark_ff::PrimeField;
use ark_r1cs_std::fields::fp::FpVar;
use ark_relations::gr1cs::SynthesisError;

pub trait ExternalLinearLayer<F: PrimeField, const WIDTH: usize> {
    fn apply(state: &mut [FpVar<F>; WIDTH]) -> Result<(), SynthesisError>;
}

pub trait InternalLinearLayer<F: PrimeField, const WIDTH: usize> {
    fn apply(state: &mut [FpVar<F>; WIDTH]) -> Result<(), SynthesisError>;
}

pub enum GoldilocksExternalLinearLayer<const WIDTH: usize> {}

impl<const WIDTH: usize> ExternalLinearLayer<F, WIDTH> for GoldilocksExternalLinearLayer<WIDTH> {
    fn apply(state: &mut [FpVar<F>; WIDTH]) -> Result<(), SynthesisError> {
        mds_light_permutation(state)?;

        Ok(())
    }
}

pub enum GoldilocksInternalLinearLayer8 {}

pub fn matmul_internal<const WIDTH: usize>(
    state: &mut [FpVar<F>; WIDTH],
    mat_internal_diag_m_1: &'static [F; WIDTH],
) {
    let sum: FpVar<F> = state.iter().sum();
    for i in 0..WIDTH {
        state[i] *= FpVar::Constant(mat_internal_diag_m_1[i]);
        state[i] += sum.clone();
    }
}

impl InternalLinearLayer<F, 8> for GoldilocksInternalLinearLayer8 {
    fn apply(state: &mut [FpVar<F>; 8]) -> Result<(), SynthesisError> {
        matmul_internal(state, matrix_diag_8_goldilocks());

        Ok(())
    }
}
