use super::constants::RoundConstants;
use super::linear_layers::{ExternalLinearLayer, InternalLinearLayer};
use crate::poseidon2::constants::{GOLDILOCKS_S_BOX_DEGREE, HALF_FULL_ROUNDS, PARTIAL_ROUNDS};
use ark_ff::PrimeField;
use ark_r1cs_std::fields::fp::FpVar;
use ark_r1cs_std::prelude::*;
use ark_relations::gr1cs::SynthesisError;

/// R1CS gadget for Poseidon2 hash function
pub struct Poseidon2Gadget<
    F: PrimeField,
    ExtLinear: ExternalLinearLayer<F, WIDTH>,
    IntLinear: InternalLinearLayer<F, WIDTH>,
    const WIDTH: usize,
    const SBOX_DEGREE: u64,
    const HALF_FULL_ROUNDS: usize,
    const PARTIAL_ROUNDS: usize,
> {
    constants: RoundConstants<F, WIDTH, HALF_FULL_ROUNDS, PARTIAL_ROUNDS>,
    _phantom: core::marker::PhantomData<(ExtLinear, IntLinear)>,
}

impl<
    F: PrimeField,
    ExtLinear: ExternalLinearLayer<F, WIDTH>,
    IntLinear: InternalLinearLayer<F, WIDTH>,
    const WIDTH: usize,
    const SBOX_DEGREE: u64,
    const HALF_FULL_ROUNDS: usize,
    const PARTIAL_ROUNDS: usize,
> Poseidon2Gadget<F, ExtLinear, IntLinear, WIDTH, SBOX_DEGREE, HALF_FULL_ROUNDS, PARTIAL_ROUNDS>
{
    pub fn new(constants: RoundConstants<F, WIDTH, HALF_FULL_ROUNDS, PARTIAL_ROUNDS>) -> Self {
        Self {
            // constants: constants.allocate(cs.clone())?,
            constants,
            _phantom: core::marker::PhantomData,
        }
    }

    /// Compute Poseidon2 permutation in R1CS
    pub fn permute(&self, inputs: &[FpVar<F>; WIDTH]) -> Result<[FpVar<F>; WIDTH], SynthesisError> {
        let mut state = inputs.clone();

        ExtLinear::apply(&mut state)?;

        // Beginning full rounds
        for round in 0..HALF_FULL_ROUNDS {
            self.eval_full_round(
                &mut state,
                &self.constants.beginning_full_round_constants[round],
            )?;
        }

        for round in 0..PARTIAL_ROUNDS {
            self.eval_partial_round(&mut state, &self.constants.partial_round_constants[round])?;
        }

        for round in 0..HALF_FULL_ROUNDS {
            self.eval_full_round(
                &mut state,
                &self.constants.ending_full_round_constants[round],
            )?;
        }

        Ok(state)
    }

    fn eval_full_round(
        &self,
        state: &mut [FpVar<F>; WIDTH],
        round_constants: &[F; WIDTH],
    ) -> Result<(), SynthesisError> {
        // Add round constants and apply S-box to each element
        for (s, r) in state.iter_mut().zip(round_constants.iter()) {
            *s = s.clone() + FpVar::constant(*r);
            *s = self.eval_sbox(s.clone())?;
        }

        // Apply external linear layer
        ExtLinear::apply(state)?;

        Ok(())
    }

    fn eval_partial_round(
        &self,
        state: &mut [FpVar<F>; WIDTH],
        round_constant: &F,
    ) -> Result<(), SynthesisError> {
        // Add round constant and apply S-box to first element only
        state[0] = state[0].clone() + FpVar::constant(*round_constant);
        state[0] = self.eval_sbox(state[0].clone())?;

        // Apply internal linear layer
        IntLinear::apply(state)?;

        Ok(())
    }

    /// Evaluates the S-box over a field variable
    fn eval_sbox(&self, x: FpVar<F>) -> Result<FpVar<F>, SynthesisError> {
        match SBOX_DEGREE {
            3 => {
                // x^3
                let x2 = x.square()?;
                Ok(x2 * &x)
            }
            5 => {
                // x^5
                let x2 = x.square()?;
                let x4 = x2.square()?;
                Ok(x4 * &x)
            }
            7 => {
                // x^7
                let x2 = x.square()?;
                let x3 = &x2 * &x;
                let x6 = x3.square()?;
                Ok(x6 * &x)
            }
            _ => Err(SynthesisError::Unsatisfiable),
        }
    }
}

#[cfg(test)]
pub fn poseidon2_hash<
    F: PrimeField,
    ExtLinear: ExternalLinearLayer<F, WIDTH>,
    IntLinear: InternalLinearLayer<F, WIDTH>,
    const WIDTH: usize,
    const SBOX_DEGREE: u64,
    const HALF_FULL_ROUNDS: usize,
    const PARTIAL_ROUNDS: usize,
>(
    inputs: &[FpVar<F>; WIDTH],
    constants: &RoundConstants<F, WIDTH, HALF_FULL_ROUNDS, PARTIAL_ROUNDS>,
) -> Result<[FpVar<F>; WIDTH], SynthesisError> {
    let gadget = Poseidon2Gadget::<
        F,
        ExtLinear,
        IntLinear,
        WIDTH,
        SBOX_DEGREE,
        HALF_FULL_ROUNDS,
        PARTIAL_ROUNDS,
    >::new(constants.clone());
    gadget.permute(inputs)
}

pub fn poseidon2_compress_8_to_4<
    F: PrimeField,
    ExtLinear: ExternalLinearLayer<F, 8>,
    IntLinear: InternalLinearLayer<F, 8>,
>(
    inputs: &[FpVar<F>; 8],
    constants: &RoundConstants<F, 8, HALF_FULL_ROUNDS, PARTIAL_ROUNDS>,
) -> Result<[FpVar<F>; 4], SynthesisError> {
    let gadget = Poseidon2Gadget::<
        F,
        ExtLinear,
        IntLinear,
        8,
        GOLDILOCKS_S_BOX_DEGREE,
        HALF_FULL_ROUNDS,
        PARTIAL_ROUNDS,
    >::new(constants.clone());
    let p_x = gadget.permute(inputs)?;

    // truncation
    let mut p_x: [FpVar<F>; 4] = std::array::from_fn(|i| p_x[i].clone());

    for (p_x, x) in p_x.iter_mut().zip(inputs) {
        // feed-forward operation
        *p_x += x;
    }

    Ok(p_x)
}
