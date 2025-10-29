//! Linear layer implementations for Poseidon2 R1CS gadget

use crate::{
    F,
    poseidon2::{
        goldilocks::{matrix_diag_8_goldilocks, matrix_diag_16_goldilocks},
        math::mds_light_permutation,
    },
};
use ark_ff::PrimeField;
use ark_r1cs_std::fields::fp::FpVar;
use ark_relations::gr1cs::SynthesisError;

/// Trait for external linear layer operations
pub trait ExternalLinearLayer<F: PrimeField, const WIDTH: usize> {
    // fn apply(state: &mut [FpVar<F>; WIDTH]) -> Result<(), SynthesisError>;

    // permute_state_initial, permute_state_terminal are split as the Poseidon2 specifications are slightly different
    // with the initial rounds involving an extra matrix multiplication.

    /// Perform the initial external layers of the Poseidon2 permutation on the given state.
    fn apply(state: &mut [FpVar<F>; WIDTH]) -> Result<(), SynthesisError>;
}

/// Trait for internal linear layer operations
pub trait InternalLinearLayer<F: PrimeField, const WIDTH: usize> {
    fn apply(state: &mut [FpVar<F>; WIDTH]) -> Result<(), SynthesisError>;
}

pub enum GoldilocksExternalLinearLayer<const WIDTH: usize> {}

// /// A generic method performing the transformation:
// ///
// /// `x -> (x + round_constant)^D`
// #[inline(always)]
// pub fn add_round_constant_and_sbox(
//     val: &mut FpVar<F>,
//     rc: &FpVar<F>,
// ) -> Result<(), SynthesisError> {
//     *val += rc;
//     *val = val.pow_by_constant(&[GOLDILOCKS_S_BOX_DEGREE])?;

//     Ok(())
// }

impl<const WIDTH: usize> ExternalLinearLayer<F, WIDTH> for GoldilocksExternalLinearLayer<WIDTH> {
    fn apply(state: &mut [FpVar<F>; WIDTH]) -> Result<(), SynthesisError> {
        mds_light_permutation(state)?;

        // for elem in &round_constants.beginning_full_round_constants {
        //     state
        //         .iter_mut()
        //         .zip(elem.iter())
        //         .for_each(|(x, c)| add_round_constant_and_sbox(x, c).unwrap());
        //     mds_light_permutation(state);
        // }

        Ok(())
    }

    // fn permute_terminal(
    //     round_constants: &AllocatedRoundConstants<F, WIDTH, HALF_FULL_ROUNDS, PARTIAL_ROUNDS>,
    //     state: &mut [FpVar<F>; WIDTH],
    // ) -> Result<(), SynthesisError> {
    //     for elem in &round_constants.ending_full_round_constants {
    //         state
    //             .iter_mut()
    //             .zip(elem.iter())
    //             .for_each(|(s, rc)| add_round_constant_and_sbox(s, rc).unwrap());
    //         mds_light_permutation(state);
    //     }

    //     Ok(())
    // }
}

pub enum GoldilocksInternalLinearLayer8 {}

pub enum GoldilocksInternalLinearLayer16 {}

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

impl InternalLinearLayer<F, 16> for GoldilocksInternalLinearLayer16 {
    fn apply(state: &mut [FpVar<F>; 16]) -> Result<(), SynthesisError> {
        matmul_internal(state, matrix_diag_16_goldilocks());

        Ok(())
    }
}

// /// Default external linear layer for width 4 (commonly used for Goldilocks)
// pub struct DefaultExternalLinearLayer4;

// impl<F: PrimeField> ExternalLinearLayer<F, 4> for DefaultExternalLinearLayer4 {
//     fn apply(state: &mut [FpVar<F>; 4]) -> Result<(), SynthesisError> {
//         // Placeholder implementation for width 4
//         let mut new_state = state.clone();

//         // Simple mixing (replace with actual Poseidon2 matrix)
//         new_state[0] = &state[0] + &state[1] + &state[2] + &state[3];
//         new_state[1] = &state[0] + &state[1];
//         new_state[2] = &state[0] + &state[2];
//         new_state[3] = &state[0] + &state[3];

//         *state = new_state;
//         Ok(())
//     }
// }

// /// Default internal linear layer for width 4
// pub struct DefaultInternalLinearLayer4;

// impl<F: PrimeField> InternalLinearLayer<F, 4> for DefaultInternalLinearLayer4 {
//     fn apply(state: &mut [FpVar<F>; 4]) -> Result<(), SynthesisError> {
//         // Simple internal linear layer for width 4
//         let sum = &state[1] + &state[2] + &state[3];
//         state[0] = &state[0] + &sum;

//         Ok(())
//     }
// }
