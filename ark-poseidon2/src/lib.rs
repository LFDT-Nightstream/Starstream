//! Poseidon2 hash function implementation for R1CS (SNARK) systems using Arkworks.

pub mod constants;
pub mod gadget;
pub mod goldilocks;
pub mod linear_layers;
pub mod math;

pub type F = ark_goldilocks::FpGoldilocks;

use crate::{
    gadget::{poseidon2_compress, poseidon2_sponge_absorb},
    linear_layers::{
        GoldilocksExternalLinearLayer, GoldilocksInternalLinearLayer8,
        GoldilocksInternalLinearLayer12,
    },
};
use ark_r1cs_std::{GR1CSVar as _, alloc::AllocVar as _, fields::fp::FpVar};
use ark_relations::gr1cs::{ConstraintSystem, SynthesisError};
pub use constants::RoundConstants;
use constants::{GOLDILOCKS_S_BOX_DEGREE, HALF_FULL_ROUNDS, PARTIAL_ROUNDS};

pub fn compress_8(inputs: &[FpVar<F>; 8]) -> Result<[FpVar<F>; 4], SynthesisError> {
    let constants = RoundConstants::new_goldilocks_8_constants();

    poseidon2_compress::<8, 4, F, GoldilocksExternalLinearLayer<8>, GoldilocksInternalLinearLayer8>(
        inputs, &constants,
    )
}

pub fn compress_12(inputs: &[FpVar<F>; 12]) -> Result<[FpVar<F>; 4], SynthesisError> {
    let constants = RoundConstants::new_goldilocks_12_constants();

    poseidon2_compress::<12, 4, F, GoldilocksExternalLinearLayer<12>, GoldilocksInternalLinearLayer12>(
        inputs, &constants,
    )
}

pub fn sponge_8(inputs: &[FpVar<F>]) -> Result<[FpVar<F>; 4], SynthesisError> {
    let constants = RoundConstants::new_goldilocks_8_constants();

    let state = poseidon2_sponge_absorb::<
        F,
        GoldilocksExternalLinearLayer<8>,
        GoldilocksInternalLinearLayer8,
        8,
        4,
        GOLDILOCKS_S_BOX_DEGREE,
        HALF_FULL_ROUNDS,
        PARTIAL_ROUNDS,
    >(inputs, &constants)?;

    Ok(std::array::from_fn(|i| state[i].clone()))
}

pub fn sponge_12(inputs: &[FpVar<F>]) -> Result<[FpVar<F>; 4], SynthesisError> {
    let constants = RoundConstants::new_goldilocks_12_constants();

    let state = poseidon2_sponge_absorb::<
        F,
        GoldilocksExternalLinearLayer<12>,
        GoldilocksInternalLinearLayer12,
        12,
        8,
        GOLDILOCKS_S_BOX_DEGREE,
        HALF_FULL_ROUNDS,
        PARTIAL_ROUNDS,
    >(inputs, &constants)?;

    Ok(std::array::from_fn(|i| state[i].clone()))
}

fn compress_trace_generic<
    const WIDTH: usize,
    ExtLinear: crate::linear_layers::ExternalLinearLayer<F, WIDTH>,
    IntLinear: crate::linear_layers::InternalLinearLayer<F, WIDTH>,
>(
    inputs: &[F; WIDTH],
    constants: &RoundConstants<F, WIDTH, HALF_FULL_ROUNDS, PARTIAL_ROUNDS>,
) -> Result<[F; 4], SynthesisError> {
    // TODO: obviously this is not a good way of implementing this, but the
    // implementation is currently not general enough to be used over both FpVar and
    // just plain field elements
    //
    // for now, we just create a throw-away constraint system and get the values
    // from that computation
    let cs = ConstraintSystem::<F>::new_ref();

    let inputs = inputs
        .iter()
        .map(|input| FpVar::new_witness(cs.clone(), || Ok(input)))
        .collect::<Result<Vec<_>, _>>()?;

    let compressed = poseidon2_compress::<WIDTH, 4, F, ExtLinear, IntLinear>(
        inputs[..].try_into().unwrap(),
        &constants,
    )?;

    Ok(std::array::from_fn(|i| compressed[i].value().unwrap()))
}

pub fn compress_8_trace(inputs: &[F; 8]) -> Result<[F; 4], SynthesisError> {
    let constants = RoundConstants::new_goldilocks_8_constants();
    compress_trace_generic::<8, GoldilocksExternalLinearLayer<8>, GoldilocksInternalLinearLayer8>(
        inputs, &constants,
    )
}

pub fn compress_12_trace(inputs: &[F; 12]) -> Result<[F; 4], SynthesisError> {
    let constants = RoundConstants::new_goldilocks_12_constants();
    compress_trace_generic::<12, GoldilocksExternalLinearLayer<12>, GoldilocksInternalLinearLayer12>(
        inputs, &constants,
    )
}

pub fn sponge_8_trace(inputs: &[F; 8]) -> Result<[F; 4], SynthesisError> {
    let constants = RoundConstants::new_goldilocks_8_constants();
    sponge_trace_generic::<8, 4, GoldilocksExternalLinearLayer<8>, GoldilocksInternalLinearLayer8>(
        inputs, &constants,
    )
}

pub fn sponge_12_trace(inputs: &[F; 12]) -> Result<[F; 4], SynthesisError> {
    let constants = RoundConstants::new_goldilocks_12_constants();
    sponge_trace_generic::<12, 8, GoldilocksExternalLinearLayer<12>, GoldilocksInternalLinearLayer12>(
        inputs, &constants,
    )
}

fn sponge_trace_generic<
    const WIDTH: usize,
    const RATE: usize,
    ExtLinear: crate::linear_layers::ExternalLinearLayer<F, WIDTH>,
    IntLinear: crate::linear_layers::InternalLinearLayer<F, WIDTH>,
>(
    inputs: &[F; WIDTH],
    constants: &RoundConstants<F, WIDTH, HALF_FULL_ROUNDS, PARTIAL_ROUNDS>,
) -> Result<[F; 4], SynthesisError> {
    // TODO: obviously this is not a good way of implementing this, but the
    // implementation is currently not general enough to be used over both FpVar and
    // just plain field elements
    //
    // for now, we just create a throw-away constraint system and get the values
    // from that computation
    let cs = ConstraintSystem::<F>::new_ref();

    let inputs = inputs
        .iter()
        .map(|input| FpVar::new_witness(cs.clone(), || Ok(input)))
        .collect::<Result<Vec<_>, _>>()?;

    let state = poseidon2_sponge_absorb::<
        F,
        ExtLinear,
        IntLinear,
        WIDTH,
        RATE,
        GOLDILOCKS_S_BOX_DEGREE,
        HALF_FULL_ROUNDS,
        PARTIAL_ROUNDS,
    >(&inputs, constants)?;

    Ok(std::array::from_fn(|i| state[i].value().unwrap()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        F,
        constants::GOLDILOCKS_S_BOX_DEGREE,
        gadget::poseidon2_hash,
        linear_layers::{GoldilocksExternalLinearLayer, GoldilocksInternalLinearLayer8},
    };
    use ark_r1cs_std::{GR1CSVar, alloc::AllocVar, fields::fp::FpVar};
    use ark_relations::gr1cs::{ConstraintSystem, SynthesisError};

    const WIDTH: usize = 8;
    const HALF_FULL_ROUNDS: usize = 4;
    const PARTIAL_ROUNDS: usize = 22;

    #[test]
    fn test_poseidon2_gadget_basic() -> Result<(), SynthesisError> {
        let cs = ConstraintSystem::<F>::new_ref();

        let constants = RoundConstants::new_goldilocks_8_constants();

        let input_values = [
            F::from(0),
            F::from(0),
            F::from(0),
            F::from(0),
            F::from(0),
            F::from(0),
            F::from(0),
            F::from(0),
        ];

        let input_vars = input_values
            .iter()
            .map(|&val| FpVar::new_witness(cs.clone(), || Ok(val)))
            .collect::<Result<Vec<_>, _>>()?;
        let input_array: [FpVar<F>; WIDTH] = input_vars.try_into().unwrap();

        let result = poseidon2_hash::<
            F,
            GoldilocksExternalLinearLayer<8>,
            GoldilocksInternalLinearLayer8,
            WIDTH,
            GOLDILOCKS_S_BOX_DEGREE,
            HALF_FULL_ROUNDS,
            PARTIAL_ROUNDS,
        >(&input_array, &constants)?;

        assert!(cs.is_satisfied()?);

        let output_values: Vec<F> = result
            .iter()
            .map(|var: &FpVar<F>| var.value().unwrap())
            .collect();

        // output taken from the plonky3 implementation
        let expected: [F; 8] = [
            F::from(12033154258266855215_u64),
            F::from(10280848056061907209_u64),
            F::from(2185915012395546036_u64),
            F::from(14655708400709920811_u64),
            F::from(8156942431357196992_u64),
            F::from(4422236401544933648_u64),
            F::from(12369536641900949_u64),
            F::from(7054567940610806767_u64),
        ];

        // At least one output should be non-zero (very likely with our placeholder linear layers)
        assert!(output_values.iter().any(|&val| val != F::from(0u64)));

        println!("Input: {:?}", input_values);
        println!("Output: {:?}", output_values);
        println!("Constraint system satisfied: {}", cs.is_satisfied()?);
        println!("Number of constraints: {}", cs.num_constraints());

        assert_eq!(output_values, expected);

        Ok(())
    }

    #[test]
    fn test_poseidon2_gadget_inc() -> Result<(), SynthesisError> {
        let cs = ConstraintSystem::<F>::new_ref();

        let constants = RoundConstants::new_goldilocks_8_constants();

        // Create test inputs
        let input_values = [
            F::from(1),
            F::from(2),
            F::from(3),
            F::from(4),
            F::from(5),
            F::from(6),
            F::from(7),
            F::from(8),
        ];

        let input_vars = input_values
            .iter()
            .map(|&val| FpVar::new_witness(cs.clone(), || Ok(val)))
            .collect::<Result<Vec<_>, _>>()?;
        let input_array: [FpVar<F>; WIDTH] = input_vars.try_into().unwrap();

        let result = poseidon2_hash::<
            F,
            GoldilocksExternalLinearLayer<8>,
            GoldilocksInternalLinearLayer8,
            WIDTH,
            GOLDILOCKS_S_BOX_DEGREE,
            HALF_FULL_ROUNDS,
            PARTIAL_ROUNDS,
        >(&input_array, &constants)?;

        // Check that the constraint system is satisfied
        assert!(cs.is_satisfied()?);

        let output_values: Vec<F> = result
            .iter()
            .map(|var: &FpVar<F>| var.value().unwrap())
            .collect();

        // output taken from the plonky3 implementation
        let expected: [F; 8] = [
            F::from(18388235340048743902_u64),
            F::from(11155847389840004280_u64),
            F::from(8258921485236881363_u64),
            F::from(13238911595928314283_u64),
            F::from(1414783942044928333_u64),
            F::from(14855162370750728991_u64),
            F::from(872655314674193689_u64),
            F::from(10410794385812429044_u64),
        ];

        println!("Input: {:?}", input_values);
        println!("Output: {:?}", output_values);
        println!("Constraint system satisfied: {}", cs.is_satisfied()?);
        println!("Number of constraints: {}", cs.num_constraints());

        assert_eq!(output_values, expected);

        Ok(())
    }
}
