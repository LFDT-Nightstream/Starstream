use ark_ff::PrimeField;
use ark_r1cs_std::fields::{
    FieldVar as _,
    fp::{AllocatedFp, FpVar},
};
use ark_relations::gr1cs::{ConstraintSystemRef, LinearCombination, SynthesisError};

#[inline(always)]
fn linear_combination_4<F: PrimeField>(
    coeffs: [F; 4],
    vars: [&FpVar<F>; 4],
) -> Result<FpVar<F>, SynthesisError> {
    let mut sum_constants = F::zero();
    let mut has_value = true;
    let mut value = F::zero();
    let mut cs = ConstraintSystemRef::None;
    let mut lc_terms = Vec::with_capacity(4);

    for (coeff, var) in coeffs.iter().zip(vars.iter()) {
        match var {
            FpVar::Constant(c) => {
                sum_constants += *coeff * *c;
            }
            FpVar::Var(v) => {
                cs = cs.or(v.cs.clone());
                lc_terms.push((*coeff, v.variable));
                if let Ok(v) = v.value() {
                    value += *coeff * v;
                } else {
                    has_value = false;
                }
            }
        }
    }

    if lc_terms.is_empty() {
        return Ok(FpVar::Constant(sum_constants));
    }

    let variable = cs
        .new_lc(|| {
            let mut lc = LinearCombination(lc_terms);
            lc.compactify();
            lc
        })
        .unwrap();
    let value = if has_value { Some(value) } else { None };

    Ok(FpVar::Var(AllocatedFp::new(value, variable, cs)) + FpVar::Constant(sum_constants))
}

/// Multiply a 4-element vector x by:
/// [ 2 3 1 1 ]
/// [ 1 2 3 1 ]
/// [ 1 1 2 3 ]
/// [ 3 1 1 2 ].
#[inline(always)]
fn apply_mat4<F: PrimeField>(x: &mut [FpVar<F>]) -> Result<(), SynthesisError> {
    let vars = [&x[0], &x[1], &x[2], &x[3]];

    let y0 = linear_combination_4([F::from(2u64), F::from(3u64), F::ONE, F::ONE], vars)?;
    let y1 = linear_combination_4([F::ONE, F::from(2u64), F::from(3u64), F::ONE], vars)?;
    let y2 = linear_combination_4([F::ONE, F::ONE, F::from(2u64), F::from(3u64)], vars)?;
    let y3 = linear_combination_4([F::from(3u64), F::ONE, F::ONE, F::from(2u64)], vars)?;

    x[0] = y0;
    x[1] = y1;
    x[2] = y2;
    x[3] = y3;

    Ok(())
}

/// Implement the matrix multiplication used by the external layer.
///
/// Given a 4x4 MDS matrix M, we multiply by the `4N x 4N` matrix
/// `[[2M M  ... M], [M  2M ... M], ..., [M  M ... 2M]]`.
///
/// # Panics
/// This will panic if `WIDTH` is not supported. Currently, the
/// supported `WIDTH` values are 2, 3, 4, 8, 12, 16, 20, 24.`
#[inline(always)]
pub fn mds_light_permutation<const WIDTH: usize, F: PrimeField>(
    state: &mut [FpVar<F>; WIDTH],
) -> Result<(), SynthesisError> {
    match WIDTH {
        2 => {
            let mut sum = state[0].clone();
            sum += &state[1];
            state[0] += &sum;
            state[1] += sum;
        }

        3 => {
            let mut sum = state[0].clone();
            sum += &state[1];
            sum += &state[2];
            state[0] += &sum;
            state[1] += &sum;
            state[2] += sum;
        }

        4 | 8 | 12 | 16 | 20 | 24 => {
            // First, we apply M_4 to each consecutive four elements of the state.
            // In Appendix B's terminology, this replaces each x_i with x_i'.
            for chunk in state.chunks_exact_mut(4) {
                // mdsmat.permute_mut(chunk.try_into().unwrap());
                apply_mat4(chunk)?;
            }
            // Now, we apply the outer circulant matrix (to compute the y_i values).

            // We first precompute the four sums of every four elements.
            let mut sums: [FpVar<F>; 4] = core::array::from_fn(|_| FpVar::zero());
            for j in (0..WIDTH).step_by(4) {
                sums[0] += &state[j];
                sums[1] += &state[j + 1];
                sums[2] += &state[j + 2];
                sums[3] += &state[j + 3];
            }

            // The formula for each y_i involves 2x_i' term and x_j' terms for each j that equals i mod 4.
            // In other words, we can add a single copy of x_i' to the appropriate one of our precomputed sums
            state
                .iter_mut()
                .enumerate()
                .for_each(|(i, elem)| *elem += &sums[i % 4]);
        }

        _ => {
            panic!("Unsupported width");
        }
    }

    Ok(())
}
