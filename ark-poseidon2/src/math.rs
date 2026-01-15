use ark_ff::PrimeField;
use ark_r1cs_std::fields::{FieldVar as _, fp::FpVar};
use ark_relations::gr1cs::SynthesisError;

/// Multiply a 4-element vector x by:
/// [ 2 3 1 1 ]
/// [ 1 2 3 1 ]
/// [ 1 1 2 3 ]
/// [ 3 1 1 2 ].
#[inline(always)]
fn apply_mat4<F: PrimeField>(x: &mut [FpVar<F>]) -> Result<(), SynthesisError> {
    let t01 = x[0].clone() + &x[1];
    let t23 = x[2].clone() + &x[3];
    let t0123 = t01.clone() + &t23;
    let t01123 = t0123.clone() + &x[1];
    let t01233 = t0123 + &x[3];

    // The order here is important. Need to overwrite x[0] and x[2] after x[1] and x[3].
    x[3] = t01233.clone() + &x[0].double()?; // 3*x[0] + x[1] + x[2] + 2*x[3]
    x[1] = t01123.clone() + &x[2].double()?; // x[0] + 2*x[1] + 3*x[2] + x[3]
    x[0] = t01123 + &t01; // 2*x[0] + 3*x[1] + x[2] + x[3]
    x[2] = t01233 + &t23; // x[0] + x[1] + 2*x[2] + 3*x[3]

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
            let sum = state[0].clone() + state[1].clone();
            state[0] += sum.clone();
            state[1] += sum;
        }

        3 => {
            let sum = state[0].clone() + state[1].clone() + state[2].clone();
            state[0] += sum.clone();
            state[1] += sum.clone();
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
            let sums: [FpVar<F>; 4] =
                core::array::from_fn(|k| (0..WIDTH).step_by(4).map(|j| state[j + k].clone()).sum());

            // The formula for each y_i involves 2x_i' term and x_j' terms for each j that equals i mod 4.
            // In other words, we can add a single copy of x_i' to the appropriate one of our precomputed sums
            state
                .iter_mut()
                .enumerate()
                .for_each(|(i, elem)| *elem += sums[i % 4].clone());
        }

        _ => {
            panic!("Unsupported width");
        }
    }

    Ok(())
}
