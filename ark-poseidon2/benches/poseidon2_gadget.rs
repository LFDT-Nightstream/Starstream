use ark_poseidon2::{
    F, RoundConstants,
    constants::GOLDILOCKS_S_BOX_DEGREE,
    gadget::Poseidon2Gadget,
    linear_layers::{GoldilocksExternalLinearLayer, GoldilocksInternalLinearLayer8},
};
use ark_r1cs_std::{GR1CSVar as _, alloc::AllocVar as _, fields::fp::FpVar};
use ark_relations::gr1cs::ConstraintSystem;
use criterion::{Criterion, black_box, criterion_group, criterion_main};

const WIDTH: usize = 8;
const HALF_FULL_ROUNDS: usize = 4;
const PARTIAL_ROUNDS: usize = 22;

fn bench_poseidon2_gadget_inc(c: &mut Criterion) {
    c.bench_function("poseidon2_gadget_inc", |b| {
        b.iter(|| {
            let cs = ConstraintSystem::<F>::new_ref();

            let constants = RoundConstants::new_goldilocks_8_constants();

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
                .collect::<Result<Vec<_>, _>>()
                .unwrap();
            let input_array: [FpVar<F>; WIDTH] = input_vars.try_into().unwrap();

            let gadget = Poseidon2Gadget::<
                F,
                GoldilocksExternalLinearLayer<8>,
                GoldilocksInternalLinearLayer8,
                WIDTH,
                GOLDILOCKS_S_BOX_DEGREE,
                HALF_FULL_ROUNDS,
                PARTIAL_ROUNDS,
            >::new(constants);
            let result = gadget.permute(&input_array).unwrap();

            let output_values: Vec<F> = result
                .iter()
                .map(|var: &FpVar<F>| var.value().unwrap())
                .collect();
            black_box(output_values);
        });
    });
}

criterion_group!(benches, bench_poseidon2_gadget_inc);
criterion_main!(benches);
