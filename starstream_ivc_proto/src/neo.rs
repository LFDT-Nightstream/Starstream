use crate::goldilocks::FpGoldilocks;
use ark_ff::{Field, PrimeField};
use ark_relations::gr1cs::ConstraintSystemRef;
use neo::{CcsStructure, F, IndexExtractor, ivc::StepBindingSpec};
use p3_field::PrimeCharacteristicRing;

pub(crate) struct NeoStep {
    pub(crate) ccs: CcsStructure<F>,
    // instance + witness assignments
    pub(crate) witness: Vec<F>,
    // only input assignments
    pub(crate) input: Vec<F>,
    pub(crate) step_binding_step: StepBindingSpec,
    pub(crate) output_extractor: IndexExtractor,
}

pub(crate) fn arkworks_to_neo(cs: ConstraintSystemRef<FpGoldilocks>) -> NeoStep {
    cs.finalize();

    let matrices = &cs.to_matrices().unwrap()["R1CS"];

    dbg!(cs.num_constraints());
    dbg!(cs.num_instance_variables());
    dbg!(cs.num_witness_variables());

    let a_mat = ark_matrix_to_neo(&cs, &matrices[0]);
    let b_mat = ark_matrix_to_neo(&cs, &matrices[1]);
    let c_mat = ark_matrix_to_neo(&cs, &matrices[2]);

    let ccs = neo_ccs::r1cs_to_ccs(a_mat, b_mat, c_mat);

    let instance_assignment = cs.instance_assignment().unwrap();
    assert_eq!(instance_assignment[0], FpGoldilocks::ONE);
    assert_eq!(instance_assignment.len() % 2, 1);

    // NOTE: this is not inherent to arkworks, it's just how the circuit is
    // constructed (see circuit.rs/ivcify_wires).
    //
    // input-output pairs are allocated contiguously
    //
    // we start from 1 to skip the 1 constant
    let input_assignments: Vec<_> = instance_assignment[1..]
        .iter()
        .step_by(2)
        .map(ark_field_to_p3_goldilocks)
        .collect();

    let instance = cs
        .instance_assignment()
        .unwrap()
        .iter()
        .map(ark_field_to_p3_goldilocks)
        .collect::<Vec<_>>();

    let witness = cs
        .witness_assignment()
        .unwrap()
        .iter()
        .map(ark_field_to_p3_goldilocks)
        .collect::<Vec<_>>();

    let binding_spec = StepBindingSpec {
        // indices of output variables (we allocate contiguous input, output
        // pairs for ivc).
        //
        // see circuit.rs/ivcify_wires
        y_step_offsets: (2..)
            .step_by(2)
            .take(input_assignments.len())
            .collect::<Vec<_>>(),

        // TODO: what's this for?
        x_witness_indices: vec![],
        // indices of input variables (we allocate contiguous input, output
        // pairs for ivc)
        //
        // see circuit.rs/ivcify_wires
        y_prev_witness_indices: (1..)
            .step_by(2)
            .take(input_assignments.len())
            .collect::<Vec<_>>(),

        const1_witness_index: 0,
    };

    let output_extractor = IndexExtractor {
        indices: (2..)
            .step_by(2)
            .take(input_assignments.len())
            .collect::<Vec<_>>(),
    };

    NeoStep {
        ccs,
        witness: [instance, witness].concat(),
        input: input_assignments,
        step_binding_step: binding_spec,
        output_extractor,
    }
}

fn ark_matrix_to_neo(
    cs: &ConstraintSystemRef<FpGoldilocks>,
    sparse_matrix: &[Vec<(FpGoldilocks, usize)>],
) -> neo_ccs::Mat<F> {
    let n_rows = cs.num_constraints();
    let n_cols = cs.num_variables();

    // TODO: would be nice to just be able to construct the sparse matrix
    let mut dense = vec![F::from_u64(0); n_rows * n_cols];

    for (row_i, row) in sparse_matrix.iter().enumerate() {
        for (col_v, col_i) in row.iter() {
            dense[n_cols * row_i + col_i] = ark_field_to_p3_goldilocks(col_v);
        }
    }

    neo_ccs::Mat::from_row_major(n_rows, n_cols, dense)
}

fn ark_field_to_p3_goldilocks(col_v: &FpGoldilocks) -> p3_goldilocks::Goldilocks {
    F::from_u64(col_v.into_bigint().0[0])
}
