use super::*;
use crate::{
    Error, TraceCommitments, Unsatisfied, build_witness_rows, ivc_state::CoroutineId,
    verify_sat_with_commitments, verify_witness_rows,
};
use starstream_interleaving_spec::{StarstreamValue, Step};

const COORDINATOR: CoroutineId = CoroutineId::Coord(1);
const UTXO: CoroutineId = CoroutineId::Utxo(0);

#[test]
fn output_selection_handles_every_block_count() {
    let normalized = crate::step::normalize(&crate::tests::method_call_trace(true));
    let padding = normalized.steps[0].padding_after();
    let relation = crate::ccs::build_relation().unwrap();
    let check = |row: &[F]| {
        neo_ccs::check_ccs_rowwise_zero(
            relation.r1cs().structure(),
            &row[..PUBLIC_INPUTS],
            &row[PUBLIC_INPUTS..],
        )
    };
    for (count, step) in [
        (0, &padding),
        (1, &normalized.steps[0]),
        (2, &normalized.steps[2]),
        (3, &normalized.steps[4]),
    ] {
        let mut row = crate::witness::build_witness_vector(step);
        // Nonzero input also tests padding's passthrough, not just zero=zero.
        for (i, column) in COL_IN.into_iter().enumerate() {
            row[column] = F::new(17 + i as u64);
        }
        assign_from_bus(&mut row, step.opcode);
        range_check_layout().assign_bits(&mut row).unwrap();
        check(&row).unwrap();
        let mut chain = COL_IN.map(|c| row[c]);
        for block in 0..3 {
            let g = gadget(block);
            chain = neo_application::event_commitment::commit_block(chain, g.block.map(|c| row[c]));
            assert_eq!(g.output.map(|c| row[c]), chain);
        }
        assert_eq!(
            COL_OUT.map(|c| row[c]),
            output_columns(count).map(|c| row[c])
        );

        // Pick the wrong prefix, but keep its limb encoding and canonicality
        // advice valid: rejection must come from the final output selection.
        let wrong = output_columns((count + 1) % 4).map(|c| row[c]);
        assert_ne!(wrong, COL_OUT.map(|c| row[c]));
        for i in 0..4 {
            row[COL_OUT[i]] = wrong[i];
            let word = wrong[i].as_canonical_u64();
            row[COL_OUT_WORDS[2 * i]] = F::new(word & u64::from(u32::MAX));
            let hi = F::new(word >> 32);
            row[COL_OUT_WORDS[2 * i + 1]] = hi;
            let delta = hi - F::new(u64::from(u32::MAX));
            row[COL_CANONICAL_HIGH_MAX[4 + i]] = if delta == F::ZERO { F::ONE } else { F::ZERO };
            row[COL_CANONICAL_HIGH_INV[4 + i]] = delta.try_inverse().unwrap_or(F::ZERO);
        }
        range_check_layout().assign_bits(&mut row).unwrap();
        let Err(neo_ccs::CcsError::RowFail { row }) = check(&row) else {
            panic!("wrong prefix accepted")
        };
        assert_eq!(
            relation.r1cs().catalog().rows()[row].tag().label(),
            "event commitment"
        );
    }
}

fn expected(
    trace: &starstream_interleaving_spec::Trace,
    owners: &[CoroutineId],
) -> TraceCommitments {
    // Callers supply attribution independently of the normalizer.
    assert_eq!(
        trace.0.len(),
        owners.len(),
        "one owner is required per event"
    );
    let mut result = TraceCommitments::new();
    for (step, owner) in trace.0.iter().zip(owners) {
        let owner = owner.encoded();
        let mut chain = result.get(&owner).copied().unwrap_or([0; 4]).map(F::new);
        for block in starstream_interleaving_spec::events::encode(step) {
            chain = neo_application::event_commitment::commit_block(chain, block.map(F::new));
        }
        result.insert(owner, chain.map(|x| x.as_canonical_u64()));
    }
    result
}

#[test]
fn multi_block_encoding_and_statement_are_bound() {
    let trace = crate::tests::method_call_trace(true);
    let blocks = starstream_interleaving_spec::events::encode(&trace.0[4]);
    // EventSequenceBuilder packing: tag+resource+six method limbs; remaining
    // method limbs+argument root; second root must start a new block.
    assert_eq!(
        blocks,
        vec![[6, 0, 1, 0, 1, 0, 1, 0], [1, 0, 1, 2, 3, 4, 0, 0], [0; 8],]
    );
    let statement = expected(
        &trace,
        &[
            COORDINATOR,
            UTXO,
            UTXO,
            UTXO,
            COORDINATOR,
            UTXO,
            UTXO,
            COORDINATOR,
        ],
    );
    for size in [1, 3, 5, 8] {
        verify_sat_with_commitments(&trace, size, &statement).unwrap();
    }
    for lane in 0..4 {
        let mut wrong = statement.clone();
        wrong.get_mut(&UTXO.encoded()).unwrap()[lane] ^= 1;
        assert!(matches!(
            verify_sat_with_commitments(&trace, 3, &wrong),
            Err(Error::Unsatisfied(Unsatisfied::TraceCommitments))
        ));
    }
    let mut missing = statement.clone();
    missing.remove(&UTXO.encoded());
    assert!(verify_sat_with_commitments(&trace, 3, &missing).is_err());
    let mut extra = statement.clone();
    extra.insert(CoroutineId::Coord(0).encoded(), [0; 4]);
    assert!(verify_sat_with_commitments(&trace, 3, &extra).is_err());
    let mut swapped = statement.clone();
    swapped.insert(UTXO.encoded(), statement[&COORDINATOR.encoded()]);
    swapped.insert(COORDINATOR.encoded(), statement[&UTXO.encoded()]);
    assert!(verify_sat_with_commitments(&trace, 3, &swapped).is_err());
}

#[test]
fn changed_values_and_order_cannot_reuse_a_statement() {
    let trace = crate::tests::method_call_trace(true);
    let statement = expected(
        &trace,
        &[
            COORDINATOR,
            UTXO,
            UTXO,
            UTXO,
            COORDINATOR,
            UTXO,
            UTXO,
            COORDINATOR,
        ],
    );
    let mut changed = trace.clone();
    if let Step::CallMethod { arguments, .. } = &mut changed.0[4] {
        arguments.0[3] += 1;
    }
    if let Step::EnterMethod { arguments, .. } = &mut changed.0[5] {
        arguments.0[3] += 1;
    }
    crate::verify_sat(&changed).unwrap(); // matching sender/receiver, valid semantics
    assert!(verify_sat_with_commitments(&changed, 3, &statement).is_err());
    // Both registration orders are legal, but their order is observable.
    let mut first = trace.clone();
    first.0.insert(
        3,
        Step::RegisterMethod {
            method: starstream_interleaving_spec::MethodHash([9, 0, 9, 0, 9, 0, 9, 0]),
        },
    );
    let first_statement = expected(
        &first,
        &[
            COORDINATOR,
            UTXO,
            UTXO,
            UTXO,
            UTXO,
            COORDINATOR,
            UTXO,
            UTXO,
            COORDINATOR,
        ],
    );
    verify_sat_with_commitments(&first, 3, &first_statement).unwrap();
    first.0.swap(2, 3);
    crate::verify_sat(&first).unwrap();
    assert!(verify_sat_with_commitments(&first, 3, &first_statement).is_err());
}

#[test]
fn tampered_compression_and_canonical_aliases_are_rejected() {
    let trace = crate::tests::method_call_trace(true);
    for column in [
        COL_EVENT_BLOCKS[8],
        COL_EVENT_HASHES[0],
        COL_EVENT_AUX[0],
        COL_OUT[0],
        COL_IN_WORDS[0],
    ] {
        let (mut rows, preload) = build_witness_rows(&trace);
        rows[4][column] += F::ONE;
        range_check_layout().assign_bits(&mut rows[4]).unwrap();
        assert!(
            verify_witness_rows(&rows, &preload).is_err(),
            "column {column}"
        );
    }
    // p reconstructs to zero in the field but must not alias the zero root.
    let mut alias = trace.clone();
    if let Step::NewUtxo { arguments, .. } = &mut alias.0[0] {
        *arguments = StarstreamValue([0xffff_ffff_0000_0001, 0, 0, 0]);
    }
    if let Step::EnterConstructor { arguments } = &mut alias.0[1] {
        *arguments = StarstreamValue([0xffff_ffff_0000_0001, 0, 0, 0]);
    }
    assert!(matches!(
        crate::verify_sat(&alias),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            constraint: "canonical event roots",
            ..
        }))
    ));
}

#[test]
fn full_field_roots_survive_ram_and_batching() {
    let root = StarstreamValue([0xffff_ffff_0000_0000, 1 << 50, (1 << 32) + 1, 0]);
    let trace = crate::tests::method_call_trace_with_values(true, root.clone(), root.clone(), root);
    let statement = expected(
        &trace,
        &[
            COORDINATOR,
            UTXO,
            UTXO,
            UTXO,
            COORDINATOR,
            UTXO,
            UTXO,
            COORDINATOR,
        ],
    );
    for size in [1, 3, 8] {
        verify_sat_with_commitments(&trace, size, &statement).unwrap();
    }
}
