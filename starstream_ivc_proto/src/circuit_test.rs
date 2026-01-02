use crate::{prove, test_utils::init_test_logging};
use starstream_mock_ledger::{
    CoroutineState, Hash, InterleavingInstance, MockedLookupTableCommitment, ProcessId, Value,
    WitLedgerEffect,
};

pub fn h<T>(n: u8) -> Hash<T> {
    // TODO: actual hashing
    let mut bytes = [0u8; 32];
    bytes[0] = n;
    Hash(bytes, std::marker::PhantomData)
}

pub fn v(data: &[u8]) -> Value {
    Value(data.to_vec())
}

#[test]
fn test_circuit_simple_resume() {
    init_test_logging();

    let utxo_id = 0;
    let token_id = 1;
    let coord_id = 2;

    let p0 = ProcessId(utxo_id);
    let p1 = ProcessId(token_id);
    let p2 = ProcessId(coord_id);

    let val_0 = v(&[0]);
    let val_1 = v(&[1]);
    let val_4 = v(&[4]);

    let utxo_trace = MockedLookupTableCommitment {
        trace: vec![
            WitLedgerEffect::Init {
                val: val_4.clone(),
                caller: p2,
            },
            WitLedgerEffect::Activation {
                val: val_0.clone(),
                caller: p2,
            },
            WitLedgerEffect::Yield {
                val: val_1.clone(), // Yielding nothing
                ret: None,          // Not expecting to be resumed again
                id_prev: Some(p2),
            },
        ],
    };

    let token_trace = MockedLookupTableCommitment {
        trace: vec![
            WitLedgerEffect::Init {
                val: val_1.clone(),
                caller: p2,
            },
            WitLedgerEffect::Activation {
                val: val_0.clone(),
                caller: p2,
            },
            WitLedgerEffect::Bind { owner_id: p0 },
            WitLedgerEffect::Yield {
                val: val_1.clone(), // Yielding nothing
                ret: None,          // Not expecting to be resumed again
                id_prev: Some(p2),
            },
        ],
    };

    let coord_trace = MockedLookupTableCommitment {
        trace: vec![
            WitLedgerEffect::NewUtxo {
                program_hash: h(0),
                val: val_4.clone(),
                id: p0,
            },
            WitLedgerEffect::NewUtxo {
                program_hash: h(1),
                val: val_1.clone(),
                id: p1,
            },
            WitLedgerEffect::Resume {
                target: p1,
                val: val_0.clone(),
                ret: val_1.clone(),
                id_prev: None,
            },
            WitLedgerEffect::Resume {
                target: p0,
                val: val_0.clone(),
                ret: val_1.clone(),
                id_prev: Some(p1),
            },
        ],
    };

    let traces = vec![
        utxo_trace,
        token_trace,
        coord_trace,
        MockedLookupTableCommitment { trace: vec![] },
        MockedLookupTableCommitment { trace: vec![] },
        MockedLookupTableCommitment { trace: vec![] },
        MockedLookupTableCommitment { trace: vec![] },
        MockedLookupTableCommitment { trace: vec![] },
        MockedLookupTableCommitment { trace: vec![] },
        MockedLookupTableCommitment { trace: vec![] },
    ];

    let trace_lens = traces
        .iter()
        .map(|t| t.trace.len() as u32)
        .collect::<Vec<_>>();

    let instance = InterleavingInstance {
        n_inputs: 0,
        n_new: 2,
        n_coords: 8,
        entrypoint: p2,
        process_table: vec![h(0), h(1), h(2), h(3), h(4), h(5), h(6), h(7), h(8), h(9)],
        is_utxo: vec![
            true, true, false, false, false, false, false, false, false, false,
        ],
        must_burn: vec![
            false, false, false, false, false, false, false, false, false, false,
        ],
        ownership_in: vec![None, None, None, None, None, None, None, None, None, None],
        ownership_out: vec![
            None,
            Some(ProcessId(0)),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        ],
        host_calls_roots: traces,
        host_calls_lens: trace_lens,
        input_states: vec![],
    };

    let result = prove(instance);
    assert!(result.is_ok());
}
