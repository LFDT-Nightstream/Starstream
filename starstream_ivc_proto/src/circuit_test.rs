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
    let coord_id = 1;

    let p0 = ProcessId(utxo_id);
    let p1 = ProcessId(coord_id);

    let val_42 = v(b"v42");
    let val_0 = v(b"v0");

    let coord_trace = MockedLookupTableCommitment {
        trace: vec![
            WitLedgerEffect::NewUtxo {
                program_hash: h(0),
                val: val_0.clone(),
                id: p0,
            },
            WitLedgerEffect::Resume {
                target: p0,
                val: val_42.clone(),
                ret: val_0.clone(),
                id_prev: None,
            },
        ],
    };

    let utxo_trace = MockedLookupTableCommitment {
        trace: vec![
            WitLedgerEffect::Input {
                val: val_42.clone(),
                caller: p0,
            },
            // WitLedgerEffect::Yield {
            //     val: val_0.clone(), // Yielding nothing
            //     ret: None,          // Not expecting to be resumed again
            //     id_prev: Some(p0),
            // },
        ],
    };

    let traces = vec![utxo_trace, coord_trace];

    let trace_lens = traces
        .iter()
        .map(|t| t.trace.len() as u32)
        .collect::<Vec<_>>();

    let instance = InterleavingInstance {
        n_inputs: 0,
        n_new: 1,
        n_coords: 1,
        entrypoint: p1,
        process_table: vec![h(0), h(1)],
        is_utxo: vec![true, false],
        must_burn: vec![false, false],
        ownership_in: vec![None, None],
        ownership_out: vec![None, None],
        host_calls_roots: traces,
        host_calls_lens: trace_lens,
        input_states: vec![],
    };

    let result = prove(instance);
    assert!(result.is_ok());
}
