use crate::{logging::setup_logger, prove};
use starstream_mock_ledger::{
    Hash, InterleavingInstance, InterleavingWitness, MockedLookupTableCommitment, ProcessId, Ref,
    Value, WitLedgerEffect,
};

pub fn h<T>(n: u8) -> Hash<T> {
    // TODO: actual hashing
    let mut bytes = [0u8; 32];
    bytes[0] = n;
    Hash(bytes, std::marker::PhantomData)
}

pub fn v(data: &[u8]) -> Value {
    let mut bytes = [0u8; 8];
    let len = data.len().min(8);
    bytes[..len].copy_from_slice(&data[..len]);
    Value(u64::from_le_bytes(bytes))
}

#[test]
fn test_circuit_many_steps() {
    setup_logger();

    let utxo_id = 0;
    let token_id = 1;
    let coord_id = 2;

    let p0 = ProcessId(utxo_id);
    let p1 = ProcessId(token_id);
    let p2 = ProcessId(coord_id);

    let val_0 = v(&[0]);
    let val_1 = v(&[1]);
    let val_4 = v(&[4]);

    let ref_0 = Ref(0);
    let ref_1 = Ref(1);
    let ref_4 = Ref(2);

    let utxo_trace = vec![
        WitLedgerEffect::Init {
            val: ref_4,
            caller: p2,
        },
        WitLedgerEffect::Get {
            reff: ref_4,
            offset: 0,
            ret: val_4.clone(),
        },
        WitLedgerEffect::Activation {
            val: ref_0,
            caller: p2,
        },
        WitLedgerEffect::GetHandlerFor {
            interface_id: h(100),
            handler_id: p2,
        },
        WitLedgerEffect::Yield {
            val: ref_1.clone(), // Yielding nothing
            ret: None,          // Not expecting to be resumed again
            id_prev: Some(p2),
        },
    ];

    let token_trace = vec![
        WitLedgerEffect::Init {
            val: ref_1,
            caller: p2,
        },
        WitLedgerEffect::Get {
            reff: ref_1,
            offset: 0,
            ret: val_1.clone(),
        },
        WitLedgerEffect::Activation {
            val: ref_0,
            caller: p2,
        },
        WitLedgerEffect::Bind { owner_id: p0 },
        WitLedgerEffect::Yield {
            val: ref_1.clone(), // Yielding nothing
            ret: None,          // Not expecting to be resumed again
            id_prev: Some(p2),
        },
    ];

    let coord_trace = vec![
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_0,
        },
        WitLedgerEffect::RefPush { val: val_0 },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_1,
        },
        WitLedgerEffect::RefPush { val: val_1.clone() },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_4,
        },
        WitLedgerEffect::RefPush { val: val_4.clone() },
        WitLedgerEffect::NewUtxo {
            program_hash: h(0),
            val: ref_4,
            id: p0,
        },
        WitLedgerEffect::NewUtxo {
            program_hash: h(1),
            val: ref_1,
            id: p1,
        },
        WitLedgerEffect::Resume {
            target: p1,
            val: ref_0.clone(),
            ret: ref_1.clone(),
            id_prev: None,
        },
        WitLedgerEffect::InstallHandler {
            interface_id: h(100),
        },
        WitLedgerEffect::Resume {
            target: p0,
            val: ref_0,
            ret: ref_1,
            id_prev: Some(p1),
        },
        WitLedgerEffect::UninstallHandler {
            interface_id: h(100),
        },
    ];

    let traces = vec![utxo_trace, token_trace, coord_trace];

    let trace_lens = traces.iter().map(|t| t.len() as u32).collect::<Vec<_>>();

    let instance = InterleavingInstance {
        n_inputs: 0,
        n_new: 2,
        n_coords: 8,
        entrypoint: p2,
        process_table: vec![h(0), h(1), h(2)],
        is_utxo: vec![true, true, false],
        must_burn: vec![false, false, false],
        ownership_in: vec![None, None, None],
        ownership_out: vec![None, Some(ProcessId(0)), None],
        host_calls_roots: vec![MockedLookupTableCommitment(0); traces.len()],
        host_calls_lens: trace_lens,
        input_states: vec![],
    };

    let wit = InterleavingWitness { traces };

    let result = prove(instance, wit);
    assert!(result.is_ok());
}

#[test]
fn test_circuit_small() {
    setup_logger();

    let utxo_id = 0;
    let coord_id = 1;

    let p0 = ProcessId(utxo_id);
    let p1 = ProcessId(coord_id);

    let val_0 = v(&[0]);

    let ref_0 = Ref(0);

    let utxo_trace = vec![WitLedgerEffect::Yield {
        val: ref_0.clone(), // Yielding nothing
        ret: None,          // Not expecting to be resumed again
        id_prev: Some(p1),
    }];

    let coord_trace = vec![
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_0,
        },
        WitLedgerEffect::RefPush { val: val_0 },
        WitLedgerEffect::NewUtxo {
            program_hash: h(0),
            val: ref_0,
            id: p0,
        },
        WitLedgerEffect::Resume {
            target: p0,
            val: ref_0.clone(),
            ret: ref_0.clone(),
            id_prev: None,
        },
    ];

    let traces = vec![utxo_trace, coord_trace];

    let trace_lens = traces.iter().map(|t| t.len() as u32).collect::<Vec<_>>();

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
        host_calls_roots: vec![MockedLookupTableCommitment(0); traces.len()],
        host_calls_lens: trace_lens,
        input_states: vec![],
    };

    let wit = InterleavingWitness { traces };

    let result = prove(instance, wit);
    assert!(result.is_ok());
}
