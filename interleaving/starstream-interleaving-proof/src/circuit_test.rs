use crate::{logging::setup_logger, prove};
use starstream_interleaving_spec::{
    Hash, InterleavingInstance, InterleavingWitness, LedgerEffectsCommitment, ProcessId, Ref,
    Value, WitEffectOutput, WitLedgerEffect,
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

fn host_calls_roots(traces: &[Vec<WitLedgerEffect>]) -> Vec<LedgerEffectsCommitment> {
    traces
        .iter()
        .map(|trace| {
            trace
                .iter()
                .cloned()
                .fold(LedgerEffectsCommitment::zero(), |acc, op| {
                    crate::commit(acc, op)
                })
        })
        .collect()
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
            val: ref_4.into(),
            caller: p2.into(),
        },
        WitLedgerEffect::Get {
            reff: ref_4,
            offset: 0,
            ret: val_4.clone().into(),
        },
        WitLedgerEffect::Activation {
            val: ref_0.into(),
            caller: p2.into(),
        },
        WitLedgerEffect::GetHandlerFor {
            interface_id: h(100),
            handler_id: p2.into(),
        },
        WitLedgerEffect::Yield {
            val: ref_1.clone(),          // Yielding nothing
            ret: WitEffectOutput::Thunk, // Not expecting to be resumed again
            id_prev: Some(p2).into(),
        },
    ];

    let token_trace = vec![
        WitLedgerEffect::Init {
            val: ref_1.into(),
            caller: p2.into(),
        },
        WitLedgerEffect::Get {
            reff: ref_1,
            offset: 0,
            ret: val_1.clone().into(),
        },
        WitLedgerEffect::Activation {
            val: ref_0.into(),
            caller: p2.into(),
        },
        WitLedgerEffect::Bind { owner_id: p0 },
        WitLedgerEffect::Yield {
            val: ref_1.clone(),          // Yielding nothing
            ret: WitEffectOutput::Thunk, // Not expecting to be resumed again
            id_prev: Some(p2).into(),
        },
    ];

    let coord_trace = vec![
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_0.into(),
        },
        WitLedgerEffect::RefPush { val: val_0 },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_1.into(),
        },
        WitLedgerEffect::RefPush { val: val_1.clone() },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_4.into(),
        },
        WitLedgerEffect::RefPush { val: val_4.clone() },
        WitLedgerEffect::NewUtxo {
            program_hash: h(0),
            val: ref_4,
            id: p0.into(),
        },
        WitLedgerEffect::NewUtxo {
            program_hash: h(1),
            val: ref_1,
            id: p1.into(),
        },
        WitLedgerEffect::Resume {
            target: p1,
            val: ref_0.clone(),
            ret: ref_1.clone().into(),
            id_prev: WitEffectOutput::Resolved(None),
        },
        WitLedgerEffect::InstallHandler {
            interface_id: h(100),
        },
        WitLedgerEffect::Resume {
            target: p0,
            val: ref_0,
            ret: ref_1.into(),
            id_prev: Some(p1).into(),
        },
        WitLedgerEffect::UninstallHandler {
            interface_id: h(100),
        },
    ];

    let traces = vec![utxo_trace, token_trace, coord_trace];

    let trace_lens = traces.iter().map(|t| t.len() as u32).collect::<Vec<_>>();

    let host_calls_roots = host_calls_roots(&traces);

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
        host_calls_roots,
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
        val: ref_0.clone(),          // Yielding nothing
        ret: WitEffectOutput::Thunk, // Not expecting to be resumed again
        id_prev: Some(p1).into(),    // This should be None actually?
    }];

    let coord_trace = vec![
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_0.into(),
        },
        WitLedgerEffect::RefPush { val: val_0 },
        WitLedgerEffect::NewUtxo {
            program_hash: h(0),
            val: ref_0,
            id: p0.into(),
        },
        WitLedgerEffect::Resume {
            target: p0,
            val: ref_0.clone(),
            ret: ref_0.clone().into(),
            id_prev: WitEffectOutput::Resolved(None.into()),
        },
    ];

    let traces = vec![utxo_trace, coord_trace];

    let trace_lens = traces.iter().map(|t| t.len() as u32).collect::<Vec<_>>();

    let host_calls_roots = host_calls_roots(&traces);

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
        host_calls_roots,
        host_calls_lens: trace_lens,
        input_states: vec![],
    };

    let wit = InterleavingWitness { traces };

    let result = prove(instance, wit);
    assert!(result.is_ok());
}

#[test]
#[should_panic]
fn test_circuit_resumer_mismatch() {
    setup_logger();

    let utxo_id = 0;
    let coord_a_id = 1;
    let coord_b_id = 2;

    let p0 = ProcessId(utxo_id);
    let p1 = ProcessId(coord_a_id);
    let p2 = ProcessId(coord_b_id);

    let val_0 = v(&[0]);

    let ref_0 = Ref(0);

    let utxo_trace = vec![WitLedgerEffect::Yield {
        val: ref_0.clone(),
        ret: WitEffectOutput::Thunk,
        id_prev: Some(p1).into(),
    }];

    let coord_a_trace = vec![
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_0.into(),
        },
        WitLedgerEffect::RefPush { val: val_0 },
        WitLedgerEffect::NewUtxo {
            program_hash: h(0),
            val: ref_0,
            id: p0.into(),
        },
        WitLedgerEffect::NewCoord {
            program_hash: h(2),
            val: ref_0,
            id: p2.into(),
        },
        WitLedgerEffect::Resume {
            target: p0,
            val: ref_0.clone(),
            ret: ref_0.clone().into(),
            id_prev: WitEffectOutput::Resolved(None),
        },
        WitLedgerEffect::Resume {
            target: p2,
            val: ref_0.clone(),
            ret: ref_0.clone().into(),
            id_prev: WitEffectOutput::Resolved(None),
        },
    ];

    let coord_b_trace = vec![WitLedgerEffect::Resume {
        target: p0,
        val: ref_0,
        ret: ref_0.into(),
        id_prev: WitEffectOutput::Resolved(None),
    }];

    let traces = vec![utxo_trace, coord_a_trace, coord_b_trace];

    let trace_lens = traces.iter().map(|t| t.len() as u32).collect::<Vec<_>>();

    let host_calls_roots = host_calls_roots(&traces);

    let instance = InterleavingInstance {
        n_inputs: 0,
        n_new: 1,
        n_coords: 2,
        entrypoint: p1,
        process_table: vec![h(0), h(1), h(2)],
        is_utxo: vec![true, false, false],
        must_burn: vec![false, false, false],
        ownership_in: vec![None, None, None],
        ownership_out: vec![None, None, None],
        host_calls_roots,
        host_calls_lens: trace_lens,
        input_states: vec![],
    };

    let wit = InterleavingWitness { traces };

    let result = prove(instance, wit);
    assert!(result.is_err());
}
