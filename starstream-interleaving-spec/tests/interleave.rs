use starstream_interleaving_spec::{
    ExecutionEvent, ExecutionTrace, InterleavingError, MethodHash, ProcessId, ResourceHandle,
    StarstreamValue, interleave_traces,
};

fn method(value: u64) -> MethodHash {
    MethodHash([value, 0, 0, 0])
}

fn return_control() -> ExecutionEvent {
    ExecutionEvent::ReturnControl {
        result: StarstreamValue::default(),
    }
}

fn enter_method(arguments: impl Into<Vec<u32>>) -> ExecutionEvent {
    ExecutionEvent::EnterMethod {
        arguments: StarstreamValue(arguments.into()),
    }
}

#[test]
fn interleaves_constructor_and_reentered_method_turns() {
    let resource = ResourceHandle(7);
    let coordinator = ExecutionTrace::new([
        ExecutionEvent::NewUtxo {
            arguments: StarstreamValue(vec![55]),
            resource,
        },
        ExecutionEvent::CallMethod {
            resource,
            method: method(1),
            arguments: StarstreamValue(vec![13]),
            result: StarstreamValue::default(),
        },
        ExecutionEvent::CallMethod {
            resource,
            method: method(2),
            arguments: StarstreamValue::default(),
            result: StarstreamValue::default(),
        },
        ExecutionEvent::CoordReturn,
    ]);
    let utxo = ExecutionTrace::new([
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method: method(1) },
        ExecutionEvent::AdvertiseMethod { method: method(2) },
        return_control(),
        enter_method([13]),
        return_control(),
        enter_method([]),
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method: method(3) },
        return_control(),
    ]);

    let merged = interleave_traces(&[coordinator, utxo]).expect("traces should interleave");

    assert_eq!(
        merged,
        ExecutionTrace::new([
            ExecutionEvent::Init,
            ExecutionEvent::NewUtxo {
                arguments: StarstreamValue(vec![55]),
                resource,
            },
            ExecutionEvent::ClearAbi,
            ExecutionEvent::AdvertiseMethod { method: method(1) },
            ExecutionEvent::AdvertiseMethod { method: method(2) },
            return_control(),
            ExecutionEvent::CallMethod {
                resource,
                method: method(1),
                arguments: StarstreamValue(vec![13]),
                result: StarstreamValue::default(),
            },
            enter_method([13]),
            return_control(),
            ExecutionEvent::CallMethod {
                resource,
                method: method(2),
                arguments: StarstreamValue::default(),
                result: StarstreamValue::default(),
            },
            enter_method([]),
            ExecutionEvent::ClearAbi,
            ExecutionEvent::AdvertiseMethod { method: method(3) },
            return_control(),
            ExecutionEvent::CoordReturn,
        ])
    );
}

#[test]
fn resolves_each_caller_local_resource_to_its_constructor_trace() {
    let first_resource = ResourceHandle(3);
    let second_resource = ResourceHandle(9);
    let coordinator = ExecutionTrace::new([
        ExecutionEvent::NewUtxo {
            arguments: StarstreamValue::default(),
            resource: first_resource,
        },
        ExecutionEvent::NewUtxo {
            arguments: StarstreamValue::default(),
            resource: second_resource,
        },
        ExecutionEvent::CallMethod {
            resource: second_resource,
            method: method(2),
            arguments: StarstreamValue::default(),
            result: StarstreamValue::default(),
        },
        ExecutionEvent::CallMethod {
            resource: first_resource,
            method: method(1),
            arguments: StarstreamValue::default(),
            result: StarstreamValue::default(),
        },
        ExecutionEvent::CoordReturn,
    ]);
    let first_utxo = ExecutionTrace::new([
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method: method(1) },
        return_control(),
        enter_method([]),
        ExecutionEvent::CoroutineReturn,
        return_control(),
    ]);
    let second_utxo = ExecutionTrace::new([
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method: method(2) },
        return_control(),
        enter_method([]),
        ExecutionEvent::CoroutineReturn,
        return_control(),
    ]);

    let merged = interleave_traces(&[coordinator, first_utxo, second_utxo])
        .expect("resource bindings should select the matching process");

    let method_turns = merged
        .0
        .windows(4)
        .filter_map(|events| match events {
            [
                ExecutionEvent::CallMethod { resource, .. },
                ExecutionEvent::EnterMethod { .. },
                ExecutionEvent::CoroutineReturn,
                ExecutionEvent::ReturnControl { .. },
            ] => Some(*resource),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(method_turns, [second_resource, first_resource]);
}

#[test]
fn rejects_a_call_when_the_resource_cannot_select_a_trace() {
    let error = interleave_traces(&[ExecutionTrace::new([
        ExecutionEvent::CallMethod {
            resource: ResourceHandle(4),
            method: method(1),
            arguments: StarstreamValue::default(),
            result: StarstreamValue::default(),
        },
        ExecutionEvent::CoordReturn,
    ])])
    .expect_err("an unknown resource leaves the next process ambiguous");

    assert_eq!(
        error,
        InterleavingError::UnknownResource {
            holder: ProcessId(0),
            resource: ResourceHandle(4),
        }
    );
}

#[test]
fn rejects_unassigned_process_traces_even_when_they_are_empty() {
    let error = interleave_traces(&[
        ExecutionTrace::new([ExecutionEvent::CoordReturn]),
        ExecutionTrace::default(),
    ])
    .expect_err("every supplied trace should be assigned");

    assert_eq!(
        error,
        InterleavingError::UnassignedProcessTrace {
            process: ProcessId(1),
        }
    );
}
