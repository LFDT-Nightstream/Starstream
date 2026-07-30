use std::collections::HashMap;

use crate::{ExecutionEvent, ExecutionTrace, ProcessId, ResourceHandle};

/// Merge process-local semantic traces in cooperative control-flow order.
///
/// `traces[0]` is the transaction entrypoint coordination script. Each
/// subsequent trace is assigned to the next `BeginNewUtxo` in constructor
/// order. A constructor return binds its caller-local resource handle to that
/// assigned process, allowing later `CallMethod` events to select the correct
/// trace.
///
/// The local traces omit the transaction-level [`ExecutionEvent::Init`], so
/// the merger prepends it to the canonical execution trace.
pub fn interleave_traces(traces: &[ExecutionTrace]) -> Result<ExecutionTrace, InterleavingError> {
    if traces.is_empty() {
        return Err(InterleavingError::MissingEntrypoint);
    }

    let event_count = traces
        .iter()
        .fold(1_usize, |count, trace| count.saturating_add(trace.0.len()));
    let mut merged = Vec::with_capacity(event_count);
    merged.push(ExecutionEvent::Init);

    let mut cursors = vec![0_usize; traces.len()];
    let mut current = 0_usize;
    let mut next_process = 1_usize;
    let mut call_stack = Vec::new();
    let mut pending_constructors = HashMap::<usize, usize>::new();
    let mut resource_targets = HashMap::<(usize, ResourceHandle), usize>::new();

    loop {
        let event_index = cursors[current];
        let Some(event) = traces[current].0.get(event_index).cloned() else {
            return Err(InterleavingError::TraceExhausted {
                process: process_id(current),
            });
        };
        cursors[current] += 1;
        merged.push(event.clone());

        match event {
            ExecutionEvent::Init => {
                return Err(InterleavingError::UnexpectedInit {
                    process: process_id(current),
                    event_index,
                });
            }
            ExecutionEvent::BeginNewUtxo { .. } => {
                if let Some(&target) = pending_constructors.get(&current) {
                    return Err(InterleavingError::ConstructorAlreadyPending {
                        caller: process_id(current),
                        target: process_id(target),
                    });
                }
                if next_process >= traces.len() {
                    return Err(InterleavingError::MissingConstructorTrace {
                        caller: process_id(current),
                        event_index,
                    });
                }

                let target = next_process;
                next_process += 1;
                pending_constructors.insert(current, target);
                call_stack.push(current);
                current = target;
            }
            ExecutionEvent::NewUtxoReturn { resource } => {
                let Some(target) = pending_constructors.remove(&current) else {
                    return Err(InterleavingError::UnexpectedConstructorReturn {
                        caller: process_id(current),
                        resource,
                    });
                };
                let key = (current, resource);
                if resource_targets.contains_key(&key) {
                    return Err(InterleavingError::DuplicateResourceBinding {
                        holder: process_id(current),
                        resource,
                    });
                }
                resource_targets.insert(key, target);
            }
            ExecutionEvent::CallMethod { resource, .. } => {
                let Some(&target) = resource_targets.get(&(current, resource)) else {
                    return Err(InterleavingError::UnknownResource {
                        holder: process_id(current),
                        resource,
                    });
                };
                call_stack.push(current);
                current = target;
            }
            ExecutionEvent::ReturnControl => {
                let Some(caller) = call_stack.pop() else {
                    return Err(InterleavingError::ReturnWithoutCaller {
                        process: process_id(current),
                    });
                };
                current = caller;
            }
            ExecutionEvent::CoordReturn => {
                if current != 0 {
                    return Err(InterleavingError::CoordReturnOutsideEntrypoint {
                        process: process_id(current),
                    });
                }
                if let Some(&caller) = call_stack.last() {
                    return Err(InterleavingError::CoordReturnWithWaitingCaller {
                        caller: process_id(caller),
                    });
                }
                break;
            }
            ExecutionEvent::ClearAbi
            | ExecutionEvent::AdvertiseMethod { .. }
            | ExecutionEvent::CoroutineReturn => {}
        }
    }

    if next_process != traces.len() {
        return Err(InterleavingError::UnassignedProcessTrace {
            process: process_id(next_process),
        });
    }
    for (process, (cursor, trace)) in cursors.iter().zip(traces).enumerate() {
        if *cursor != trace.0.len() {
            return Err(InterleavingError::UnconsumedEvents {
                process: process_id(process),
                next_event_index: *cursor,
                remaining: trace.0.len() - *cursor,
            });
        }
    }

    Ok(ExecutionTrace(merged))
}

fn process_id(index: usize) -> ProcessId {
    ProcessId(index as u64)
}

/// A structural failure that prevents selecting the next process-local trace.
///
/// Semantic protocol failures that do not make scheduling ambiguous remain in
/// the merged trace and are rejected by the Quint specification.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum InterleavingError {
    #[error("no entrypoint coordination-script trace was supplied")]
    MissingEntrypoint,

    #[error("process {process:?} exhausted its trace before the transaction returned")]
    TraceExhausted { process: ProcessId },

    #[error(
        "process {process:?} contains an unexpected Init event at local event {event_index}; \
         Init is synthesized by the merger"
    )]
    UnexpectedInit {
        process: ProcessId,
        event_index: usize,
    },

    #[error("process {caller:?} began a constructor while process {target:?} was still pending")]
    ConstructorAlreadyPending {
        caller: ProcessId,
        target: ProcessId,
    },

    #[error(
        "process {caller:?} began a constructor at local event {event_index}, \
         but no unassigned process trace remains"
    )]
    MissingConstructorTrace {
        caller: ProcessId,
        event_index: usize,
    },

    #[error("process {caller:?} returned resource {resource:?} without a pending constructor")]
    UnexpectedConstructorReturn {
        caller: ProcessId,
        resource: ResourceHandle,
    },

    #[error("process {holder:?} returned already-bound caller-local resource {resource:?}")]
    DuplicateResourceBinding {
        holder: ProcessId,
        resource: ResourceHandle,
    },

    #[error("process {holder:?} called through unknown caller-local resource {resource:?}")]
    UnknownResource {
        holder: ProcessId,
        resource: ResourceHandle,
    },

    #[error("process {process:?} returned control without a waiting caller")]
    ReturnWithoutCaller { process: ProcessId },

    #[error("non-entrypoint process {process:?} emitted CoordReturn")]
    CoordReturnOutsideEntrypoint { process: ProcessId },

    #[error("the entrypoint returned while caller {caller:?} was still waiting")]
    CoordReturnWithWaitingCaller { caller: ProcessId },

    #[error(
        "process {process:?} has {remaining} unconsumed event(s), starting at local event \
         {next_event_index}"
    )]
    UnconsumedEvents {
        process: ProcessId,
        next_event_index: usize,
        remaining: usize,
    },

    #[error("process trace {process:?} was never assigned by a constructor call")]
    UnassignedProcessTrace { process: ProcessId },
}
