//! Cooperative scheduling of per-instance semantic traces.

use std::collections::HashMap;

use crate::{MethodHash, OutputUtxo, ResourceHandle, Step, Trace, TransactionStatement};

/// Index in the supplied trace list, not a guest-visible handle.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ProcessIndex(pub usize);

/// A transaction trace with the statement it must be checked against.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InterleavedTransaction {
    /// Execution-only trace, ending at the entrypoint's return.
    pub execution: Trace,
    /// The full transaction: execution, then the finalization scan.
    pub trace: Trace,
    /// Synthesized outputs; inputs are not supported yet.
    pub statement: TransactionStatement,
}

/// Merge process-local semantic traces in cooperative control-flow order.
///
/// `traces[0]` is the coordinator. The caller must supply subsequent traces in
/// `NewUtxo` allocation order; loading inputs via `SetStorage` isn't supported yet.
/// Handles are bound on constructor return. All steps must be consumed when
/// the coordinator returns; use [`interleave_transaction`] for storage reads.
pub fn interleave_execution_only(traces: &[Trace]) -> Result<Trace, InterleavingError> {
    let scheduled = schedule_execution(traces)?;
    for (process, (cursor, trace)) in scheduled.cursors.iter().zip(traces).enumerate() {
        if *cursor != trace.0.len() {
            return Err(InterleavingError::UnconsumedSteps {
                process: ProcessIndex(process),
                next_step_index: *cursor,
                remaining: trace.0.len() - *cursor,
            });
        }
    }
    Ok(Trace(scheduled.merged))
}

/// Schedule execution, then finalize UTXOs in allocation order.
/// Live UTXOs must have one trailing `GetStorage`; consumed UTXOs have none.
/// ABI reads and transaction completion are synthesized. Inputs aren't supported.
pub fn interleave_transaction(
    traces: &[Trace],
) -> Result<InterleavedTransaction, InterleavingError> {
    let Scheduled {
        mut merged,
        cursors,
        allocation_order,
        registrations,
    } = schedule_execution(traces)?;
    if cursors[0] != traces[0].0.len() {
        return Err(InterleavingError::UnconsumedSteps {
            process: ProcessIndex(0),
            next_step_index: cursors[0],
            remaining: traces[0].0.len() - cursors[0],
        });
    }
    let execution = Trace(merged.clone());

    let mut outputs = Vec::new();
    for (utxo, &process) in allocation_order.iter().enumerate() {
        let remaining = &traces[process].0[cursors[process]..];
        let methods = &registrations[process];
        if methods.is_empty() {
            if !remaining.is_empty() {
                return Err(InterleavingError::UnconsumedSteps {
                    process: ProcessIndex(process),
                    next_step_index: cursors[process],
                    remaining: remaining.len(),
                });
            }
            merged.push(Step::SkipConsumed);
            continue;
        }
        let [Step::GetStorage { storage }] = remaining else {
            return Err(InterleavingError::MissingStorageRead {
                process: ProcessIndex(process),
                remaining: remaining.len(),
            });
        };
        merged.push(Step::GetStorage {
            storage: storage.clone(),
        });
        merged.extend(methods.iter().map(|&method| Step::ReadAbi { method }));
        outputs.push(OutputUtxo {
            utxo: u32::try_from(utxo).map_err(|_| InterleavingError::TooManyUtxos)?,
            storage: storage.0.clone(),
            methods: methods.clone(),
        });
    }
    merged.push(Step::FinalizeCoordinator);
    merged.push(Step::FinishTransaction);

    Ok(InterleavedTransaction {
        execution,
        trace: Trace(merged),
        statement: TransactionStatement {
            inputs: Vec::new(),
            outputs,
        },
    })
}

struct Scheduled {
    merged: Vec<Step>,
    cursors: Vec<usize>,
    /// Process index of each allocated UTXO, by UTXO id.
    allocation_order: Vec<usize>,
    /// Final-generation method registrations of each process.
    registrations: Vec<Vec<MethodHash>>,
}

fn schedule_execution(traces: &[Trace]) -> Result<Scheduled, InterleavingError> {
    if traces.is_empty() {
        return Err(InterleavingError::MissingEntrypoint);
    }

    let step_count = traces
        .iter()
        .fold(0_usize, |count, trace| count.saturating_add(trace.0.len()));
    let mut merged = Vec::with_capacity(step_count);

    let mut cursors = vec![0_usize; traces.len()];
    let mut registrations = vec![Vec::new(); traces.len()];
    let mut allocation_order = Vec::with_capacity(traces.len().saturating_sub(1));
    let mut current = 0_usize;
    let mut next_process = 1_usize;
    let mut call_stack = Vec::new();
    let mut pending_constructors = HashMap::<usize, (usize, ResourceHandle)>::new();
    let mut resource_targets = HashMap::<(usize, ResourceHandle), usize>::new();

    loop {
        let step_index = cursors[current];
        let Some(step) = traces[current].0.get(step_index).cloned() else {
            return Err(InterleavingError::UnexpectedEndOfTrace {
                process: ProcessIndex(current),
            });
        };
        cursors[current] += 1;

        match step {
            Step::NewUtxo { resource, .. } => {
                let resource = resource.0;
                if let Some(&(target, _)) = pending_constructors.get(&current) {
                    return Err(InterleavingError::ConstructorAlreadyPending {
                        caller: ProcessIndex(current),
                        target: ProcessIndex(target),
                    });
                }
                if next_process >= traces.len() {
                    return Err(InterleavingError::MissingUtxoTrace {
                        caller: ProcessIndex(current),
                        step_index,
                    });
                }

                let target = next_process;
                next_process += 1;
                allocation_order.push(target);
                pending_constructors.insert(current, (target, resource));
                call_stack.push(current);
                merged.push(step);
                current = target;
            }
            Step::CallMethod { resource, .. } => {
                let Some(&target) = resource_targets.get(&(current, resource)) else {
                    return Err(InterleavingError::UnknownResource {
                        holder: ProcessIndex(current),
                        resource,
                    });
                };
                call_stack.push(current);
                merged.push(step);
                current = target;
            }
            Step::Return { .. } => {
                let returning = current;
                merged.push(step);
                let Some(caller) = call_stack.pop() else {
                    if current != 0 {
                        return Err(InterleavingError::ReturnWithoutCaller {
                            process: ProcessIndex(current),
                        });
                    }
                    // The entrypoint returned to the transaction context.
                    break;
                };
                if let Some(&(target, resource)) = pending_constructors.get(&caller)
                    && target == returning
                {
                    pending_constructors.remove(&caller);
                    resource_targets.insert((caller, resource), target);
                }
                current = caller;
            }
            Step::YieldBegin => {
                registrations[current].clear();
                merged.push(step);
            }
            Step::RegisterMethod { method } => {
                registrations[current].push(method);
                merged.push(step);
            }
            Step::EnterConstructor { .. } | Step::EnterMethod { .. } => merged.push(step),
            Step::GetStorage { .. }
            | Step::SetStorage { .. }
            | Step::PreloadMethod { .. }
            | Step::ReadAbi { .. }
            | Step::SkipConsumed
            | Step::FinalizeCoordinator
            | Step::FinishTransaction => {
                return Err(InterleavingError::UnexpectedTransactionStep {
                    process: ProcessIndex(current),
                    step_index,
                });
            }
        }
    }

    if next_process != traces.len() {
        return Err(InterleavingError::UnassignedProcessTrace {
            process: ProcessIndex(next_process),
        });
    }

    Ok(Scheduled {
        merged,
        cursors,
        allocation_order,
        registrations,
    })
}

/// Scheduling failure. Quint or the circuit must still validate the merged trace.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum InterleavingError {
    #[error("no entrypoint coordination-script trace was supplied")]
    MissingEntrypoint,

    #[error("process {process:?} exhausted its trace before the transaction returned")]
    UnexpectedEndOfTrace { process: ProcessIndex },

    #[error("process {caller:?} began a constructor while process {target:?} was still pending")]
    ConstructorAlreadyPending {
        caller: ProcessIndex,
        target: ProcessIndex,
    },

    #[error(
        "process {caller:?} began a constructor at local step {step_index}, \
         but no unassigned process trace remains"
    )]
    MissingUtxoTrace {
        caller: ProcessIndex,
        step_index: usize,
    },

    #[error("process {holder:?} called through unknown caller-local resource {resource:?}")]
    UnknownResource {
        holder: ProcessIndex,
        resource: ResourceHandle,
    },

    #[error("non-entrypoint process {process:?} returned control without a waiting caller")]
    ReturnWithoutCaller { process: ProcessIndex },

    #[error(
        "process {process:?} has {remaining} unconsumed step(s), starting at local step \
         {next_step_index}"
    )]
    UnconsumedSteps {
        process: ProcessIndex,
        next_step_index: usize,
        remaining: usize,
    },

    #[error("process trace {process:?} was never assigned by a constructor call")]
    UnassignedProcessTrace { process: ProcessIndex },

    #[error(
        "process {process:?} emitted a transaction-boundary step at local step {step_index} \
         during execution"
    )]
    UnexpectedTransactionStep {
        process: ProcessIndex,
        step_index: usize,
    },

    #[error(
        "live process {process:?} must end with exactly its storage read, found {remaining} \
         remaining step(s)"
    )]
    MissingStorageRead {
        process: ProcessIndex,
        remaining: usize,
    },

    #[error("more UTXOs were allocated than the statement can number")]
    TooManyUtxos,
}

#[cfg(test)]
mod tests {
    use crate::{Out, StarstreamValue};

    use super::*;

    fn method(value: u32) -> MethodHash {
        MethodHash([value, 0, 0, 0, 0, 0, 0, 0])
    }

    fn value(words: impl Into<Vec<u32>>) -> StarstreamValue {
        StarstreamValue::from(words.into())
    }

    fn ret() -> Step {
        Step::Return {
            result: Out(StarstreamValue::default()),
        }
    }

    fn enter_method(method: MethodHash, arguments: impl Into<Vec<u32>>) -> Step {
        Step::EnterMethod {
            method,
            arguments: value(arguments),
        }
    }

    fn enter_constructor(arguments: impl Into<Vec<u32>>) -> Step {
        Step::EnterConstructor {
            arguments: value(arguments),
        }
    }

    fn new_utxo(arguments: impl Into<Vec<u32>>, resource: ResourceHandle) -> Step {
        Step::NewUtxo {
            arguments: value(arguments),
            resource: Out(resource),
        }
    }

    fn call_method(
        resource: ResourceHandle,
        method: MethodHash,
        arguments: impl Into<Vec<u32>>,
    ) -> Step {
        Step::CallMethod {
            resource,
            method,
            arguments: value(arguments),
            result: Out(StarstreamValue::default()),
        }
    }

    fn register(method: MethodHash) -> Step {
        Step::RegisterMethod { method }
    }

    fn get_storage(words: impl Into<Vec<u32>>) -> Step {
        Step::GetStorage {
            storage: Out(value(words)),
        }
    }

    fn transaction_traces() -> Vec<Trace> {
        let live = ResourceHandle(7);
        let consumed = ResourceHandle(8);
        let coordinator = Trace::new([
            new_utxo([1], live),
            new_utxo([2], consumed),
            call_method(live, method(9), [13]),
            ret(),
        ]);
        let live_utxo = Trace::new([
            enter_constructor([1]),
            register(method(9)),
            ret(),
            enter_method(method(9), [13]),
            Step::YieldBegin,
            register(method(1)),
            register(method(2)),
            ret(),
            get_storage([21, 22]),
        ]);
        let consumed_utxo = Trace::new([enter_constructor([2]), ret()]);

        vec![coordinator, live_utxo, consumed_utxo]
    }

    #[test]
    fn transaction_finalizes_live_and_consumed_utxos() {
        let transaction = interleave_transaction(&transaction_traces()).unwrap();
        assert_eq!(transaction.execution.0.len(), 14);
        assert_eq!(
            transaction.trace.0[14..],
            [
                get_storage([21, 22]),
                Step::ReadAbi { method: method(1) },
                Step::ReadAbi { method: method(2) },
                Step::SkipConsumed,
                Step::FinalizeCoordinator,
                Step::FinishTransaction,
            ]
        );
        assert_eq!(
            transaction.statement,
            TransactionStatement {
                inputs: vec![],
                outputs: vec![OutputUtxo {
                    utxo: 0,
                    storage: value([21, 22]),
                    methods: vec![method(1), method(2)],
                }],
            }
        );
    }

    #[test]
    fn transaction_requires_exactly_one_storage_read_per_live_utxo() {
        let resource = ResourceHandle(7);
        let coordinator = Trace::new([new_utxo([1], resource), ret()]);
        let without_read = Trace::new([enter_constructor([1]), register(method(1)), ret()]);
        assert_eq!(
            interleave_transaction(&[coordinator.clone(), without_read]),
            Err(InterleavingError::MissingStorageRead {
                process: ProcessIndex(1),
                remaining: 0
            })
        );
        let consumed_with_read = Trace::new([enter_constructor([1]), ret(), get_storage([1])]);
        assert_eq!(
            interleave_transaction(&[coordinator, consumed_with_read]),
            Err(InterleavingError::UnconsumedSteps {
                process: ProcessIndex(1),
                next_step_index: 2,
                remaining: 1
            })
        );
    }

    #[test]
    fn execution_rejects_transaction_boundary_steps() {
        let coordinator = Trace::new([Step::FinishTransaction, ret()]);
        assert_eq!(
            interleave_execution_only(&[coordinator]),
            Err(InterleavingError::UnexpectedTransactionStep {
                process: ProcessIndex(0),
                step_index: 0
            })
        );
    }

    fn reentered_method_traces() -> Vec<Trace> {
        let resource = ResourceHandle(7);
        let coordinator = Trace::new([
            new_utxo([55], resource),
            call_method(resource, method(1), [13]),
            call_method(resource, method(2), []),
            ret(),
        ]);
        let utxo = Trace::new([
            enter_constructor([55]),
            Step::RegisterMethod { method: method(1) },
            Step::RegisterMethod { method: method(2) },
            ret(),
            enter_method(method(1), [13]),
            ret(),
            enter_method(method(2), []),
            Step::YieldBegin,
            Step::RegisterMethod { method: method(3) },
            ret(),
        ]);

        vec![coordinator, utxo]
    }

    #[test]
    fn interleaves_constructor_and_reentered_method_turns() {
        let resource = ResourceHandle(7);
        let merged = interleave_execution_only(&reentered_method_traces())
            .expect("traces should interleave");

        assert_eq!(
            merged,
            Trace::new([
                new_utxo([55], resource),
                enter_constructor([55]),
                Step::RegisterMethod { method: method(1) },
                Step::RegisterMethod { method: method(2) },
                ret(),
                call_method(resource, method(1), [13]),
                enter_method(method(1), [13]),
                ret(),
                call_method(resource, method(2), []),
                enter_method(method(2), []),
                Step::YieldBegin,
                Step::RegisterMethod { method: method(3) },
                ret(),
                ret(),
            ])
        );
    }

    fn two_utxo_traces() -> Vec<Trace> {
        let coordinator = Trace::new([
            new_utxo([1], ResourceHandle(0)),
            new_utxo([2], ResourceHandle(1)),
            call_method(ResourceHandle(1), method(2), []),
            call_method(ResourceHandle(0), method(1), []),
            ret(),
        ]);
        let first = Trace::new([
            enter_constructor([1]),
            Step::RegisterMethod { method: method(1) },
            ret(),
            enter_method(method(1), []),
            ret(),
        ]);
        let second = Trace::new([
            enter_constructor([2]),
            Step::RegisterMethod { method: method(2) },
            ret(),
            enter_method(method(2), []),
            ret(),
        ]);

        vec![coordinator, first, second]
    }

    #[test]
    fn binds_each_constructor_handle_to_its_own_process() {
        let merged = interleave_execution_only(&two_utxo_traces()).expect("traces interleave");

        assert_eq!(
            merged.0[8..],
            [
                call_method(ResourceHandle(1), method(2), []),
                enter_method(method(2), []),
                ret(),
                call_method(ResourceHandle(0), method(1), []),
                enter_method(method(1), []),
                ret(),
                ret(),
            ]
        );
    }

    #[test]
    #[ignore = "requires Quint; run through npm test"]
    fn positive_interleavings_satisfy_quint() {
        let verifier = crate::QuintVerifier::new().unwrap();
        for (name, traces) in [
            ("reentered methods", reentered_method_traces()),
            ("separate handle bindings", two_utxo_traces()),
        ] {
            let merged = interleave_execution_only(&traces).unwrap();
            verifier
                .verify(&merged)
                .unwrap_or_else(|error| panic!("{name}: {error}"));
        }
        let transaction = interleave_transaction(&transaction_traces()).unwrap();
        verifier.verify(&transaction.execution).unwrap();
        verifier
            .verify_transaction(&transaction.trace, &transaction.statement)
            .unwrap();
    }

    #[test]
    fn rejects_calls_through_unbound_resources() {
        let coordinator = Trace::new([call_method(ResourceHandle(3), method(1), []), ret()]);

        assert_eq!(
            interleave_execution_only(&[coordinator]),
            Err(InterleavingError::UnknownResource {
                holder: ProcessIndex(0),
                resource: ResourceHandle(3),
            })
        );
    }

    #[test]
    fn rejects_constructors_without_a_process_trace() {
        let coordinator = Trace::new([new_utxo([], ResourceHandle(0)), ret()]);

        assert_eq!(
            interleave_execution_only(&[coordinator]),
            Err(InterleavingError::MissingUtxoTrace {
                caller: ProcessIndex(0),
                step_index: 0,
            })
        );
    }

    #[test]
    fn rejects_unassigned_traces() {
        let coordinator = Trace::new([ret()]);
        let utxo = Trace::new([enter_constructor([]), ret()]);
        assert_eq!(
            interleave_execution_only(&[coordinator, utxo]),
            Err(InterleavingError::UnassignedProcessTrace {
                process: ProcessIndex(1),
            })
        );
    }

    #[test]
    fn rejects_an_entrypoint_that_never_returns() {
        let coordinator = Trace::new([new_utxo([], ResourceHandle(0))]);
        let utxo = Trace::new([enter_constructor([]), ret()]);

        assert_eq!(
            interleave_execution_only(&[coordinator, utxo]),
            Err(InterleavingError::UnexpectedEndOfTrace {
                process: ProcessIndex(0),
            })
        );
    }

    #[test]
    fn rejects_unconsumed_trailing_utxo_return() {
        let coordinator = Trace::new([new_utxo([], ResourceHandle(0)), ret()]);
        let utxo = Trace::new([enter_constructor([]), ret(), ret()]);

        assert_eq!(
            interleave_execution_only(&[coordinator, utxo]),
            Err(InterleavingError::UnconsumedSteps {
                process: ProcessIndex(1),
                next_step_index: 2,
                remaining: 1,
            })
        );
    }
}
