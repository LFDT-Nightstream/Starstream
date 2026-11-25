use std::collections::{BTreeMap, BTreeSet};

use neo_application::{
    MemoryActivationFacts, MemoryCatalog, MemoryCheckPolicy, MemoryKind, MemoryPortActivation,
    MemoryPortKind, MemoryPortSpec, MemoryPreload, MemorySpec, RamInitialization,
    plan_ordered_memory_slots,
};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;

use crate::ccs::layout::{
    COL_CALL_FRAME_SAVED_EFFECT_DEPTH, COL_CALL_FRAME_SAVED_EFFECT_HEAD,
    COL_EFFECT_ARENA_LEN_AFTER, COL_EFFECT_FRAME_ID, COL_EFFECT_HEAD_AFTER, COL_EFFECT_HEAD_BEFORE,
    COL_EFFECT_REGISTERED, COL_EFFECT_SCOPE_READ, COL_INSTANCE_DEPTH_AFTER,
    COL_INSTANCE_DEPTH_BEFORE, COL_INSTANCE_PUSH, COL_SEL_ADD_EFFECT_HANDLER,
    COL_SEL_PUSH_EFFECT_HANDLER_FRAME, COL_SEL_RAISE_EFFECT, COL_SEL_SKIP_EFFECT_HANDLER,
};
use crate::opcode::Opcode;

use crate::ccs::layout::{
    COL_ABI_GENERATION_ADDR, COL_ABI_GENERATION_AFTER, COL_ABI_GENERATION_BEFORE,
    COL_ABI_METHOD_COUNT_ADDR, COL_ABI_METHOD_COUNT_AFTER, COL_ABI_METHOD_COUNT_BEFORE,
    COL_ABI_METHOD_COUNT_READ, COL_ABI_METHOD_COUNT_WRITE, COL_CALL_FRAME_CONSTRUCTOR,
    COL_CALL_FRAME_IS_EFFECT, COL_CALL_FRAME_SAVED_PHASE, COL_CALL_STACK_EXPECTED_ARG_VALUE,
    COL_CALL_STACK_FRAME, COL_CALL_STACK_MUL_STRIDE_8, COL_CALL_STACK_POP, COL_CALL_STACK_PUSH,
    COL_CALL_TARGET, COL_CURR_BEFORE, COL_ENABLED_METHOD_LOG_ADDR,
    COL_ENABLED_METHOD_LOG_GENERATION, COL_ENABLED_METHOD_LOG_ORDINAL, COL_ENABLED_METHOD_LOG_UTXO,
    COL_EVENT_OWNER_STRIDE_8, COL_IN_WORDS, COL_METHOD_APPEND, COL_METHOD_HASH_VALUE,
    COL_METHOD_INDEX, COL_METHOD_LOOKUP, COL_METHOD_TABLE_ADDR, COL_ONE, COL_OUT_WORDS,
    COL_RESOURCE_RESOLVER_ADDR_CID, COL_RESOURCE_RESOLVER_ADDR_HANDLE, COL_RESOURCE_RESOLVER_READ,
    COL_RESOURCE_RESOLVER_VALUE, COL_RESOURCE_RESOLVER_WRITE, COL_SEL_CALL_METHOD,
    COL_SEL_ENTER_EFFECT_HANDLER, COL_SEL_EXIT_EFFECT_HANDLER, COL_SEL_READ_ABI,
    COL_SEL_RECEIVE_CALL_RESULT, COL_SEL_RECEIVE_NEW_UTXO_HANDLE, COL_SEL_RESUME_EFFECT,
    COL_SEL_RETURN, COL_SEL_YIELD_BEGIN, COL_SIGNER_INDEX, COL_SIGNER_KEY_ADDR, SIGNER_KEY_LIMBS,
    range_check_layout,
};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum MemoryId {
    InstanceStack,
    CallStackCaller,
    CallStackEffectArgument,
    CallStackEffectIndex,
    CallStackIsEffect,
    CallStackConstructor,
    CallStackSavedPhase,
    CallStackSavedEffectHead,
    CallStackSavedEffectDepth,
    EffectScopeParent,
    EffectScopeOwner,
    EffectScopeFrameId,
    EffectRegistered,
    AbiMethodCount,
    AbiGeneration,
    EnabledMethodLogUtxo,
    EnabledMethodLogMethod,
    EnabledMethodLogGeneration,
    EnabledMethodLogOrdinal,
    MethodTable,
    SignerKey,
    SignerPresent,
    ResourceResolver,
    TokenTraceCommitments,
    UtxoTraceCommitments,
    CoordTraceCommitments,
}

/// Activation facts of the single-step relation, read from its own rows.
///
/// The support atoms are the opcode selectors, which the one-hot row makes
/// pairwise exclusive. A selector gate supports itself. Any other gate gets a
/// support from one of two row shapes:
///
/// - `gate = sum of selectors`: the support is those selectors.
/// - `gate = base * factor`, with `base` a gate that has a support: the
///   product is 1 only when `base` is, so it inherits that support.
///
/// A gate with neither row has no support, and the planner then never shares
/// its ports. Two product gates of the same base whose factors add up to one
/// cannot both be 1; these pairs are the exclusions.
///
/// A wrong fact cannot make the binding accept a bad row: the binding
/// requires at most one active port per slot, so it would reject an honest
/// row instead.
fn activation_facts<Owner>(
    relation: &neo_application::R1csRelation<Owner>,
    catalog: &MemoryCatalog<MemoryId>,
) -> MemoryActivationFacts {
    use crate::ccs::layout::{COL_ONE, SELECTORS};
    let rows = relation.catalog().rows();

    // filtering rows that repeat a column (since there is no reason to do that
    // and should not happen) to simplify the filtering below
    for row in rows.iter().map(|tagged| tagged.row()) {
        for side in [row.a_terms(), row.b_terms(), row.c_terms()] {
            let columns: BTreeSet<usize> = side.iter().map(|(column, _)| *column).collect();
            assert_eq!(columns.len(), side.len(), "a row repeats a column");
        }
    }

    let sum_support = |gate: usize| -> Option<BTreeSet<usize>> {
        if SELECTORS.contains(&gate) {
            return Some(BTreeSet::from([gate]));
        }
        rows.iter().map(|tagged| tagged.row()).find_map(|row| {
            if row.b_terms() != [(COL_ONE, F::ONE)] || !row.c_terms().is_empty() {
                return None;
            }
            let &(_, sign) = row.a_terms().iter().find(|(column, _)| *column == gate)?;
            if sign != F::ONE && sign != -F::ONE {
                return None;
            }
            let atoms: BTreeSet<usize> = row
                .a_terms()
                .iter()
                .filter(|(column, _)| *column != gate)
                .map(|&(column, value)| {
                    (SELECTORS.contains(&column) && value == -sign).then_some(column)
                })
                .collect::<Option<_>>()?;
            Some(atoms)
        })
    };
    // The base and factor of the row `base * factor = gate`.
    let product = |gate: usize| -> Option<(usize, BTreeMap<usize, F>)> {
        rows.iter().map(|tagged| tagged.row()).find_map(|row| {
            let (&[(base, base_coefficient)], &[(column, coefficient)]) =
                (row.a_terms(), row.c_terms())
            else {
                return None;
            };
            let factor: BTreeMap<usize, F> = row.b_terms().iter().copied().collect();
            (column == gate && coefficient == F::ONE && base_coefficient == F::ONE)
                .then_some((base, factor))
        })
    };
    let gates: BTreeSet<usize> = catalog
        .entries()
        .iter()
        .flat_map(|memory| &memory.ports)
        .filter_map(|port| match port.activation {
            MemoryPortActivation::When(gate) => Some(gate),
            MemoryPortActivation::Always | MemoryPortActivation::Unless(_) => None,
        })
        .collect();
    let products: BTreeMap<usize, (usize, BTreeMap<usize, F>)> = gates
        .iter()
        .filter_map(|&gate| Some((gate, product(gate)?)))
        .collect();
    let supports = gates
        .iter()
        .filter_map(|&gate| {
            let support = sum_support(gate)
                .or_else(|| products.get(&gate).and_then(|(base, _)| sum_support(*base)))?;
            Some((gate, support))
        })
        .collect();
    let complementary = |a: &BTreeMap<usize, F>, b: &BTreeMap<usize, F>| {
        let mut sum = a.clone();
        for (&column, &value) in b {
            *sum.entry(column).or_insert(F::ZERO) += value;
        }
        sum.retain(|_, value| *value != F::ZERO);
        sum == BTreeMap::from([(COL_ONE, F::ONE)])
    };
    let gate_exclusions = products
        .iter()
        .flat_map(|(&a, (base_a, factor_a))| {
            products
                .range(a + 1..)
                .filter(move |(_, (base_b, factor_b))| {
                    base_a == base_b
                        && sum_support(*base_a).is_some()
                        && complementary(factor_a, factor_b)
                })
                .map(move |(&b, _)| (a, b))
        })
        .collect();
    MemoryActivationFacts {
        supports,
        gate_exclusions,
    }
}

pub(crate) fn planned_slots(
    batch_size: std::num::NonZeroUsize,
) -> Result<Vec<Vec<usize>>, crate::batch::proving::ProvingError> {
    let relation = crate::ccs::build_relation()?;
    let catalog = build_memory_layout();
    let plan = plan_ordered_memory_slots(&catalog, &activation_facts(relation.r1cs(), &catalog));
    Ok(nightstream::nebula::batched_slots(
        &catalog,
        &plan.slots,
        batch_size,
    )?)
}

pub fn build_memory_layout() -> MemoryCatalog<MemoryId> {
    let mut memories = vec![];

    memories.extend_from_slice(&call_stack_layout());
    memories.push(MemorySpec {
        id: MemoryId::InstanceStack,
        kind: MemoryKind::Ram,
        ports: vec![
            MemoryPortSpec {
                address_columns: vec![COL_INSTANCE_DEPTH_AFTER],
                value_column: COL_CALL_TARGET,
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_INSTANCE_PUSH),
            },
            MemoryPortSpec {
                address_columns: vec![COL_INSTANCE_DEPTH_BEFORE],
                value_column: COL_RESOURCE_RESOLVER_VALUE,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(COL_SEL_RECEIVE_NEW_UTXO_HANDLE),
            },
        ],
    });

    memories.extend_from_slice(&effect_scope_layout());

    memories.extend_from_slice(&abi_method_count_layout());

    memories.extend_from_slice(&abi_generation_layout());

    memories.extend_from_slice(&enabled_method_log_layout());

    memories.extend_from_slice(&method_table_layout());
    let signer_read = |address, value| MemoryPortSpec {
        address_columns: vec![address],
        value_column: value,
        kind: MemoryPortKind::Read,
        activation: MemoryPortActivation::When(Opcode::AssertSignedBy.selector()),
    };
    memories.push(MemorySpec {
        id: MemoryId::SignerKey,
        kind: MemoryKind::Rom,
        ports: COL_SIGNER_KEY_ADDR
            .into_iter()
            .zip(COL_METHOD_HASH_VALUE)
            .map(|(address, value)| signer_read(address, value))
            .collect(),
    });
    memories.push(MemorySpec {
        id: MemoryId::SignerPresent,
        kind: MemoryKind::Rom,
        ports: vec![signer_read(COL_SIGNER_INDEX, COL_ONE)],
    });

    memories.extend_from_slice(&resource_resolver_layout());

    memories.extend_from_slice(&trace_commitments_layout());

    MemoryCatalog::new(memories, range_check_layout().columns()).expect("valid memory declarations")
}

/// Append-only `try` scope arena. A node is stored at its id + 1, so the
/// encoded head is also its address and zero is never allocated. Ids are not
/// reused, which lets registrations stay in RAM without cleanup on pop.
fn effect_scope_layout() -> [MemorySpec<MemoryId>; 4] {
    let field = |id, written, read: Option<(usize, usize)>| MemorySpec {
        id,
        kind: MemoryKind::Ram,
        ports: std::iter::once(MemoryPortSpec {
            address_columns: vec![COL_EFFECT_ARENA_LEN_AFTER],
            value_column: written,
            kind: MemoryPortKind::Write {
                value_before_column: None,
            },
            activation: MemoryPortActivation::When(COL_SEL_PUSH_EFFECT_HANDLER_FRAME),
        })
        .chain(read.map(|(value_column, activation)| MemoryPortSpec {
            address_columns: vec![COL_EFFECT_HEAD_BEFORE],
            value_column,
            kind: MemoryPortKind::Read,
            activation: MemoryPortActivation::When(activation),
        }))
        .collect(),
    };
    let registered = |activation| MemoryPortSpec {
        // The method index is the trace-local intern of the effect hash.
        address_columns: vec![COL_EFFECT_HEAD_BEFORE, COL_METHOD_INDEX],
        value_column: COL_EFFECT_REGISTERED,
        kind: MemoryPortKind::Read,
        activation: MemoryPortActivation::When(activation),
    };
    [
        // Pushing links the new node to the current head.
        field(
            MemoryId::EffectScopeParent,
            COL_EFFECT_HEAD_BEFORE,
            Some((COL_EFFECT_HEAD_AFTER, COL_EFFECT_SCOPE_READ)),
        ),
        field(
            MemoryId::EffectScopeOwner,
            COL_CURR_BEFORE,
            Some((COL_CALL_TARGET, COL_SEL_ENTER_EFFECT_HANDLER)),
        ),
        field(
            MemoryId::EffectScopeFrameId,
            COL_EFFECT_FRAME_ID,
            Some((COL_EFFECT_FRAME_ID, COL_SEL_ENTER_EFFECT_HANDLER)),
        ),
        MemorySpec {
            id: MemoryId::EffectRegistered,
            kind: MemoryKind::Ram,
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_EFFECT_HEAD_BEFORE, COL_METHOD_INDEX],
                    value_column: COL_EFFECT_REGISTERED,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_SEL_ADD_EFFECT_HANDLER),
                },
                registered(COL_SEL_ENTER_EFFECT_HANDLER),
                registered(COL_SEL_SKIP_EFFECT_HANDLER),
            ],
        },
    ]
}

fn trace_commitments_layout() -> [MemorySpec<MemoryId>; 3] {
    use crate::ccs::layout::{
        COL_COORD_EVENT, COL_SEL_FINALIZE_COORDINATOR, COL_SEL_SKIP_CONSUMED, COL_UTXO_EVENT,
    };
    let memory = |id, event, read: Option<usize>| MemorySpec {
        id,
        kind: MemoryKind::Ram,
        ports: COL_EVENT_OWNER_STRIDE_8
            .into_iter()
            .zip(COL_OUT_WORDS)
            .zip(COL_IN_WORDS)
            .map(|((address, value_after), value_before)| MemoryPortSpec {
                address_columns: vec![address],
                value_column: value_after,
                kind: MemoryPortKind::Write {
                    value_before_column: Some(value_before),
                },
                activation: MemoryPortActivation::When(event),
            })
            .chain(read.into_iter().flat_map(|read| {
                COL_EVENT_OWNER_STRIDE_8.into_iter().zip(COL_IN_WORDS).map(
                    move |(address, value)| MemoryPortSpec {
                        address_columns: vec![address],
                        value_column: value,
                        kind: MemoryPortKind::Read,
                        activation: MemoryPortActivation::When(read),
                    },
                )
            }))
            .collect(),
    };
    [
        memory(
            MemoryId::TokenTraceCommitments,
            crate::ccs::layout::COL_TOKEN_LOAD,
            None,
        ),
        memory(
            MemoryId::UtxoTraceCommitments,
            COL_UTXO_EVENT,
            Some(COL_SEL_SKIP_CONSUMED),
        ),
        memory(
            MemoryId::CoordTraceCommitments,
            COL_COORD_EVENT,
            Some(COL_SEL_FINALIZE_COORDINATOR),
        ),
    ]
}

fn resource_resolver_layout() -> [MemorySpec<MemoryId>; 1] {
    let address_columns = vec![
        COL_RESOURCE_RESOLVER_ADDR_CID,
        COL_RESOURCE_RESOLVER_ADDR_HANDLE,
    ];

    let value_column = COL_RESOURCE_RESOLVER_VALUE;

    [MemorySpec {
        id: MemoryId::ResourceResolver,
        kind: MemoryKind::Ram,
        ports: vec![
            MemoryPortSpec {
                address_columns: address_columns.clone(),
                value_column,
                kind: MemoryPortKind::Write {
                    // We check that we are not overwriting an existing mapping,
                    // so we use the value before and constrain it to Zero
                    //
                    // that works because utxo ids are always greater than 0 in
                    // our encoding
                    value_before_column: Some(crate::ccs::layout::COL_RESOURCE_RESOLVER_BEFORE),
                },
                activation: MemoryPortActivation::When(COL_RESOURCE_RESOLVER_WRITE),
            },
            MemoryPortSpec {
                address_columns,
                value_column,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(COL_RESOURCE_RESOLVER_READ),
            },
        ],
    }]
}

fn abi_generation_layout() -> [MemorySpec<MemoryId>; 1] {
    // A separate registration count tracks this generation's size, including duplicates.
    [MemorySpec {
        id: MemoryId::AbiGeneration,
        kind: MemoryKind::Ram,
        ports: vec![
            MemoryPortSpec {
                address_columns: vec![COL_ABI_GENERATION_ADDR],
                value_column: COL_ABI_GENERATION_AFTER,
                kind: MemoryPortKind::Write {
                    value_before_column: Some(COL_ABI_GENERATION_BEFORE),
                },
                activation: MemoryPortActivation::When(COL_SEL_YIELD_BEGIN),
            },
            MemoryPortSpec {
                address_columns: vec![COL_ABI_GENERATION_ADDR],
                value_column: COL_ABI_GENERATION_BEFORE,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(crate::ccs::layout::COL_ABI_GENERATION_READ),
            },
        ],
    }]
}

fn enabled_method_log_layout() -> [MemorySpec<MemoryId>; 4] {
    // TODO(perf): Packing bounded UTXO, method, and generation indices may
    // reduce both scan cells and operation slots once the prover's concrete
    // limits are known. Keep parallel 32-bit RAMs until then.
    [
        MemorySpec {
            id: MemoryId::EnabledMethodLogOrdinal,
            kind: MemoryKind::Ram,
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_ENABLED_METHOD_LOG_ADDR],
                    value_column: COL_ENABLED_METHOD_LOG_ORDINAL,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_METHOD_APPEND),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_ENABLED_METHOD_LOG_ADDR],
                    value_column: COL_ENABLED_METHOD_LOG_ORDINAL,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_READ_ABI),
                },
            ],
        },
        enabled_method_log_component(MemoryId::EnabledMethodLogUtxo, COL_ENABLED_METHOD_LOG_UTXO),
        enabled_method_log_component(MemoryId::EnabledMethodLogMethod, COL_METHOD_INDEX),
        enabled_method_log_component(
            MemoryId::EnabledMethodLogGeneration,
            COL_ENABLED_METHOD_LOG_GENERATION,
        ),
    ]
}

fn enabled_method_log_component(id: MemoryId, value_column: usize) -> MemorySpec<MemoryId> {
    MemorySpec {
        id,
        kind: MemoryKind::Ram,
        ports: vec![
            MemoryPortSpec {
                address_columns: vec![COL_ENABLED_METHOD_LOG_ADDR],
                value_column,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(COL_SEL_READ_ABI),
            },
            MemoryPortSpec {
                address_columns: vec![COL_ENABLED_METHOD_LOG_ADDR],
                value_column,
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_METHOD_APPEND),
            },
            MemoryPortSpec {
                address_columns: vec![COL_ENABLED_METHOD_LOG_ADDR],
                value_column,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(COL_SEL_CALL_METHOD),
            },
        ],
    }
}

fn method_table_layout() -> [MemorySpec<MemoryId>; 1] {
    [MemorySpec {
        id: MemoryId::MethodTable,
        kind: MemoryKind::Rom,
        ports: COL_METHOD_TABLE_ADDR
            .into_iter()
            .zip(COL_METHOD_HASH_VALUE)
            .map(|(address, value)| MemoryPortSpec {
                address_columns: vec![address],
                value_column: value,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(COL_METHOD_LOOKUP),
            })
            .collect(),
    }]
}

fn abi_method_count_layout() -> [MemorySpec<MemoryId>; 1] {
    // Once every call has returned, a nonzero count means the UTXO is live.
    // Fresh UTXO IDs start at zero through RAM initialization.
    [MemorySpec {
        id: MemoryId::AbiMethodCount,
        kind: MemoryKind::Ram,
        ports: vec![
            MemoryPortSpec {
                address_columns: vec![COL_ABI_METHOD_COUNT_ADDR],
                value_column: COL_ABI_METHOD_COUNT_AFTER,
                kind: MemoryPortKind::Write {
                    value_before_column: Some(COL_ABI_METHOD_COUNT_BEFORE),
                },
                activation: MemoryPortActivation::When(COL_ABI_METHOD_COUNT_WRITE),
            },
            MemoryPortSpec {
                address_columns: vec![COL_ABI_METHOD_COUNT_ADDR],
                value_column: COL_ABI_METHOD_COUNT_BEFORE,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(COL_ABI_METHOD_COUNT_READ),
            },
        ],
    }]
}

fn call_stack_layout() -> [MemorySpec<MemoryId>; 8] {
    [
        MemorySpec {
            id: MemoryId::CallStackCaller,
            kind: MemoryKind::Ram,
            // Pushes select the next free frame and peeks/pops the current top.
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_FRAME],
                    value_column: COL_CURR_BEFORE,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_FRAME],
                    value_column: COL_CALL_TARGET,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_CALL_STACK_POP),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_FRAME],
                    value_column: COL_CALL_TARGET,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RETURN),
                },
            ],
        },
        // Program calls hand their argument, method and result to the next
        // step through the carried hand-off registers. A raised effect is
        // different: a handler that declines returns to the search, so its
        // argument and identity must outlive the handler bodies in between.
        MemorySpec {
            id: MemoryId::CallStackEffectArgument,
            kind: MemoryKind::Ram,
            ports: COL_CALL_STACK_EXPECTED_ARG_VALUE
                .into_iter()
                .zip(COL_CALL_STACK_MUL_STRIDE_8)
                .flat_map(|(value, address)| {
                    [
                        MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Write {
                                value_before_column: None,
                            },
                            activation: MemoryPortActivation::When(COL_SEL_RAISE_EFFECT),
                        },
                        MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Read,
                            activation: MemoryPortActivation::When(COL_SEL_ENTER_EFFECT_HANDLER),
                        },
                    ]
                })
                .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackEffectIndex,
            kind: MemoryKind::Ram,
            // The method-table lookup of each of these steps ties the index
            // to the effect hash that its event commits to.
            ports: std::iter::once(MemoryPortSpec {
                address_columns: vec![COL_CALL_STACK_FRAME],
                value_column: COL_METHOD_INDEX,
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_SEL_RAISE_EFFECT),
            })
            .chain(
                [COL_SEL_ENTER_EFFECT_HANDLER, COL_SEL_SKIP_EFFECT_HANDLER]
                    .into_iter()
                    .map(|selector| MemoryPortSpec {
                        address_columns: vec![COL_CALL_STACK_FRAME],
                        value_column: COL_METHOD_INDEX,
                        kind: MemoryPortKind::Read,
                        activation: MemoryPortActivation::When(selector),
                    }),
            )
            .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackIsEffect,
            kind: MemoryKind::Ram,
            ports: std::iter::once(MemoryPortSpec {
                address_columns: vec![COL_CALL_STACK_FRAME],
                value_column: COL_CALL_FRAME_IS_EFFECT,
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
            })
            .chain(
                Opcode::all()
                    .into_iter()
                    .filter(|op| op.required_effect_frame().is_some() && !op.pushes_to_call_stack())
                    .map(|op| MemoryPortSpec {
                        address_columns: vec![COL_CALL_STACK_FRAME],
                        value_column: COL_CALL_FRAME_IS_EFFECT,
                        kind: MemoryPortKind::Read,
                        activation: MemoryPortActivation::When(op.selector()),
                    }),
            )
            .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackConstructor,
            kind: MemoryKind::Ram,
            ports: std::iter::once(MemoryPortSpec {
                address_columns: vec![COL_CALL_STACK_FRAME],
                value_column: COL_CALL_FRAME_CONSTRUCTOR,
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
            })
            .chain(
                Opcode::all()
                    .into_iter()
                    .filter(|op| {
                        // Return uses the flag to select the pending receive phase
                        // and unit-result requirement, rather than fixing its value.
                        (op.required_constructor_frame().is_some() || *op == Opcode::Return)
                            && !op.pushes_to_call_stack()
                    })
                    .map(|op| MemoryPortSpec {
                        address_columns: vec![COL_CALL_STACK_FRAME],
                        value_column: COL_CALL_FRAME_CONSTRUCTOR,
                        kind: MemoryPortKind::Read,
                        activation: MemoryPortActivation::When(op.selector()),
                    }),
            )
            .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackSavedPhase,
            kind: MemoryKind::Ram,
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_FRAME],
                    value_column: COL_CALL_FRAME_SAVED_PHASE,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_FRAME],
                    value_column: COL_CALL_FRAME_SAVED_PHASE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RESUME_EFFECT),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_FRAME],
                    value_column: COL_CALL_FRAME_SAVED_PHASE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RECEIVE_NEW_UTXO_HANDLE),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_FRAME],
                    value_column: COL_CALL_FRAME_SAVED_PHASE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RECEIVE_CALL_RESULT),
                },
            ],
        },
        saved_effect_context(
            MemoryId::CallStackSavedEffectHead,
            COL_CALL_FRAME_SAVED_EFFECT_HEAD,
            &[COL_SEL_EXIT_EFFECT_HANDLER],
        ),
        saved_effect_context(
            MemoryId::CallStackSavedEffectDepth,
            COL_CALL_FRAME_SAVED_EFFECT_DEPTH,
            &[COL_SEL_RETURN, COL_SEL_EXIT_EFFECT_HANDLER],
        ),
    ]
}

/// Written by every call-stack push and read back from the top frame.
fn saved_effect_context(
    id: MemoryId,
    value_column: usize,
    readers: &[usize],
) -> MemorySpec<MemoryId> {
    MemorySpec {
        id,
        kind: MemoryKind::Ram,
        ports: std::iter::once(MemoryPortSpec {
            address_columns: vec![COL_CALL_STACK_FRAME],
            value_column,
            kind: MemoryPortKind::Write {
                value_before_column: None,
            },
            activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
        })
        .chain(readers.iter().map(|&selector| MemoryPortSpec {
            address_columns: vec![COL_CALL_STACK_FRAME],
            value_column,
            kind: MemoryPortKind::Read,
            activation: MemoryPortActivation::When(selector),
        }))
        .collect(),
    }
}

pub(crate) fn sanity_checking_policy(
    catalog: &MemoryCatalog<MemoryId>,
) -> MemoryCheckPolicy<MemoryId> {
    use MemoryId::*;
    // TODO: unwrap
    MemoryCheckPolicy::new(
        catalog,
        // TODO: could this be a function with an exhaustive match instead?
        vec![
            (InstanceStack, RamInitialization::Zero),
            (CallStackCaller, RamInitialization::Zero),
            (CallStackEffectArgument, RamInitialization::Zero),
            (CallStackEffectIndex, RamInitialization::Zero),
            (CallStackIsEffect, RamInitialization::Zero),
            (CallStackConstructor, RamInitialization::Zero),
            (CallStackSavedPhase, RamInitialization::Zero),
            (CallStackSavedEffectHead, RamInitialization::Zero),
            (CallStackSavedEffectDepth, RamInitialization::Zero),
            (EffectScopeParent, RamInitialization::Zero),
            (EffectScopeOwner, RamInitialization::Zero),
            (EffectScopeFrameId, RamInitialization::Zero),
            (EffectRegistered, RamInitialization::Zero),
            (AbiMethodCount, RamInitialization::Zero),
            (AbiGeneration, RamInitialization::Zero),
            (EnabledMethodLogUtxo, RamInitialization::Zero),
            (EnabledMethodLogMethod, RamInitialization::Zero),
            (EnabledMethodLogGeneration, RamInitialization::Zero),
            (EnabledMethodLogOrdinal, RamInitialization::Zero),
            (ResourceResolver, RamInitialization::Zero),
            (TokenTraceCommitments, RamInitialization::Zero),
            (UtxoTraceCommitments, RamInitialization::Zero),
            (CoordTraceCommitments, RamInitialization::Zero),
        ],
    )
    .unwrap()
}

pub(crate) fn preload_tables(
    method_table: &[starstream_interleaving_spec::MethodHash],
) -> MemoryPreload<MemoryId> {
    // Host diagnostics derive this dictionary from the trace. The Nebula proof
    // context instead takes it as explicit verifier-owned input and binds this
    // ROM through the program's initial-memory commitment.
    let mut preload = MemoryPreload::default();

    for (index, method) in method_table.iter().copied().enumerate() {
        let base = u32::try_from(index)
            .expect("the trace-local method table fits in u32")
            .checked_mul(8)
            .expect("the trace-local method table address fits in u32");

        for (offset, value) in method.0.into_iter().enumerate() {
            preload.insert(
                MemoryId::MethodTable,
                vec![base + u32::try_from(offset).expect("method hashes have eight limbs")],
                value,
            );
        }
    }

    preload
}

pub(crate) fn preload_signers(preload: &mut MemoryPreload<MemoryId>, signers: &BTreeSet<u64>) {
    for (index, &key) in (0u32..).zip(signers) {
        for offset in 0..SIGNER_KEY_LIMBS {
            let address = index * SIGNER_KEY_LIMBS as u32 + offset as u32;
            let limb = (key >> (offset as u32 * u32::BITS)) as u32;
            preload.insert(MemoryId::SignerKey, vec![address], limb);
        }
        preload.insert(MemoryId::SignerPresent, vec![index], 1);
    }
}

impl MemoryId {
    pub const fn name(self) -> &'static str {
        match self {
            MemoryId::InstanceStack => "instance_stack",
            MemoryId::CallStackCaller => "call_stack_caller",
            MemoryId::CallStackEffectArgument => "call_stack_effect_argument",
            MemoryId::CallStackEffectIndex => "call_stack_effect_index",
            MemoryId::CallStackIsEffect => "call_stack_is_effect",
            MemoryId::CallStackConstructor => "call_stack_constructor",
            MemoryId::CallStackSavedPhase => "call_stack_saved_phase",
            MemoryId::CallStackSavedEffectHead => "call_stack_saved_effect_head",
            MemoryId::CallStackSavedEffectDepth => "call_stack_saved_effect_depth",
            MemoryId::EffectScopeParent => "effect_scope_parent",
            MemoryId::EffectScopeOwner => "effect_scope_owner",
            MemoryId::EffectScopeFrameId => "effect_scope_frame_id",
            MemoryId::EffectRegistered => "effect_registered",
            MemoryId::AbiMethodCount => "abi_method_count",
            MemoryId::AbiGeneration => "abi_generation",
            MemoryId::EnabledMethodLogUtxo => "enabled_method_log_utxo",
            MemoryId::EnabledMethodLogMethod => "enabled_method_log_method",
            MemoryId::EnabledMethodLogGeneration => "enabled_method_log_generation",
            MemoryId::EnabledMethodLogOrdinal => "enabled_method_log_ordinal",
            MemoryId::MethodTable => "method_table",
            MemoryId::SignerKey => "signer_key",
            MemoryId::SignerPresent => "signer_present",
            MemoryId::ResourceResolver => "resource_resolver",
            MemoryId::TokenTraceCommitments => "token_trace_commitments",
            MemoryId::UtxoTraceCommitments => "utxo_trace_commitments",
            MemoryId::CoordTraceCommitments => "coord_trace_commitments",
        }
    }
}

impl std::fmt::Display for MemoryId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.name())
    }
}

#[cfg(test)]
mod port_load {
    use super::*;

    /// Ports that each opcode can activate, by memory. The heaviest opcode is
    /// the fewest slots that any plan of this catalog can reach.
    #[test]
    #[ignore = "prints a table; run with -- --ignored --nocapture"]
    fn ports_by_opcode() {
        let relation = crate::ccs::build_relation().unwrap();
        let catalog = build_memory_layout();
        let facts = activation_facts(relation.r1cs(), &catalog);
        let plan = plan_ordered_memory_slots(&catalog, &facts);
        println!(
            "{} ports, {} slots, {} ports without support",
            plan.logical_ports,
            plan.slots.len(),
            plan.ports_without_support
        );
        let mut loads = Opcode::all()
            .into_iter()
            .map(|opcode| {
                let by_memory: Vec<(MemoryId, usize)> = catalog
                    .entries()
                    .iter()
                    .filter_map(|memory| {
                        let ports = memory
                            .ports
                            .iter()
                            .filter(|port| match port.activation {
                                MemoryPortActivation::When(gate) => facts
                                    .supports
                                    .get(&gate)
                                    .is_none_or(|atoms| atoms.contains(&opcode.selector())),
                                _ => true,
                            })
                            .count();
                        (ports > 0).then_some((memory.id, ports))
                    })
                    .collect();
                (
                    by_memory.iter().map(|(_, n)| n).sum::<usize>(),
                    opcode,
                    by_memory,
                )
            })
            .collect::<Vec<_>>();
        loads.sort_by_key(|(total, ..)| std::cmp::Reverse(*total));
        for (total, opcode, by_memory) in loads {
            let parts = by_memory
                .iter()
                .map(|(id, n)| format!("{id:?} {n}"))
                .collect::<Vec<_>>()
                .join(", ");
            println!("{total:>3} {opcode:?}: {parts}");
        }
    }
}
