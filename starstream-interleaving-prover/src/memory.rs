use neo_application::{
    MemoryCatalog, MemoryCheckPolicy, MemoryKind, MemoryPortActivation, MemoryPortKind,
    MemoryPortSpec, MemoryPreload, MemorySpec, RamInitialization,
};

use crate::opcode::Opcode;

use crate::ccs::layout::{
    COL_ABI_GENERATION_ADDR, COL_ABI_GENERATION_AFTER, COL_ABI_GENERATION_BEFORE,
    COL_ABI_METHOD_COUNT_ADDR, COL_ABI_METHOD_COUNT_AFTER, COL_ABI_METHOD_COUNT_BEFORE,
    COL_ABI_METHOD_COUNT_READ, COL_ABI_METHOD_COUNT_WRITE, COL_CALL_FRAME_CALLEE,
    COL_CALL_FRAME_CONSTRUCTOR, COL_CALL_FRAME_IS_EFFECT, COL_CALL_FRAME_SAVED_PHASE,
    COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8, COL_CALL_STACK_EXPECTED_ARG_VALUE,
    COL_CALL_STACK_EXPECTED_RESULT_VALUE, COL_CALL_STACK_MUL_STRIDE_8, COL_CALL_STACK_POP,
    COL_CALL_STACK_PUSH, COL_CALL_STACK_TOP, COL_CALL_TARGET, COL_CURR_BEFORE,
    COL_ENABLED_METHOD_LOG_ADDR, COL_ENABLED_METHOD_LOG_GENERATION, COL_ENABLED_METHOD_LOG_ORDINAL,
    COL_ENABLED_METHOD_LOG_UTXO, COL_EVENT_ACTIVE, COL_EVENT_OWNER_STRIDE_8,
    COL_HANDLER_SEARCH_REMAINING_AFTER, COL_HANDLER_SEARCH_REMAINING_BEFORE, COL_IN_WORDS,
    COL_METHOD_APPEND, COL_METHOD_HASH_VALUE, COL_METHOD_INDEX, COL_METHOD_LOOKUP,
    COL_METHOD_TABLE_ADDR, COL_OUT_WORDS, COL_RESOURCE_RESOLVER_ADDR_CID,
    COL_RESOURCE_RESOLVER_ADDR_HANDLE, COL_RESOURCE_RESOLVER_READ, COL_RESOURCE_RESOLVER_VALUE,
    COL_RESOURCE_RESOLVER_WRITE, COL_SEL_CALL_METHOD, COL_SEL_ENTER_EFFECT_HANDLER,
    COL_SEL_ENTER_METHOD, COL_SEL_EXIT_EFFECT_HANDLER, COL_SEL_READ_ABI,
    COL_SEL_RECEIVE_METHOD_RESULT, COL_SEL_RECEIVE_NEW_UTXO_HANDLE, COL_SEL_RESUME_EFFECT,
    COL_SEL_RETURN, COL_SEL_YIELD_BEGIN, COL_TRACE_ROOT_READ, range_check_layout,
};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum MemoryId {
    CallStackCaller,
    CallStackExpectedArgument,
    CallStackExpectedResult,
    CallStackExpectedMethod,
    CallStackIsEffect,
    CallStackConstructor,
    CallStackCallee,
    CallStackSavedPhase,
    CallStackHandlerSearchRemaining,
    AbiMethodCount,
    AbiGeneration,
    EnabledMethodLogUtxo,
    EnabledMethodLogMethod,
    EnabledMethodLogGeneration,
    EnabledMethodLogOrdinal,
    MethodTable,
    ResourceResolver,
    TraceCommitments,
}

pub fn build_memory_layout() -> MemoryCatalog<MemoryId> {
    let mut memories = vec![];

    memories.extend_from_slice(&call_stack_layout());

    memories.extend_from_slice(&abi_method_count_layout());

    memories.extend_from_slice(&abi_generation_layout());

    memories.extend_from_slice(&enabled_method_log_layout());

    memories.extend_from_slice(&method_table_layout());

    memories.extend_from_slice(&resource_resolver_layout());

    memories.extend_from_slice(&trace_commitments_layout());

    MemoryCatalog::new(memories, range_check_layout().columns()).expect("valid memory declarations")
}

fn trace_commitments_layout() -> [MemorySpec<MemoryId>; 1] {
    [MemorySpec {
        id: MemoryId::TraceCommitments,
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
                activation: MemoryPortActivation::When(COL_EVENT_ACTIVE),
            })
            .chain(COL_EVENT_OWNER_STRIDE_8.into_iter().zip(COL_IN_WORDS).map(
                |(address, value)| MemoryPortSpec {
                    address_columns: vec![address],
                    value_column: value,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_TRACE_ROOT_READ),
                },
            ))
            .collect(),
    }]
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
                activation: MemoryPortActivation::When(COL_METHOD_LOOKUP),
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

fn call_stack_layout() -> [MemorySpec<MemoryId>; 9] {
    [
        MemorySpec {
            id: MemoryId::CallStackCaller,
            kind: MemoryKind::Ram,
            // Reuse the first limb-stride address for caller words: pushes
            // select the next free frame and peeks/pops select the current top.
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CURR_BEFORE,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CALL_TARGET,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_CALL_STACK_POP),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CALL_TARGET,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RETURN),
                },
            ],
        },
        MemorySpec {
            id: MemoryId::CallStackExpectedArgument,
            kind: MemoryKind::Ram,
            ports: COL_CALL_STACK_EXPECTED_ARG_VALUE
                .into_iter()
                .zip(COL_CALL_STACK_MUL_STRIDE_8)
                .flat_map(|(value, address)| {
                    vec![
                        MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Write {
                                value_before_column: None,
                            },
                            activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                        },
                        MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Read,
                            activation: MemoryPortActivation::When(COL_CALL_STACK_TOP),
                        },
                    ]
                    .into_iter()
                })
                .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackExpectedResult,
            kind: neo_application::MemoryKind::Ram,

            ports: COL_CALL_STACK_EXPECTED_RESULT_VALUE
                .into_iter()
                .zip(COL_CALL_STACK_MUL_STRIDE_8)
                .flat_map(|(value, address)| {
                    vec![
                        MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Write {
                                value_before_column: None,
                            },
                            activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                        },
                        MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Write {
                                value_before_column: None,
                            },
                            activation: MemoryPortActivation::When(COL_SEL_EXIT_EFFECT_HANDLER),
                        },
                        MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Write {
                                value_before_column: None,
                            },
                            activation: MemoryPortActivation::When(COL_SEL_RETURN),
                        },
                        MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Read,
                            activation: MemoryPortActivation::When(COL_SEL_RESUME_EFFECT),
                        },
                    ]
                    .into_iter()
                })
                .chain(
                    COL_CALL_STACK_EXPECTED_RESULT_VALUE
                        .into_iter()
                        .zip(COL_CALL_STACK_MUL_STRIDE_8)
                        .map(|(value, address)| MemoryPortSpec {
                            address_columns: vec![address],
                            value_column: value,
                            kind: MemoryPortKind::Read,
                            activation: MemoryPortActivation::When(COL_SEL_RECEIVE_METHOD_RESULT),
                        }),
                )
                .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackExpectedMethod,
            kind: neo_application::MemoryKind::Ram,
            ports: COL_METHOD_HASH_VALUE
                .into_iter()
                .zip(COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8)
                .flat_map(|(value, address)| {
                    [MemoryPortSpec {
                        address_columns: vec![address],
                        value_column: value,
                        kind: MemoryPortKind::Write {
                            value_before_column: None,
                        },
                        activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                    }]
                    .into_iter()
                    .chain(
                        [COL_SEL_ENTER_METHOD, COL_SEL_ENTER_EFFECT_HANDLER]
                            .into_iter()
                            .map(move |selector| MemoryPortSpec {
                                address_columns: vec![address],
                                value_column: value,
                                kind: MemoryPortKind::Read,
                                activation: MemoryPortActivation::When(selector),
                            }),
                    )
                })
                .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackIsEffect,
            kind: MemoryKind::Ram,
            ports: std::iter::once(MemoryPortSpec {
                address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
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
                        address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
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
                address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
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
                        address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                        value_column: COL_CALL_FRAME_CONSTRUCTOR,
                        kind: MemoryPortKind::Read,
                        activation: MemoryPortActivation::When(op.selector()),
                    }),
            )
            .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackCallee,
            kind: MemoryKind::Ram,
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CALL_FRAME_CALLEE,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CALL_FRAME_CALLEE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RECEIVE_NEW_UTXO_HANDLE),
                },
            ],
        },
        MemorySpec {
            id: MemoryId::CallStackSavedPhase,
            kind: MemoryKind::Ram,
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CALL_FRAME_SAVED_PHASE,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CALL_FRAME_SAVED_PHASE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RESUME_EFFECT),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CALL_FRAME_SAVED_PHASE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RECEIVE_NEW_UTXO_HANDLE),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_CALL_FRAME_SAVED_PHASE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RECEIVE_METHOD_RESULT),
                },
            ],
        },
        MemorySpec {
            id: MemoryId::CallStackHandlerSearchRemaining,
            kind: MemoryKind::Ram,
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_HANDLER_SEARCH_REMAINING_AFTER,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_HANDLER_SEARCH_REMAINING_BEFORE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_ENTER_EFFECT_HANDLER),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_HANDLER_SEARCH_REMAINING_AFTER,
                    kind: MemoryPortKind::Write {
                        value_before_column: Some(COL_HANDLER_SEARCH_REMAINING_BEFORE),
                    },
                    activation: MemoryPortActivation::When(COL_SEL_EXIT_EFFECT_HANDLER),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_STACK_MUL_STRIDE_8[0]],
                    value_column: COL_HANDLER_SEARCH_REMAINING_BEFORE,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_SEL_RESUME_EFFECT),
                },
            ],
        },
    ]
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
            (CallStackCaller, RamInitialization::Zero),
            (CallStackExpectedArgument, RamInitialization::Zero),
            (CallStackExpectedResult, RamInitialization::Zero),
            (CallStackExpectedMethod, RamInitialization::Zero),
            (CallStackIsEffect, RamInitialization::Zero),
            (CallStackConstructor, RamInitialization::Zero),
            (CallStackCallee, RamInitialization::Zero),
            (CallStackSavedPhase, RamInitialization::Zero),
            (CallStackHandlerSearchRemaining, RamInitialization::Zero),
            (AbiMethodCount, RamInitialization::Zero),
            (AbiGeneration, RamInitialization::Zero),
            (EnabledMethodLogUtxo, RamInitialization::Zero),
            (EnabledMethodLogMethod, RamInitialization::Zero),
            (EnabledMethodLogGeneration, RamInitialization::Zero),
            (EnabledMethodLogOrdinal, RamInitialization::Zero),
            (ResourceResolver, RamInitialization::Zero),
            (TraceCommitments, RamInitialization::Zero),
        ],
    )
    .unwrap()
}

pub(crate) fn preload_tables(
    method_table: &[starstream_interleaving_spec::MethodHash],
) -> MemoryPreload<MemoryId> {
    // TODO(proof): This trace-derived dictionary is currently trusted input to
    // the diagnostic memory checker. Bind its initialization to the eventual
    // proof, or replace it with the ledger-derived static ABI table, when the
    // prover path is integrated.
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

impl MemoryId {
    pub const fn name(self) -> &'static str {
        match self {
            MemoryId::CallStackCaller => "call_stack_caller",
            MemoryId::CallStackExpectedArgument => "call_stack_expected_argument",
            MemoryId::CallStackExpectedResult => "call_stack_expected_result",
            MemoryId::CallStackExpectedMethod => "call_stack_expected_method",
            MemoryId::CallStackIsEffect => "call_stack_is_effect",
            MemoryId::CallStackConstructor => "call_stack_constructor",
            MemoryId::CallStackCallee => "call_stack_callee",
            MemoryId::CallStackSavedPhase => "call_stack_saved_phase",
            MemoryId::CallStackHandlerSearchRemaining => "call_stack_handler_search_remaining",
            MemoryId::AbiMethodCount => "abi_method_count",
            MemoryId::AbiGeneration => "abi_generation",
            MemoryId::EnabledMethodLogUtxo => "enabled_method_log_utxo",
            MemoryId::EnabledMethodLogMethod => "enabled_method_log_method",
            MemoryId::EnabledMethodLogGeneration => "enabled_method_log_generation",
            MemoryId::EnabledMethodLogOrdinal => "enabled_method_log_ordinal",
            MemoryId::MethodTable => "method_table",
            MemoryId::ResourceResolver => "resource_resolver",
            MemoryId::TraceCommitments => "trace_commitments",
        }
    }
}

impl std::fmt::Display for MemoryId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.name())
    }
}
