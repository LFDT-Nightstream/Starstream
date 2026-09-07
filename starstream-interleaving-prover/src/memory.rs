use neo_application::{
    MemoryCatalog, MemoryCheckPolicy, MemoryKind, MemoryPortActivation, MemoryPortKind,
    MemoryPortSpec, MemoryPreload, MemorySpec, RamInitialization,
};

use crate::ccs::layout::{
    COL_ABI_GENERATION_ADDR, COL_ABI_GENERATION_AFTER, COL_ABI_GENERATION_BEFORE,
    COL_CALL_SP_AFTER, COL_CALL_SP_BEFORE, COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8,
    COL_CALL_STACK_EXPECTED_ARG_VALUE, COL_CALL_STACK_EXPECTED_RESULT_VALUE,
    COL_CALL_STACK_MUL_STRIDE_4, COL_CALL_STACK_POP, COL_CALL_STACK_PUSH, COL_CALL_STACK_TOP,
    COL_CALL_TARGET, COL_CURR_BEFORE, COL_CURR_BEFORE_STRIDE_4, COL_ENABLED_METHOD_LOG_ADDR,
    COL_ENABLED_METHOD_LOG_GENERATION, COL_ENABLED_METHOD_LOG_UTXO, COL_IN, COL_METHOD_HASH_VALUE,
    COL_METHOD_INDEX, COL_METHOD_LOOKUP, COL_METHOD_TABLE_ADDR, COL_OUT,
    COL_RESOURCE_RESOLVER_ADDR_CID, COL_RESOURCE_RESOLVER_ADDR_HANDLE, COL_RESOURCE_RESOLVER_READ,
    COL_RESOURCE_RESOLVER_VALUE, COL_RESOURCE_RESOLVER_WRITE, COL_SEL_CALL_METHOD,
    COL_SEL_ENTER_METHOD, COL_SEL_REGISTER_METHOD, COL_SEL_YIELD_BEGIN, COL_UTXO_LIFECYCLE_ADDR,
    COL_UTXO_LIFECYCLE_READ, COL_UTXO_LIFECYCLE_VALUE, COL_UTXO_LIFECYCLE_WRITE,
    range_check_layout,
};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum MemoryId {
    CallStackCaller,
    CallStackExpectedArgument,
    CallStackExpectedResult,
    CallStackExpectedMethod,
    UtxoLifecycle,
    AbiGeneration,
    EnabledMethodLogUtxo,
    EnabledMethodLogMethod,
    EnabledMethodLogGeneration,
    MethodTable,
    ResourceResolver,
    TraceCommitments,
}

pub fn build_memory_layout() -> MemoryCatalog<MemoryId> {
    let mut memories = vec![];

    memories.extend_from_slice(&call_stack_layout());

    memories.extend_from_slice(&utxo_lifecycle_map_layout());

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
        ports: COL_CURR_BEFORE_STRIDE_4
            .into_iter()
            .zip(COL_OUT)
            .zip(COL_IN)
            .map(|((address, value_after), value_before)| MemoryPortSpec {
                address_columns: vec![address],
                value_column: value_after,
                kind: MemoryPortKind::Write {
                    value_before_column: Some(value_before),
                },
                activation: MemoryPortActivation::Always,
            })
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
                    value_before_column: None,
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
    // TODO(lifecycle): Track whether the current generation registered any
    // methods (or its count) when UTXO lifecycle transitions are wired. That
    // metadata can likely share a packed word with the generation.
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

fn enabled_method_log_layout() -> [MemorySpec<MemoryId>; 3] {
    // TODO(perf): Packing bounded UTXO, method, and generation indices may
    // reduce both scan cells and operation slots once the prover's concrete
    // limits are known. Keep parallel 32-bit RAMs until then.
    [
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
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_SEL_REGISTER_METHOD),
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

fn utxo_lifecycle_map_layout() -> [MemorySpec<MemoryId>; 1] {
    [MemorySpec {
        id: MemoryId::UtxoLifecycle,
        kind: MemoryKind::Ram,
        ports: vec![
            MemoryPortSpec {
                address_columns: vec![COL_UTXO_LIFECYCLE_ADDR],
                value_column: COL_UTXO_LIFECYCLE_VALUE,
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_UTXO_LIFECYCLE_WRITE),
            },
            MemoryPortSpec {
                address_columns: vec![COL_UTXO_LIFECYCLE_ADDR],
                value_column: COL_UTXO_LIFECYCLE_VALUE,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(COL_UTXO_LIFECYCLE_READ),
            },
        ],
    }]
}

fn call_stack_layout() -> [MemorySpec<MemoryId>; 4] {
    [
        MemorySpec {
            id: MemoryId::CallStackCaller,
            kind: MemoryKind::Ram,
            // A push writes frame `sp_before`; the matching pop has already
            // decremented that pointer into `sp_after`. Using the two pointer
            // columns keeps this one-word-per-frame memory densely addressed.
            ports: vec![
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_SP_BEFORE],
                    value_column: COL_CURR_BEFORE,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                },
                MemoryPortSpec {
                    address_columns: vec![COL_CALL_SP_AFTER],
                    value_column: COL_CALL_TARGET,
                    kind: MemoryPortKind::Read,
                    activation: MemoryPortActivation::When(COL_CALL_STACK_POP),
                },
            ],
        },
        MemorySpec {
            id: MemoryId::CallStackExpectedArgument,
            kind: MemoryKind::Ram,
            ports: COL_CALL_STACK_EXPECTED_ARG_VALUE
                .into_iter()
                .zip(COL_CALL_STACK_MUL_STRIDE_4)
                .flat_map(|(value, address)| {
                    [
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
                .zip(COL_CALL_STACK_MUL_STRIDE_4)
                .flat_map(|(value, address)| {
                    [
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
                            activation: MemoryPortActivation::When(COL_CALL_STACK_POP),
                        },
                    ]
                    .into_iter()
                })
                .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackExpectedMethod,
            kind: neo_application::MemoryKind::Ram,
            ports: COL_METHOD_HASH_VALUE
                .into_iter()
                .zip(COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8)
                .flat_map(|(value, address)| {
                    [
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
                            activation: MemoryPortActivation::When(COL_SEL_ENTER_METHOD),
                        },
                    ]
                    .into_iter()
                })
                .collect(),
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
            (UtxoLifecycle, RamInitialization::Zero),
            (AbiGeneration, RamInitialization::Zero),
            (EnabledMethodLogUtxo, RamInitialization::Zero),
            (EnabledMethodLogMethod, RamInitialization::Zero),
            (EnabledMethodLogGeneration, RamInitialization::Zero),
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

        for (offset, value) in crate::step::method_hash_words(method)
            .into_iter()
            .enumerate()
        {
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
            MemoryId::UtxoLifecycle => "utxo_lifecycle",
            MemoryId::AbiGeneration => "abi_generation",
            MemoryId::EnabledMethodLogUtxo => "enabled_method_log_utxo",
            MemoryId::EnabledMethodLogMethod => "enabled_method_log_method",
            MemoryId::EnabledMethodLogGeneration => "enabled_method_log_generation",
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
