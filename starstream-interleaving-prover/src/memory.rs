use neo_application::{
    MemoryCatalog, MemoryCheckPolicy, MemoryKind, MemoryPortActivation, MemoryPortKind,
    MemoryPortSpec, MemoryPreload, MemorySpec, RamInitialization,
};

use crate::ccs::layout::{
    COL_CALL_SP_BEFORE, COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8, COL_CALL_STACK_EXPECTED_ARG_VALUE,
    COL_CALL_STACK_EXPECTED_METHOD_VALUE, COL_CALL_STACK_EXPECTED_RESULT_VALUE,
    COL_CALL_STACK_MUL_STRIDE_4, COL_CALL_STACK_PUSH, COL_CALL_STACK_TOP, COL_CURR_BEFORE,
    COL_CURR_BEFORE_STRIDE_4, COL_ENABLED_METHOD_ADDR, COL_ENABLED_METHOD_VALUE, COL_IN, COL_OUT,
    COL_RESOURCE_RESOLVER_ADDR_CID, COL_RESOURCE_RESOLVER_ADDR_HANDLE, COL_RESOURCE_RESOLVER_READ,
    COL_RESOURCE_RESOLVER_VALUE, COL_RESOURCE_RESOLVER_WRITE, COL_UTXO_LIFECYCLE_ADDR,
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
    EnabledMethod,
    ResourceResolver,
    TraceCommitments,
}

pub fn build_memory_layout() -> MemoryCatalog<MemoryId> {
    let mut memories = vec![];

    memories.extend_from_slice(&call_stack_layout());

    memories.extend_from_slice(&utxo_lifecycle_map_layout());

    memories.extend_from_slice(&enabled_method_map_layout());

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

fn enabled_method_map_layout() -> [MemorySpec<MemoryId>; 1] {
    let address_columns = vec![COL_ENABLED_METHOD_ADDR];

    [MemorySpec {
        id: MemoryId::EnabledMethod,
        kind: MemoryKind::Ram,
        ports: vec![
            MemoryPortSpec {
                address_columns: address_columns.clone(),
                value_column: COL_ENABLED_METHOD_VALUE,
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_UTXO_LIFECYCLE_WRITE),
            },
            MemoryPortSpec {
                address_columns,
                value_column: COL_ENABLED_METHOD_VALUE,
                kind: MemoryPortKind::Read,
                activation: MemoryPortActivation::When(COL_UTXO_LIFECYCLE_READ),
            },
        ],
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
            ports: vec![MemoryPortSpec {
                address_columns: vec![COL_CALL_SP_BEFORE],
                value_column: COL_CURR_BEFORE,
                kind: MemoryPortKind::Write {
                    value_before_column: None,
                },
                activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
            }],
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
                .map(|(value, address)| MemoryPortSpec {
                    address_columns: vec![address],
                    value_column: value,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
                })
                .collect(),
        },
        MemorySpec {
            id: MemoryId::CallStackExpectedMethod,
            kind: neo_application::MemoryKind::Ram,
            ports: COL_CALL_STACK_EXPECTED_METHOD_VALUE
                .into_iter()
                .zip(COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8)
                .map(|(value, address)| MemoryPortSpec {
                    address_columns: vec![address],
                    value_column: value,
                    kind: MemoryPortKind::Write {
                        value_before_column: None,
                    },
                    activation: MemoryPortActivation::When(COL_CALL_STACK_PUSH),
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
            (EnabledMethod, RamInitialization::Zero),
            (ResourceResolver, RamInitialization::Zero),
            (TraceCommitments, RamInitialization::Zero),
        ],
    )
    .unwrap()
}

pub(crate) fn preload_tables() -> MemoryPreload<MemoryId> {
    MemoryPreload::default()
}

impl MemoryId {
    pub const fn name(self) -> &'static str {
        match self {
            MemoryId::CallStackCaller => "call_stack_caller",
            MemoryId::CallStackExpectedArgument => "call_stack_expected_argument",
            MemoryId::CallStackExpectedResult => "call_stack_expected_result",
            MemoryId::CallStackExpectedMethod => "call_stack_expected_method",
            MemoryId::UtxoLifecycle => "utxo_lifecycle",
            MemoryId::EnabledMethod => "enabled_method",
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
