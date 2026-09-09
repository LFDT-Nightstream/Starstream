use std::sync::OnceLock;

use neo_application::{RangeCheckBitFamily, RangeCheckLayout, define_column_region};

pub const PUBLIC_INPUTS: usize = 1;

define_column_region! {
    region: "main_section",
    start: 0usize,
    width: pub MAIN_COLUMN_COUNT,
    families: pub MAIN_COLUMN_FAMILIES,
    indices: pub,
    columns: [
        COL_ONE: Field => "",
        COL_SEL_NEW_UTXO: Boolean => "selector for the NewUtxo action",
        COL_SEL_ENTER_CONSTRUCTOR: Boolean => "selector for the EnterConstructor action",
        COL_SEL_YIELD_BEGIN: Boolean => "selector for the YieldBegin action",
        COL_SEL_REGISTER_METHOD: Boolean => "selector for the RegisterMethod action",
        COL_SEL_RETURN: Boolean => "selector for the Return action",
        COL_SEL_CALL_METHOD: Boolean => "selector for the CallMethod action",
        COL_SEL_ENTER_METHOD: Boolean => "selector for the EnterMethod action",
        COL_SEL_PADDING: Boolean => "state-preserving circuit-only padding slot",
        COL_CURR_BEFORE: U32 => "packed coroutine id that has the turn",
        COL_CURR_AFTER: U32 => "packed coroutine id that has the turn in the next step",
        COL_CALL_STACK_PUSH: Boolean => "true when a call happens and we need to keep track of the caller's context",
        COL_CALL_STACK_POP: Boolean => "true when returning from a call",
        COL_CALL_STACK_TOP: Boolean => "true when peeking at the top of the call stack without popping",
        COL_CALL_SP_BEFORE_INVERSE: Field => "inverse proving that an execution row starts with a nonempty call stack",
        // TODO: limit sp so that this doesn't overflow
        COL_CALL_STACK_MUL_STRIDE_4: [U32; 4] => "SP * 4 + i",
        // TODO: limit sp so that this doesn't overflow
        COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8: [U32; 8] => "SP * 8 + i",
        COL_CALL_STACK_EXPECTED_ARG_VALUE: [U32; 4] => "call_stack[COL_CALL_STACK_EXPECTED_ADDR[i]].expected_arg",
        COL_CALL_STACK_EXPECTED_RESULT_VALUE: [U32; 4] => "call_stack[COL_CALL_STACK_EXPECTED_ADDR[i]].expected_result",
        // using 8 limbs for exact sha256 repr for now (in 32-bit limbs), we
        // could improve this, but the memory argument as currently implemented
        // is 32-bit based, plus we'd have to drop 2 bits to use 4 limbs
        COL_METHOD_HASH_VALUE: [U32; 8] => "method hash limb bus: call-stack expected-method value and method-table ROM lookup value",
        COL_METHOD_INDEX: (Bits(29)) => "trace-local compact index for the method hash",
        COL_METHOD_LOOKUP: Boolean => "true when RegisterMethod or CallMethod resolves a method hash",
        COL_METHOD_TABLE_ADDR: [U32; 8] => "method_index * 8 + hash limb offset",
        COL_CALL_TARGET: U32 => "packed coroutine id that gets control in the next step",
        COL_UTXO_LIFECYCLE_ADDR: U32 => "key for lifecycle read/write (can be target or curr)",
        COL_UTXO_LIFECYCLE_VALUE: Byte => "value for lifecycle read/write (can be target or curr)",
        COL_UTXO_LIFECYCLE_WRITE: Boolean => "true (1) if the current opcode writes to the lifecycle map (utxo_id -> live|dead)",
        COL_UTXO_LIFECYCLE_READ: Boolean => "true (1) if the current opcode reads the lifecycle map (utxo_id -> live|dead)",
        COL_ENABLED_METHOD_LOG_ADDR: U32 => "append-log entry written by RegisterMethod or selected by CallMethod",
        COL_ENABLED_METHOD_LOG_UTXO: U32 => "packed UTXO id stored in the enabled-method log entry",
        COL_ENABLED_METHOD_LOG_GENERATION: U32 => "ABI generation stored in the enabled-method log entry",
        COL_ABI_GENERATION_ADDR: U32 => "packed UTXO id whose current ABI generation is accessed",
        COL_ABI_GENERATION_BEFORE: U32 => "current ABI generation read from memory",
        COL_ABI_GENERATION_AFTER: U32 => "next ABI generation written by YieldBegin",

        COL_RESOURCE_RESOLVER_ADDR_CID: U32 => "packed holder coroutine id in the resource key",
        COL_RESOURCE_RESOLVER_ADDR_HANDLE: U32 => "resource handle in the resource key",
        COL_RESOURCE_RESOLVER_VALUE: U32 => "the packed coroutine id assigned to the resource at (cid, handle)",
        COL_RESOURCE_RESOLVER_WRITE: Boolean => "1 if writing to the resource resolver map (on return)",
        COL_RESOURCE_RESOLVER_READ: Boolean => "1 if reading from the resource resolver map (on call_method)",

        // TODO: limit curr side so that this doesn't overflow
        COL_CURR_BEFORE_STRIDE_4: [U32; 4] => "curr * 4 + i",


    ]
}

define_column_region! {
    region: "ivc_state",
    start: MAIN_COLUMN_COUNT,
    width: pub IVC_COLUMNS_COUNT,
    families: pub IVC_COLUMN_FAMILIES,
    indices: pub,
    columns: [
        COL_CURR_PHASE_BEFORE: (Bits(2)) => "internal phase of curr for enforcing cross-step consistency",
        COL_CURR_PHASE_AFTER: (Bits(2)) => "internal phase of curr in the next step",
        COL_CALL_SP_BEFORE: U32 => "call stack pointer before",
        COL_CALL_SP_AFTER: U32 => "call stack pointer after",
        COL_NEXT_UTXO_ID_BEFORE: U32 => "utxo id allocator",
        COL_NEXT_UTXO_ID_AFTER: U32 => "utxo id allocator",
        COL_ENABLED_METHOD_LOG_LEN_BEFORE: U32 => "next free enabled-method append-log entry",
        COL_ENABLED_METHOD_LOG_LEN_AFTER: U32 => "next free enabled-method append-log entry after this step",
        COL_PENDING_CTOR_PRESENT_BEFORE: Boolean => "whether a constructor resource key is pending",
        COL_PENDING_CTOR_PRESENT_AFTER: Boolean => "whether a constructor resource key remains pending",
        COL_PENDING_CTOR_HOLDER_BEFORE: U32 => "packed holder coroutine id in the pending constructor key",
        COL_PENDING_CTOR_HOLDER_AFTER: U32 => "packed holder coroutine id in the pending constructor key",
        COL_PENDING_CTOR_HANDLE_BEFORE: U32 => "resource handle in the pending constructor key",
        COL_PENDING_CTOR_HANDLE_AFTER: U32 => "resource handle in the pending constructor key",
    ]
}

define_column_region! {
    region: "trace_commitment",
    start: MAIN_COLUMN_COUNT + IVC_COLUMNS_COUNT,
    width: pub TRACE_COMM_COLUMN_COUNT,
    families: pub TRACE_COMM_COLUMN_FAMILIES,
    indices: pub,
    columns: [
        COL_IN: [Field; 4] => "the carried 4-limb poseidon2 commitment",
        COL_OUT: [Field; 4] => "the carried 4-limb poseidon2 commitment",
    ]
}

pub const SELECTORS: [usize; 8] = [
    COL_SEL_NEW_UTXO,
    COL_SEL_ENTER_CONSTRUCTOR,
    COL_SEL_YIELD_BEGIN,
    COL_SEL_REGISTER_METHOD,
    COL_SEL_RETURN,
    COL_SEL_CALL_METHOD,
    COL_SEL_ENTER_METHOD,
    COL_SEL_PADDING,
];

pub(crate) fn range_check_layout() -> &'static RangeCheckLayout {
    static LAYOUT: OnceLock<RangeCheckLayout> = OnceLock::new();

    LAYOUT.get_or_init(|| {
        RangeCheckLayout::new(
            MAIN_COLUMN_FAMILIES
                .iter()
                .copied()
                .chain(IVC_COLUMN_FAMILIES.iter().copied())
                .chain(TRACE_COMM_COLUMN_FAMILIES.iter().copied()),
            RangeCheckBitFamily {
                region: "range_check_bits",
                name: "RANGE_CHECK_BITS",
                role: "Boolean decomposition bits for bounded columns",
            },
        )
        .expect("valid range-check layout")
    })
}
