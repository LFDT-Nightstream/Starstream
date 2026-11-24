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
        COL_SEL_RECEIVE_NEW_UTXO_HANDLE: Boolean => "selector for the ReceiveNewUtxoHandle action",
        COL_SEL_ENTER_CONSTRUCTOR: Boolean => "selector for the EnterConstructor action",
        COL_SEL_YIELD_BEGIN: Boolean => "selector for the YieldBegin action",
        COL_SEL_REGISTER_METHOD: Boolean => "selector for the RegisterMethod action",
        COL_SEL_RETURN: Boolean => "selector for the Return action",
        COL_SEL_CALL_METHOD: Boolean => "selector for the CallMethod action",
        COL_SEL_RECEIVE_CALL_RESULT: Boolean => "selector for the ReceiveCallResult action",
        COL_SEL_ENTER_CALL: Boolean => "selector for the EnterCall action",
        COL_SEL_RAISE_EFFECT: Boolean => "selector for the RaiseEffect action",
        COL_SEL_PUSH_EFFECT_HANDLER_FRAME: Boolean => "selector for the PushEffectHandlerFrame action",
        COL_SEL_ADD_EFFECT_HANDLER: Boolean => "selector for the AddEffectHandler action",
        COL_SEL_ASSERT_SIGNED_BY: Boolean => "signer membership check",
        COL_SEL_POP_EFFECT_HANDLER_FRAME: Boolean => "selector for the PopEffectHandlerFrame action",
        COL_SEL_SKIP_EFFECT_HANDLER: Boolean => "selector for the SkipEffectHandler host search step",
        COL_SEL_ENTER_EFFECT_HANDLER: Boolean => "selector for the EnterEffectHandler action",
        COL_SEL_EXIT_EFFECT_HANDLER: Boolean => "selector for the ExitEffectHandler action",
        COL_SEL_RESUME_EFFECT: Boolean => "selector for the ResumeEffect action",
        COL_SEL_PADDING: Boolean => "state-preserving circuit-only padding slot",
        COL_SEL_SET_STORAGE: Boolean => "load an input UTXO",
        COL_SEL_PRELOAD_METHOD: Boolean => "load a ledger ABI entry",
        COL_SEL_GET_STORAGE: Boolean => "export the next surviving UTXO",
        COL_SEL_SKIP_CONSUMED: Boolean => "skip the next consumed UTXO",
        COL_SEL_FINISH_TRANSACTION: Boolean => "finish the output scan",
        COL_SEL_FINALIZE_COORDINATOR: Boolean => "enumerate the next coordinator's final trace root, in allocation order",
        COL_SEL_CALL_COORDINATOR: Boolean => "selector for the CallCoordinator action",
        COL_SEL_BEGIN_EXECUTION: Boolean => "the transaction context calls the entry script once inputs are loaded",
        COL_METHOD_APPEND: Boolean => "RegisterMethod or PreloadMethod",
        COL_EVENT_ACTIVE: Boolean => "this row emits a program event",
        COL_EVENT_OWNER: U32 => "packed instance owning the program event or final trace root being read",
        COL_UTXO_EVENT: Boolean => "this row emits a program event owned by a UTXO",
        COL_COORD_EVENT: Boolean => "this row emits a program event owned by a coordinator",
        COL_BOUNDARY_UTXO: U32 => "packed UTXO ID for loading or the output scan",
        COL_OUTPUT_REMAINING: U32 => "instances after this one in the active scan: next_utxo_id - output_cursor - 1 on output scans, next_coordinator_id - coordinator_cursor - 1 on coordinator finalization",
        COL_CURR_BEFORE: U32 => "packed coroutine id that has the turn",
        COL_CURR_AFTER: U32 => "packed coroutine id that has the turn in the next step",
        COL_INSTANCE_DEPTH_BEFORE: U32 => "runtime invocation depth excluding the entry coordinator",
        COL_INSTANCE_DEPTH_AFTER: U32 => "runtime invocation depth after this step",
        COL_INSTANCE_PUSH: Boolean => "constructor, method call or script call pushes a runtime invocation",
        COL_INSTANCE_POP: Boolean => "receiving a program result pops its invocation",
        COL_EFFECT_FRAME_ID: U32 => "compiled handler id: written on scope push, read on handler entry",
        COL_EFFECT_SCOPE_READ: Boolean => "pop, skip or handler entry reads the head scope's parent",
        COL_EFFECT_REGISTERED: Boolean => "head scope registers the method-index effect: add writes 1, entry reads 1, skip reads 0",
        COL_EFFECT_HEAD_INVERSE: Field => "inverse proving a nonempty head on skip and handler entry",
        COL_EFFECT_DEPTH_INVERSE: Field => "inverse proving a local scope exists on add and pop",
        COL_EFFECT_RESTORE_DELTA: Field => "handled flag times (saved head - head) on handler exit",
        COL_PHASE_BEFORE_BITS_1_AND_2: Boolean => "product of phase-before bits 1 and 2: set only for entry-pending and effect-pending phases",
        COL_CALL_STACK_PUSH: Boolean => "true when a call (including the transaction's entry call) happens and we need to keep track of the caller's context",
        COL_CALL_STACK_POP: Boolean => "true when consuming a completed call or resuming an effect",
        COL_CALL_STACK_TOP: Boolean => "true when peeking at the top of the call stack without popping",
        COL_CALL_FRAME_IS_EFFECT: Boolean => "whether the call frame is an effect frame",
        COL_CALL_FRAME_CONSTRUCTOR: Boolean => "program call is a constructor call",
        COL_CALL_FRAME_SAVED_PHASE: (Bits(4)) => "phase saved in an effect frame and restored on resume",
        COL_RETURN_REQUIRES_UNIT: Boolean => "return is from a constructor or the entry coordinator",
        COL_CALL_FRAME_SAVED_EFFECT_HEAD: U32 => "caller's effect head saved on push, restored by an accepted handler exit",
        COL_CALL_FRAME_SAVED_EFFECT_DEPTH: U32 => "caller's local scope depth saved on push, restored on return and handler exit",
        COL_EFFECT_HANDLED: Boolean => "handler exit status: continue search = 0, resume = 1",
        COL_CALL_SP_BEFORE_INVERSE: Field => "inverse proving that an execution row starts with a nonempty call stack",
        // TODO: limit sp so that this doesn't overflow
        COL_CALL_STACK_FRAME: U32 => "call-stack frame accessed: SP on a push, SP - 1 on a pop or peek",
        COL_CALL_STACK_MUL_STRIDE_8: [U32; 8] => "accessed frame * 8 + i, for the frame's eight-word fields",
        COL_CALL_STACK_EXPECTED_ARG_VALUE: [U32; 8] => "opaque argument root, little-endian 32-bit limbs; an effect frame stores them on raise and handler entry reads them",
        // using 8 limbs for exact sha256 repr for now (in 32-bit limbs), we
        // could improve this, but the memory argument as currently implemented
        // is 32-bit based, plus we'd have to drop 2 bits to use 4 limbs
        COL_METHOD_HASH_VALUE: [U32; 8] => "method hash limb bus: event identity (method, script, constructor or effect), pending-identity hand-off and method-table ROM lookup value",
        COL_METHOD_INDEX: (Bits(29)) => "method dictionary index or signer ROM index, selected by opcode",
        COL_METHOD_LOOKUP: Boolean => "a method, ABI or effect step resolves a method hash through the method table",
        COL_METHOD_TABLE_ADDR: [U32; 8] => "method_index * 8 + hash limb offset",
        // The signer lookup constraint gives zero on inactive rows. On active rows
        // a 29-bit index addresses up to eight limbs within a 32-bit ROM address.
        COL_SIGNER_KEY_ADDR: [(Implied(32)); SIGNER_KEY_LIMBS] => "signer index * signer key stride + limb offset",
        COL_ABI_GENERATION_READ: Boolean => "RegisterMethod, PreloadMethod, CallMethod or ReadAbi reads a UTXO's current ABI generation",
        COL_CALL_TARGET: U32 => "packed coroutine id that gets control in the next step",
        COL_CALL_TARGET_IS_ZERO: Boolean => "whether the call target is Coord(0), the transaction context",
        COL_CALL_TARGET_INVERSE: Field => "inverse proving that a call target is nonzero",
        COL_ABI_METHOD_COUNT_ADDR: U32 => "UTXO local index (packed id without the tag) keying the current-generation registration count",
        COL_SEL_READ_ABI: Boolean => "host-synthesized output ABI enumeration",
        COL_ABI_READ_REMAINING_BEFORE: (Bits(8)) => "unread registrations of the last exported UTXO",
        COL_ABI_READ_REMAINING_AFTER: (Bits(8)) => "remaining registrations after GetStorage loads the count or ReadAbi decrements it",
        COL_ABI_READ_ORDINAL_BEFORE: (Bits(8)) => "next output ABI ordinal",
        COL_ABI_READ_ORDINAL_AFTER: (Bits(8)) => "output ABI ordinal after this row",
        COL_ABI_METHOD_COUNT_BEFORE: (Bits(8)) => "registration count before update, or final count on output reads; includes duplicates",
        COL_ABI_METHOD_COUNT_AFTER: (Bits(8)) => "registration count after append (+1) or YieldBegin (0)",
        COL_ABI_METHOD_COUNT_INVERSE: Field => "inverse proving a surviving output has a nonzero registration count",
        COL_ENABLED_METHOD_LOG_ORDINAL: (Bits(8)) => "zero-based registration ordinal: written on append, read during output ABI enumeration",
        COL_ABI_METHOD_COUNT_WRITE: Boolean => "RegisterMethod/PreloadMethod increments count; YieldBegin clears it",
        COL_ABI_METHOD_COUNT_READ: Boolean => "GetStorage/SkipConsumed reads final liveness",
        COL_ENABLED_METHOD_LOG_ADDR: U32 => "append-log entry written by RegisterMethod/PreloadMethod or selected by CallMethod",
        COL_ENABLED_METHOD_LOG_UTXO: U32 => "packed UTXO id stored in the enabled-method log entry",
        COL_ENABLED_METHOD_LOG_GENERATION: U32 => "ABI generation stored in the enabled-method log entry",
        COL_ABI_GENERATION_ADDR: U32 => "UTXO local index (packed id without the tag) whose current ABI generation is accessed",
        COL_ABI_GENERATION_BEFORE: U32 => "current ABI generation read from memory",
        COL_ABI_GENERATION_AFTER: U32 => "next ABI generation written by YieldBegin",

        COL_RESOURCE_RESOLVER_ADDR_CID: U32 => "packed holder coroutine id in the resource key",
        COL_RESOURCE_RESOLVER_ADDR_HANDLE: U32 => "resource handle in the resource key",
        COL_RESOURCE_RESOLVER_VALUE: U32 => "the packed coroutine id assigned to the resource at (cid, handle)",
        COL_RESOURCE_RESOLVER_BEFORE: Field => "zero old value required when binding a previously unbound resource",
        COL_RESOURCE_RESOLVER_WRITE: Boolean => "SetStorage or ReceiveNewUtxoHandle writes a resource binding",
        COL_RESOURCE_RESOLVER_READ: Boolean => "1 if reading from the resource resolver map (on call_method)",

        // TODO: limit curr side so that this doesn't overflow
        COL_EVENT_OWNER_STRIDE_8: [U32; 8] => "event owner local index * 8 + i, the address in its kind's trace digest RAM",


    ]
}

define_column_region! {
    region: "ivc_state",
    start: MAIN_COLUMN_COUNT,
    width: pub IVC_COLUMNS_COUNT,
    families: pub IVC_COLUMN_FAMILIES,
    indices: pub,
    columns: [
        COL_TX_PHASE_BEFORE: (Bits(2)) => "Loading=0, Running (including output processing)=1, Finished=2",
        COL_TX_PHASE_AFTER: (Bits(2)) => "transaction phase after this row",
        COL_LAST_INPUT_HAS_ABI_BEFORE: Boolean => "last loaded UTXO has a nonempty ABI (or none has been loaded yet)",
        COL_LAST_INPUT_HAS_ABI_AFTER: Boolean => "last loaded UTXO has a nonempty ABI after this row (or none has been loaded yet)",
        COL_OUTPUT_CURSOR_BEFORE: U32 => "next UTXO ID to finalize",
        COL_OUTPUT_CURSOR_AFTER: U32 => "next UTXO ID after this row",
        COL_NEXT_COORD_ID_BEFORE: U32 => "coordinator id allocator; Coord(0) is the transaction context",
        COL_NEXT_COORD_ID_AFTER: U32 => "coordinator id allocator after this row",
        COL_COORD_CURSOR_BEFORE: U32 => "next coordinator ID to finalize",
        COL_COORD_CURSOR_AFTER: U32 => "next coordinator ID after this row",
        COL_CURR_PHASE_BEFORE: (Bits(4)) => "internal phase of curr for enforcing cross-step consistency",
        COL_CURR_PHASE_AFTER: (Bits(4)) => "internal phase of curr in the next step",
        COL_CALL_SP_BEFORE: U32 => "call stack pointer before",
        COL_CALL_SP_AFTER: U32 => "call stack pointer after",
        COL_NEXT_UTXO_ID_BEFORE: U32 => "utxo id allocator",
        COL_NEXT_UTXO_ID_AFTER: U32 => "utxo id allocator",
        COL_ENABLED_METHOD_LOG_LEN_BEFORE: U32 => "next free enabled-method append-log entry",
        COL_ENABLED_METHOD_LOG_LEN_AFTER: U32 => "next free enabled-method append-log entry after this step",
        COL_EFFECT_HEAD_BEFORE: U32 => "visible scope chain head (node id + 1, 0 if empty); search cursor while an effect is pending",
        COL_EFFECT_HEAD_AFTER: U32 => "effect head after this step",
        COL_EFFECT_DEPTH_BEFORE: U32 => "scopes pushed and not yet popped by the current invocation",
        COL_EFFECT_DEPTH_AFTER: U32 => "local scope depth after this step",
        COL_EFFECT_ARENA_LEN_BEFORE: U32 => "scopes allocated so far; ids are never reused",
        COL_EFFECT_ARENA_LEN_AFTER: U32 => "scopes allocated after this step",
        COL_PENDING_ROOT_BEFORE: [Field; 4] => "hand-off register: the argument root of the pending program call, or the result root of the pending return or handler exit",
        COL_PENDING_ROOT_AFTER: [Field; 4] => "hand-off root after this row",
        COL_PENDING_METHOD_BEFORE: [Field; 8] => "hand-off register: method, script or constructor identity limbs of the pending program call",
        COL_PENDING_METHOD_AFTER: [Field; 8] => "hand-off identity after this row",
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
        COL_IN_WORDS: [U32; 8] => "canonical input digest limbs for trace RAM",
        COL_OUT_WORDS: [U32; 8] => "canonical output digest limbs for trace RAM",
        COL_ARGUMENT_ROOT: [Field; 4] => "opaque argument root of this row's event, tied to its 32-bit limbs",
        COL_RESULT_ROOT: [Field; 4] => "opaque result root of this row's event",
        COL_CANONICAL_HIGH_MAX: [Boolean; 12] => "whether the high u32 limb equals 0xffffffff",
        COL_CANONICAL_HIGH_INV: [Field; 12] => "inverse of high limb minus 0xffffffff",
        COL_POSEIDON_INPUT_ROOT: [Field; 12] => "selected input roots for three shared program/transaction compression gadgets",
        COL_EVENT_BLOCKS: [Field; 24] => "three shared program/transaction blocks; program blocks precede transaction blocks",
        COL_EVENT_HASHES: [Field; 12] => "shared compression outputs selected separately for each chain",
        COL_EVENT_AUX: [Field; 3 * neo_application::EVENT_COMMITMENT_AUX_COLUMNS] => "shared Poseidon compression auxiliaries",
        COL_IO_BEFORE: [Field; 4] => "transaction IO and instance-root digest before this row",
        COL_IO_AFTER: [Field; 4] => "transaction IO and instance-root digest after this row",
    ]
}

pub(crate) const SIGNER_KEY_LIMBS: usize = u64::BITS as usize / u32::BITS as usize;
/// Signer and method lookups occur on disjoint opcodes and share an advice column.
pub(crate) const COL_SIGNER_INDEX: usize = COL_METHOD_INDEX;

pub const SELECTORS: [usize; 28] = [
    COL_SEL_ASSERT_SIGNED_BY,
    COL_SEL_CALL_COORDINATOR,
    COL_SEL_FINALIZE_COORDINATOR,
    COL_SEL_BEGIN_EXECUTION,
    COL_SEL_READ_ABI,
    COL_SEL_NEW_UTXO,
    COL_SEL_RECEIVE_NEW_UTXO_HANDLE,
    COL_SEL_ENTER_CONSTRUCTOR,
    COL_SEL_YIELD_BEGIN,
    COL_SEL_REGISTER_METHOD,
    COL_SEL_RETURN,
    COL_SEL_CALL_METHOD,
    COL_SEL_RECEIVE_CALL_RESULT,
    COL_SEL_ENTER_CALL,
    COL_SEL_RAISE_EFFECT,
    COL_SEL_PUSH_EFFECT_HANDLER_FRAME,
    COL_SEL_ADD_EFFECT_HANDLER,
    COL_SEL_POP_EFFECT_HANDLER_FRAME,
    COL_SEL_SKIP_EFFECT_HANDLER,
    COL_SEL_ENTER_EFFECT_HANDLER,
    COL_SEL_EXIT_EFFECT_HANDLER,
    COL_SEL_RESUME_EFFECT,
    COL_SEL_PADDING,
    COL_SEL_SET_STORAGE,
    COL_SEL_PRELOAD_METHOD,
    COL_SEL_GET_STORAGE,
    COL_SEL_SKIP_CONSUMED,
    COL_SEL_FINISH_TRANSACTION,
];

pub(crate) fn range_check_layout() -> &'static RangeCheckLayout {
    static LAYOUT: OnceLock<RangeCheckLayout> = OnceLock::new();

    LAYOUT.get_or_init(|| {
        RangeCheckLayout::new(
            MAIN_COLUMN_FAMILIES
                .iter()
                .cloned()
                .chain(IVC_COLUMN_FAMILIES.iter().cloned())
                .chain(TRACE_COMM_COLUMN_FAMILIES.iter().cloned()),
            RangeCheckBitFamily {
                region: "range_check_bits",
                name: "RANGE_CHECK_BITS",
                role: "Boolean decomposition bits for bounded columns",
            },
        )
        .expect("valid range-check layout")
    })
}
