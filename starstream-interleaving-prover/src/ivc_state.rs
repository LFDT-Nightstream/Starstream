use neo_application::{ContinuityGroup, ContinuityLink};
use neo_math::F;

use crate::ccs::layout::*;

/// Circuit encoding of Quint's `CurrPhase` variants.
///
/// The bits make the multi-phase opcode predicates linear:
///
/// - bit 1 rejects raising once registration has started,
/// - bit 2 rejects ordinary returns from a handler,
/// - `ConstructorExecuting` and `Registering` share bit 0 for registration entry, and
/// - pending phases are excluded by the opcode-specific bit predicates.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum CurrPhase {
    Executing = 0b0000,
    ConstructorExecuting = 0b0001,
    Registering = 0b0011,
    EffectHandlerExecuting = 0b0100,
    CtorEnterPending = 0b0110,
    CallEnterPending = 0b0111,
    EffectHandlerPending = 0b1110,
    EffectResumePending = 0b1111,
    CallResultPending = 0b1000,
    NewResourceHandlePending = 0b1001,
}

impl CurrPhase {
    pub(crate) const fn value(self) -> u8 {
        self as u8
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum TxPhase {
    Loading = 0,
    // Execution has started; output processing also remains in this phase.
    Running = 1,
    Finished = 2,
}

/// Packed circuit refinement of Quint's tagged `CoroutineId` union.
///
/// The low two bits encode the kind (coordinator 0, UTXO 1, token 2).
/// The upper 30 bits hold the transaction-local index.
// TODO: Bind the payloads to the transaction's coordinator/UTXO domains once
// those counts become part of the circuit instance.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CoroutineId {
    Coord(u32),
    Utxo(u32),
    Token(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub(crate) enum CoroutineKind {
    Coord = 0,
    Utxo = 1,
    Token = 2,
}

impl CoroutineKind {
    pub(crate) const fn tag(self) -> u32 {
        self as u32
    }
}

impl CoroutineId {
    /// Decode a local index and its kind, rejecting the reserved tag.
    pub fn from_packed(packed: u32) -> Option<Self> {
        let id = packed >> 2;
        match packed & 3 {
            0 => Some(Self::Coord(id)),
            1 => Some(Self::Utxo(id)),
            2 => Some(Self::Token(id)),
            _ => None,
        }
    }

    /// Pack the kind tag and local ID. Panics if the ID exceeds 30 bits.
    pub fn encoded(self) -> u32 {
        self.checked_encoded()
            .expect("coroutine ID fits the packed u32 encoding")
    }

    /// Returns the packed ID if the local index fits in 30 bits.
    pub fn checked_encoded(self) -> Option<u32> {
        let (id, kind) = match self {
            Self::Coord(id) => (id, CoroutineKind::Coord),
            Self::Utxo(id) => (id, CoroutineKind::Utxo),
            Self::Token(id) => (id, CoroutineKind::Token),
        };

        id.checked_mul(4).and_then(|id| id.checked_add(kind.tag()))
    }

    pub(crate) fn field(self) -> F {
        F::new(u64::from(self.encoded()))
    }

    pub(crate) fn local_index(self) -> F {
        let (Self::Coord(id) | Self::Utxo(id) | Self::Token(id)) = self;
        F::new(u64::from(id))
    }
}

const fn link(previous_step_column: usize, next_step_column: usize) -> ContinuityLink {
    ContinuityLink {
        previous_step_column,
        next_step_column,
    }
}

pub(crate) fn build_ivc_state_continuity_links() -> Vec<ContinuityGroup> {
    vec![
        ContinuityGroup {
            name: "transaction_digest",
            role: "transaction IO and final instance roots in scan order",
            links: COL_IO_AFTER
                .into_iter()
                .zip(COL_IO_BEFORE)
                .map(|(a, b)| link(a, b))
                .collect(),
        },
        ContinuityGroup {
            name: "transaction_continuity",
            role: "transaction phase, last input ABI nonemptiness, output cursor and ABI enumeration progress",
            links: vec![
                link(COL_TX_PHASE_AFTER, COL_TX_PHASE_BEFORE),
                link(COL_LAST_INPUT_HAS_ABI_AFTER, COL_LAST_INPUT_HAS_ABI_BEFORE),
                link(COL_OUTPUT_CURSOR_AFTER, COL_OUTPUT_CURSOR_BEFORE),
                link(COL_COORD_CURSOR_AFTER, COL_COORD_CURSOR_BEFORE),
                link(COL_ABI_READ_REMAINING_AFTER, COL_ABI_READ_REMAINING_BEFORE),
                link(COL_ABI_READ_ORDINAL_AFTER, COL_ABI_READ_ORDINAL_BEFORE),
            ],
        },
        ContinuityGroup {
            name: "curr_continuity",
            role: "row[i].COL_CURR_AFTER must match row[i+1].COL_CURR_BEFORE",
            links: vec![link(COL_CURR_AFTER, COL_CURR_BEFORE)],
        },
        ContinuityGroup {
            name: "curr_phase_continuity",
            role: "row[i].COL_CURR_PHASE_AFTER must match row[i+1].COL_CURR_PHASE_BEFORE",
            links: vec![link(COL_CURR_PHASE_AFTER, COL_CURR_PHASE_BEFORE)],
        },
        ContinuityGroup {
            name: "instance_stack_depth",
            role: "runtime invocation depth survives effect dispatch and nested handlers",
            links: vec![link(COL_INSTANCE_DEPTH_AFTER, COL_INSTANCE_DEPTH_BEFORE)],
        },
        ContinuityGroup {
            name: "effect_scope_continuity",
            role: "effect head, local scope depth and arena allocator carry across rows",
            links: vec![
                link(COL_EFFECT_HEAD_AFTER, COL_EFFECT_HEAD_BEFORE),
                link(COL_EFFECT_DEPTH_AFTER, COL_EFFECT_DEPTH_BEFORE),
                link(COL_EFFECT_ARENA_LEN_AFTER, COL_EFFECT_ARENA_LEN_BEFORE),
            ],
        },
        ContinuityGroup {
            name: "call_handoff",
            role: "argument, method and result that a call, return or handler exit hands to the next step",
            links: COL_PENDING_VALUE_AFTER
                .into_iter()
                .zip(COL_PENDING_VALUE_BEFORE)
                .chain(
                    COL_PENDING_METHOD_AFTER
                        .into_iter()
                        .zip(COL_PENDING_METHOD_BEFORE),
                )
                .map(|(a, b)| link(a, b))
                .collect(),
        },
        ContinuityGroup {
            name: "call_stack_pointer_continuity",
            role: "row[i].COL_CALL_SP_AFTER must match row[i+1].COL_CALL_SP_BEFORE",
            links: vec![link(COL_CALL_SP_AFTER, COL_CALL_SP_BEFORE)],
        },
        ContinuityGroup {
            name: "next_token_id_continuity",
            role: "token allocator carries across adjacent rows",
            links: vec![link(COL_NEXT_TOKEN_ID_AFTER, COL_NEXT_TOKEN_ID_BEFORE)],
        },
        ContinuityGroup {
            name: "next_utxo_id_continuity",
            role: "row[i].COL_NEXT_UTXO_ID_AFTER must match row[i+1].COL_NEXT_UTXO_ID_BEFORE",
            links: vec![link(COL_NEXT_UTXO_ID_AFTER, COL_NEXT_UTXO_ID_BEFORE)],
        },
        ContinuityGroup {
            name: "next_coordinator_id_continuity",
            role: "the coordinator allocator must carry across adjacent rows",
            links: vec![link(COL_NEXT_COORD_ID_AFTER, COL_NEXT_COORD_ID_BEFORE)],
        },
        ContinuityGroup {
            name: "enabled_method_log_length_continuity",
            role: "the next free enabled-method log entry must carry across adjacent rows",
            links: vec![link(
                COL_ENABLED_METHOD_LOG_LEN_AFTER,
                COL_ENABLED_METHOD_LOG_LEN_BEFORE,
            )],
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::CoroutineId;

    #[test]
    fn checked_encoding_preserves_tags_and_rejects_overflow() {
        let max_id = u32::MAX / 4;
        for (id, packed) in [
            (CoroutineId::Coord(0), 0),
            (CoroutineId::Utxo(0), 1),
            (CoroutineId::Token(0), 2),
            (CoroutineId::Coord(1), 4),
            (CoroutineId::Utxo(1), 5),
            (CoroutineId::Token(1), 6),
            (CoroutineId::Coord(max_id), u32::MAX - 3),
            (CoroutineId::Utxo(max_id), u32::MAX - 2),
            (CoroutineId::Token(max_id), u32::MAX - 1),
        ] {
            assert_eq!(id.checked_encoded(), Some(packed));
            assert_eq!(id.encoded(), packed);
            assert_eq!(CoroutineId::from_packed(packed), Some(id));
        }
        assert_eq!(CoroutineId::from_packed(u32::MAX), None);
        for id in [max_id + 1, u32::MAX] {
            assert_eq!(CoroutineId::Coord(id).checked_encoded(), None);
            assert_eq!(CoroutineId::Utxo(id).checked_encoded(), None);
            assert_eq!(CoroutineId::Token(id).checked_encoded(), None);
        }
    }
}
