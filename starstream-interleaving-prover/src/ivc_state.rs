use neo_application::{ContinuityGroup, ContinuityLink};
use neo_math::F;

use crate::ccs::layout::*;

/// Circuit encoding of Quint's `CurrPhase` variants.
///
/// The values are chosen so the phase preconditions used by the current spec
/// have simple two-bit encodings:
///
/// - constructor entry requires `CtorEnterPending` (`01`),
/// - method entry requires `MethodEnterPending` (`10`), and
/// - return allows `Executing` or `Yield` (both bits equal).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum CurrPhase {
    Executing = 0b00,
    CtorEnterPending = 0b01,
    MethodEnterPending = 0b10,
    Yield = 0b11,
}

impl CurrPhase {
    pub(crate) const fn value(self) -> u8 {
        self as u8
    }
}

/// Packed circuit refinement of Quint's tagged `CoroutineId` union.
///
/// The low bit is the kind tag, leaving the remaining bits for the local ID:
/// `Coord(i) = 2*i` and `Utxo(i) = 2*i + 1`.
// TODO: Bind the payloads to the transaction's coordinator/UTXO domains once
// those counts become part of the circuit instance.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum CoroutineId {
    Coord(u32),
    Utxo(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub(crate) enum CoroutineKind {
    Coord = 0,
    Utxo = 1,
}

impl CoroutineKind {
    pub(crate) const fn tag(self) -> u32 {
        self as u32
    }
}

impl CoroutineId {
    pub(crate) fn encoded(self) -> u32 {
        let (id, kind) = match self {
            Self::Coord(id) => (id, CoroutineKind::Coord),
            Self::Utxo(id) => (id, CoroutineKind::Utxo),
        };

        id.checked_mul(2)
            .and_then(|id| id.checked_add(kind.tag()))
            .expect("coroutine ID fits the packed u32 encoding")
    }

    pub(crate) fn field(self) -> F {
        F::new(u64::from(self.encoded()))
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
            name: "call_stack_pointer_continuity",
            role: "row[i].COL_CALL_SP_AFTER must match row[i+1].COL_CALL_SP_BEFORE",
            links: vec![link(COL_CALL_SP_AFTER, COL_CALL_SP_BEFORE)],
        },
        ContinuityGroup {
            name: "next_utxo_id_continuity",
            role: "row[i].COL_NEXT_UTXO_ID_AFTER must match row[i+1].COL_NEXT_UTXO_ID_BEFORE",
            links: vec![link(COL_NEXT_UTXO_ID_AFTER, COL_NEXT_UTXO_ID_BEFORE)],
        },
        ContinuityGroup {
            name: "pending_constructor_key_continuity",
            role: "the pending constructor resource key must carry across adjacent rows",
            links: vec![
                link(
                    COL_PENDING_CTOR_PRESENT_AFTER,
                    COL_PENDING_CTOR_PRESENT_BEFORE,
                ),
                link(
                    COL_PENDING_CTOR_HOLDER_AFTER,
                    COL_PENDING_CTOR_HOLDER_BEFORE,
                ),
                link(
                    COL_PENDING_CTOR_HANDLE_AFTER,
                    COL_PENDING_CTOR_HANDLE_BEFORE,
                ),
            ],
        },
    ]
}
