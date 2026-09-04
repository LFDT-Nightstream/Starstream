use neo_application::{ContinuityGroup, ContinuityLink};

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
    ]
}
