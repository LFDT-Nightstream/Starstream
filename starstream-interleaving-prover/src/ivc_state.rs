use neo_application::{ContinuityGroup, ContinuityLink};

use crate::ccs::layout::*;

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
    ]
}
