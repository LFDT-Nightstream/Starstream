use neo_math::F;
use p3_field::{PrimeCharacteristicRing, PrimeField64};

use crate::{
    Unsatisfied,
    ccs::layout::*,
    ivc_state::{CoroutineId, TxPhase},
};

pub(crate) enum TerminalClaim {
    Execution,
    Transaction { commitment: [F; 4] },
}

impl TerminalClaim {
    pub(crate) fn check(&self, value: impl Fn(usize) -> F) -> Result<(), Unsatisfied> {
        check_execution_end(value(COL_CALL_SP_AFTER), value(COL_CURR_AFTER))?;
        let phase = match self {
            Self::Execution => TxPhase::Running,
            Self::Transaction { .. } => TxPhase::Finished,
        };
        if value(COL_TX_PHASE_AFTER) != F::new(phase as u64) {
            return Err(Unsatisfied::TransactionStatement);
        }
        if let Self::Transaction { commitment } = self
            && COL_IO_AFTER.map(value) != *commitment
        {
            return Err(Unsatisfied::TransactionCommitment);
        }
        Ok(())
    }
}

pub(crate) fn check_execution_end(sp: F, curr: F) -> Result<(), Unsatisfied> {
    if sp != F::ZERO {
        return Err(Unsatisfied::TerminalCallStackNotEmpty { actual: sp });
    }
    if !u32::try_from(curr.as_canonical_u64())
        .ok()
        .is_some_and(|packed| matches!(CoroutineId::from_packed(packed), CoroutineId::Coord(_)))
    {
        return Err(Unsatisfied::TerminalCoroutineNotCoordinator { actual: curr });
    }
    Ok(())
}
