use crate::ccs::layout::*;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub(crate) enum Opcode {
    NewUtxo = 0,
    EnterConstructor,
    YieldBegin,
    RegisterMethod,
    Return,
    CallMethod,
    EnterMethod,
    Padding,
    SetStorage,
    PreloadMethod,
    GetStorage,
    SkipConsumed,
    FinishTransaction,
    ReadAbi,
}

impl From<&starstream_interleaving_spec::Step> for Opcode {
    fn from(value: &starstream_interleaving_spec::Step) -> Self {
        match value {
            starstream_interleaving_spec::Step::ReadAbi { .. } => Self::ReadAbi,
            starstream_interleaving_spec::Step::SetStorage { .. } => Self::SetStorage,
            starstream_interleaving_spec::Step::PreloadMethod { .. } => Self::PreloadMethod,
            starstream_interleaving_spec::Step::GetStorage { .. } => Self::GetStorage,
            starstream_interleaving_spec::Step::SkipConsumed => Self::SkipConsumed,
            starstream_interleaving_spec::Step::FinishTransaction => Self::FinishTransaction,
            starstream_interleaving_spec::Step::NewUtxo {
                arguments: _,
                resource: _,
            } => Opcode::NewUtxo,
            starstream_interleaving_spec::Step::EnterConstructor { arguments: _ } => {
                Opcode::EnterConstructor
            }
            starstream_interleaving_spec::Step::YieldBegin => Opcode::YieldBegin,
            starstream_interleaving_spec::Step::RegisterMethod { method: _ } => {
                Opcode::RegisterMethod
            }
            starstream_interleaving_spec::Step::Return { result: _ } => Opcode::Return,
            starstream_interleaving_spec::Step::CallMethod {
                resource: _,
                method: _,
                arguments: _,
                result: _,
            } => Opcode::CallMethod,
            starstream_interleaving_spec::Step::EnterMethod {
                method: _,
                arguments: _,
            } => Opcode::EnterMethod,
        }
    }
}

impl Opcode {
    pub fn is_execution(&self) -> bool {
        matches!(
            self,
            Self::NewUtxo
                | Self::EnterConstructor
                | Self::YieldBegin
                | Self::RegisterMethod
                | Self::Return
                | Self::CallMethod
                | Self::EnterMethod
        )
    }

    pub fn appends_method(&self) -> bool {
        matches!(self, Self::RegisterMethod | Self::PreloadMethod)
    }

    pub fn has_event(&self) -> bool {
        self.is_execution() || matches!(self, Self::SetStorage | Self::GetStorage)
    }

    pub fn scans_output(&self) -> bool {
        matches!(self, Self::GetStorage | Self::SkipConsumed)
    }

    pub fn phase_after(&self, before: crate::ivc_state::CurrPhase) -> crate::ivc_state::CurrPhase {
        use crate::ivc_state::CurrPhase;
        match self {
            Self::NewUtxo => CurrPhase::CtorEnterPending,
            Self::CallMethod => CurrPhase::MethodEnterPending,
            Self::EnterConstructor | Self::YieldBegin => CurrPhase::Yield,
            Self::EnterMethod | Self::Return => CurrPhase::Executing,
            Self::RegisterMethod
            | Self::Padding
            | Self::SetStorage
            | Self::PreloadMethod
            | Self::GetStorage
            | Self::SkipConsumed
            | Self::FinishTransaction => before,
            Self::ReadAbi => before,
        }
    }

    pub fn all() -> Vec<Self> {
        vec![
            Opcode::NewUtxo,
            Opcode::EnterConstructor,
            Opcode::YieldBegin,
            Opcode::RegisterMethod,
            Opcode::Return,
            Opcode::CallMethod,
            Opcode::EnterMethod,
            Opcode::Padding,
            Self::SetStorage,
            Self::PreloadMethod,
            Self::GetStorage,
            Self::SkipConsumed,
            Self::FinishTransaction,
            Self::ReadAbi,
        ]
    }

    pub fn pushes_to_call_stack(&self) -> bool {
        matches!(self, Self::NewUtxo | Self::CallMethod)
    }

    pub fn pops_from_call_stack(&self) -> bool {
        matches!(self, Self::Return)
    }

    pub fn peeks_call_stack_top(&self) -> bool {
        matches!(self, Self::EnterConstructor | Self::EnterMethod)
    }

    /// Whether this opcode transfers control to `COL_CALL_TARGET`.
    pub fn switches_curr(&self) -> bool {
        self.pushes_to_call_stack() || self.pops_from_call_stack()
    }

    pub fn selector(&self) -> usize {
        match self {
            Self::ReadAbi => COL_SEL_READ_ABI,
            Self::SetStorage => COL_SEL_SET_STORAGE,
            Self::PreloadMethod => COL_SEL_PRELOAD_METHOD,
            Self::GetStorage => COL_SEL_GET_STORAGE,
            Self::SkipConsumed => COL_SEL_SKIP_CONSUMED,
            Self::FinishTransaction => COL_SEL_FINISH_TRANSACTION,
            Opcode::Padding => COL_SEL_PADDING,
            Opcode::NewUtxo => COL_SEL_NEW_UTXO,
            Opcode::EnterConstructor => COL_SEL_ENTER_CONSTRUCTOR,
            Opcode::YieldBegin => COL_SEL_YIELD_BEGIN,
            Opcode::RegisterMethod => COL_SEL_REGISTER_METHOD,
            Opcode::Return => COL_SEL_RETURN,
            Opcode::CallMethod => COL_SEL_CALL_METHOD,
            Opcode::EnterMethod => COL_SEL_ENTER_METHOD,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::opcode::Opcode;

    #[test]
    fn all_is_exhaustive() {
        let all = Opcode::all();
        assert_eq!(
            all.iter().map(|op| *op as u8).collect::<Vec<_>>(),
            (0..all.len() as u8).collect::<Vec<_>>()
        );
    }

    #[test]
    fn call_stack_access_modes_are_disjoint() {
        for opcode in Opcode::all() {
            let modes = [
                opcode.pushes_to_call_stack(),
                opcode.pops_from_call_stack(),
                opcode.peeks_call_stack_top(),
            ];

            assert!(
                modes.into_iter().filter(|active| *active).count() <= 1,
                "{opcode:?} has overlapping call-stack access modes"
            );
        }
    }
}
