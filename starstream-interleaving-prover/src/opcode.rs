use neo_math::F;
use p3_field::PrimeCharacteristicRing;

use crate::ccs::layout::*;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub(crate) enum Opcode {
    NewUtxo = 0,
    ReceiveNewUtxoHandle,
    EnterConstructor,
    YieldBegin,
    RegisterMethod,
    Return,
    CallMethod,
    ReceiveMethodResult,
    EnterMethod,
    RaiseEffect,
    EnterEffectHandler,
    ExitEffectHandler,
    ResumeEffect,
    Padding,
    SetStorage,
    PreloadMethod,
    GetStorage,
    SkipConsumed,
    FinishTransaction,
    ReadAbi,
    FinalizeCoordinator,
    PushEffectHandlerFrame,
    AddEffectHandler,
    PopEffectHandlerFrame,
    SkipEffectHandler,
}

/// Linear terms for the sum of the supplied opcode selectors.
/// Includes exactly the supplied opcodes, including padding if present.
pub(crate) fn selector_sum(opcodes: impl IntoIterator<Item = Opcode>) -> Vec<(usize, F)> {
    opcodes
        .into_iter()
        .map(|op| (op.selector(), F::ONE))
        .collect()
}

impl From<&starstream_interleaving_spec::Step> for Opcode {
    fn from(value: &starstream_interleaving_spec::Step) -> Self {
        match value {
            starstream_interleaving_spec::Step::PushEffectHandlerFrame { .. } => {
                Self::PushEffectHandlerFrame
            }
            starstream_interleaving_spec::Step::AddEffectHandler { .. } => Self::AddEffectHandler,
            starstream_interleaving_spec::Step::PopEffectHandlerFrame => {
                Self::PopEffectHandlerFrame
            }
            starstream_interleaving_spec::Step::SkipEffectHandler => Self::SkipEffectHandler,
            starstream_interleaving_spec::Step::FinalizeCoordinator => Self::FinalizeCoordinator,
            starstream_interleaving_spec::Step::ReadAbi { .. } => Self::ReadAbi,
            starstream_interleaving_spec::Step::SetStorage { .. } => Self::SetStorage,
            starstream_interleaving_spec::Step::PreloadMethod { .. } => Self::PreloadMethod,
            starstream_interleaving_spec::Step::GetStorage { .. } => Self::GetStorage,
            starstream_interleaving_spec::Step::SkipConsumed => Self::SkipConsumed,
            starstream_interleaving_spec::Step::FinishTransaction => Self::FinishTransaction,
            starstream_interleaving_spec::Step::NewUtxo { arguments: _ } => Opcode::NewUtxo,
            starstream_interleaving_spec::Step::ReceiveNewUtxoHandle { .. } => {
                Opcode::ReceiveNewUtxoHandle
            }
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
            } => Opcode::CallMethod,
            starstream_interleaving_spec::Step::ReceiveMethodResult { .. } => {
                Opcode::ReceiveMethodResult
            }
            starstream_interleaving_spec::Step::EnterMethod {
                method: _,
                arguments: _,
            } => Opcode::EnterMethod,
            starstream_interleaving_spec::Step::RaiseEffect { .. } => Opcode::RaiseEffect,
            starstream_interleaving_spec::Step::EnterEffectHandler { .. } => {
                Opcode::EnterEffectHandler
            }
            starstream_interleaving_spec::Step::ExitEffectHandler { .. } => {
                Opcode::ExitEffectHandler
            }
            starstream_interleaving_spec::Step::ResumeEffect { .. } => Opcode::ResumeEffect,
        }
    }
}

impl Opcode {
    pub fn is_execution(&self) -> bool {
        matches!(
            self,
            Self::NewUtxo
                | Self::ReceiveNewUtxoHandle
                | Self::EnterConstructor
                | Self::YieldBegin
                | Self::RegisterMethod
                | Self::Return
                | Self::CallMethod
                | Self::ReceiveMethodResult
                | Self::EnterMethod
                | Self::RaiseEffect
                | Self::PushEffectHandlerFrame
                | Self::AddEffectHandler
                | Self::PopEffectHandlerFrame
                | Self::SkipEffectHandler
                | Self::EnterEffectHandler
                | Self::ExitEffectHandler
                | Self::ResumeEffect
        )
    }

    pub fn appends_method(&self) -> bool {
        matches!(self, Self::RegisterMethod | Self::PreloadMethod)
    }

    /// The search skip is a host step: it runs in the raiser's turn but
    /// neither the raiser nor the skipped scope's owner observes it.
    pub fn has_event(&self) -> bool {
        (self.is_execution() && *self != Self::SkipEffectHandler)
            || matches!(self, Self::SetStorage | Self::GetStorage)
    }

    pub fn scans_output(&self) -> bool {
        matches!(self, Self::GetStorage | Self::SkipConsumed)
    }

    /// Uses the read-only trace-root port. Program events instead use a write
    /// port that checks the previous root and stores the updated root.
    pub fn reads_trace_root(&self) -> bool {
        matches!(self, Self::SkipConsumed | Self::FinalizeCoordinator)
    }

    /// Phase transition for opcodes whose result does not depend on frame or
    /// event data. Dynamic transitions are assigned by normalization.
    pub fn fixed_phase_after(
        &self,
        before: crate::ivc_state::CurrPhase,
    ) -> Option<crate::ivc_state::CurrPhase> {
        use crate::ivc_state::CurrPhase;
        Some(match self {
            Self::NewUtxo => CurrPhase::CtorEnterPending,
            Self::CallMethod => CurrPhase::MethodEnterPending,
            Self::EnterConstructor => CurrPhase::ConstructorExecuting,
            Self::YieldBegin => CurrPhase::Registering,
            Self::EnterMethod => CurrPhase::Executing,
            Self::RegisterMethod => CurrPhase::Registering,
            Self::RaiseEffect => CurrPhase::EffectHandlerPending,
            Self::EnterEffectHandler => CurrPhase::EffectHandlerExecuting,
            Self::SkipEffectHandler => CurrPhase::EffectHandlerPending,
            Self::PushEffectHandlerFrame | Self::AddEffectHandler | Self::PopEffectHandlerFrame => {
                before
            }
            Self::ReceiveNewUtxoHandle
            | Self::ReceiveMethodResult
            | Self::Return
            | Self::ExitEffectHandler
            | Self::ResumeEffect => return None,
            Self::Padding
            | Self::SetStorage
            | Self::PreloadMethod
            | Self::GetStorage
            | Self::SkipConsumed
            | Self::FinishTransaction => before,
            Self::ReadAbi | Self::FinalizeCoordinator => before,
        })
    }

    pub fn all() -> Vec<Self> {
        vec![
            Opcode::NewUtxo,
            Opcode::ReceiveNewUtxoHandle,
            Opcode::EnterConstructor,
            Opcode::YieldBegin,
            Opcode::RegisterMethod,
            Opcode::Return,
            Opcode::CallMethod,
            Opcode::ReceiveMethodResult,
            Opcode::EnterMethod,
            Opcode::RaiseEffect,
            Opcode::EnterEffectHandler,
            Opcode::ExitEffectHandler,
            Opcode::ResumeEffect,
            Opcode::Padding,
            Self::SetStorage,
            Self::PreloadMethod,
            Self::GetStorage,
            Self::SkipConsumed,
            Self::FinishTransaction,
            Self::ReadAbi,
            Self::FinalizeCoordinator,
            Self::PushEffectHandlerFrame,
            Self::AddEffectHandler,
            Self::PopEffectHandlerFrame,
            Self::SkipEffectHandler,
        ]
    }

    /// Required effect flag, or None when this opcode does not check it.
    ///
    /// Pushes constrain the frame being written; other opcodes constrain the
    /// frame read from the stack top. Memory read ports derive from this predicate.
    pub fn required_effect_frame(&self) -> Option<bool> {
        match self {
            Self::SkipEffectHandler
            | Self::RaiseEffect
            | Self::EnterEffectHandler
            | Self::ExitEffectHandler
            | Self::ResumeEffect => Some(true),
            Self::NewUtxo
            | Self::EnterConstructor
            | Self::CallMethod
            | Self::EnterMethod
            | Self::Return => Some(false),
            _ => None,
        }
    }

    /// Required constructor flag, with the same write/read meaning as
    /// [`Self::required_effect_frame`]. None means no fixed value, not necessarily
    /// no read: Return reads this flag to determine its result and phase constraints.
    pub fn required_constructor_frame(&self) -> Option<bool> {
        match self {
            Self::NewUtxo | Self::EnterConstructor => Some(true),
            Self::CallMethod | Self::EnterMethod => Some(false),
            _ => None,
        }
    }

    pub fn pushes_to_call_stack(&self) -> bool {
        matches!(self, Self::NewUtxo | Self::CallMethod | Self::RaiseEffect)
    }

    pub fn pops_from_call_stack_unconditionally(&self) -> bool {
        matches!(
            self,
            Self::ReceiveNewUtxoHandle | Self::ReceiveMethodResult | Self::ResumeEffect
        )
    }

    pub fn peeks_call_stack_top(&self) -> bool {
        matches!(
            self,
            Self::EnterConstructor
                | Self::EnterMethod
                | Self::SkipEffectHandler
                | Self::EnterEffectHandler
                | Self::ExitEffectHandler
        )
    }

    /// Whether this opcode transfers control to `COL_CALL_TARGET`.
    pub fn switches_curr(&self) -> bool {
        self.pushes_to_call_stack()
            || matches!(
                self,
                Self::Return | Self::ResumeEffect | Self::EnterEffectHandler
            )
    }

    pub fn selector(&self) -> usize {
        match self {
            Self::PushEffectHandlerFrame => COL_SEL_PUSH_EFFECT_HANDLER_FRAME,
            Self::AddEffectHandler => COL_SEL_ADD_EFFECT_HANDLER,
            Self::PopEffectHandlerFrame => COL_SEL_POP_EFFECT_HANDLER_FRAME,
            Self::SkipEffectHandler => COL_SEL_SKIP_EFFECT_HANDLER,
            Self::ReadAbi => COL_SEL_READ_ABI,
            Self::FinalizeCoordinator => COL_SEL_FINALIZE_COORDINATOR,
            Self::SetStorage => COL_SEL_SET_STORAGE,
            Self::PreloadMethod => COL_SEL_PRELOAD_METHOD,
            Self::GetStorage => COL_SEL_GET_STORAGE,
            Self::SkipConsumed => COL_SEL_SKIP_CONSUMED,
            Self::FinishTransaction => COL_SEL_FINISH_TRANSACTION,
            Opcode::Padding => COL_SEL_PADDING,
            Opcode::NewUtxo => COL_SEL_NEW_UTXO,
            Opcode::ReceiveNewUtxoHandle => COL_SEL_RECEIVE_NEW_UTXO_HANDLE,
            Opcode::EnterConstructor => COL_SEL_ENTER_CONSTRUCTOR,
            Opcode::YieldBegin => COL_SEL_YIELD_BEGIN,
            Opcode::RegisterMethod => COL_SEL_REGISTER_METHOD,
            Opcode::Return => COL_SEL_RETURN,
            Opcode::CallMethod => COL_SEL_CALL_METHOD,
            Opcode::ReceiveMethodResult => COL_SEL_RECEIVE_METHOD_RESULT,
            Opcode::EnterMethod => COL_SEL_ENTER_METHOD,
            Opcode::RaiseEffect => COL_SEL_RAISE_EFFECT,
            Opcode::EnterEffectHandler => COL_SEL_ENTER_EFFECT_HANDLER,
            Opcode::ExitEffectHandler => COL_SEL_EXIT_EFFECT_HANDLER,
            Opcode::ResumeEffect => COL_SEL_RESUME_EFFECT,
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
                opcode.pops_from_call_stack_unconditionally(),
                opcode.peeks_call_stack_top(),
            ];

            assert!(
                modes.into_iter().filter(|active| *active).count() <= 1,
                "{opcode:?} has overlapping call-stack access modes"
            );
        }
    }
}
