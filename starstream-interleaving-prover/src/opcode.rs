use crate::ccs::layout::{
    COL_SEL_CALL_METHOD, COL_SEL_ENTER_CONSTRUCTOR, COL_SEL_ENTER_METHOD, COL_SEL_NEW_UTXO,
    COL_SEL_REGISTER_METHOD, COL_SEL_RETURN, COL_SEL_YIELD_BEGIN,
};

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
}

impl From<&starstream_interleaving_spec::Step> for Opcode {
    fn from(value: &starstream_interleaving_spec::Step) -> Self {
        match value {
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
    pub fn all() -> Vec<Self> {
        vec![
            Opcode::NewUtxo,
            Opcode::EnterConstructor,
            Opcode::YieldBegin,
            Opcode::RegisterMethod,
            Opcode::Return,
            Opcode::CallMethod,
            Opcode::EnterMethod,
        ]
    }

    pub fn pushes_to_call_stack(&self) -> bool {
        match self {
            Opcode::NewUtxo => true,
            Opcode::EnterConstructor => false,
            Opcode::YieldBegin => false,
            Opcode::RegisterMethod => false,
            Opcode::Return => false,
            Opcode::CallMethod => true,
            Opcode::EnterMethod => false,
        }
    }

    pub fn pops_from_call_stack(&self) -> bool {
        match self {
            Opcode::NewUtxo => false,
            Opcode::EnterConstructor => false,
            Opcode::YieldBegin => false,
            Opcode::RegisterMethod => false,
            Opcode::Return => true,
            Opcode::CallMethod => false,
            Opcode::EnterMethod => false,
        }
    }

    pub fn peeks_call_stack_top(&self) -> bool {
        match self {
            Opcode::NewUtxo => false,
            Opcode::EnterConstructor => true,
            Opcode::YieldBegin => false,
            Opcode::RegisterMethod => false,
            Opcode::Return => false,
            Opcode::CallMethod => false,
            Opcode::EnterMethod => true,
        }
    }

    pub fn selector(&self) -> usize {
        match self {
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
}
