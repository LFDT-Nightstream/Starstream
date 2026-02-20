pub const OPCODE_ARG_COUNT: usize = 7;

#[derive(Copy, Clone, Debug)]
pub enum ArgName {
    Target,
    Val,
    Ret,
    Caller,
    Offset,
    Size,
    ProgramHash0,
    ProgramHash1,
    ProgramHash2,
    ProgramHash3,
    ActivationCaller,
    OwnerId,
    TokenId,
    InterfaceId0,
    InterfaceId1,
    InterfaceId2,
    InterfaceId3,
    PackedRef0,
    PackedRef1,
    PackedRef2,
    PackedRef3,
    PackedRef4,
    PackedRef5,
}

impl ArgName {
    // Maps ABI argument names to positional indices.
    pub const fn idx(self) -> usize {
        match self {
            ArgName::Target | ArgName::OwnerId | ArgName::TokenId => 0,
            ArgName::Val => 1,
            ArgName::Ret => 2,
            ArgName::Caller | ArgName::Offset | ArgName::Size | ArgName::ActivationCaller => 3,
            ArgName::InterfaceId0 => 3,
            ArgName::InterfaceId1 => 4,
            ArgName::InterfaceId2 => 5,
            ArgName::InterfaceId3 => 6,
            ArgName::ProgramHash0 => 3,
            ArgName::ProgramHash1 => 4,
            ArgName::ProgramHash2 => 5,
            ArgName::ProgramHash3 => 6,
            // Packed ref args for RefPush/RefGet/RefWrite.
            ArgName::PackedRef0 => 0,
            ArgName::PackedRef1 => 1,
            ArgName::PackedRef2 => 2,
            ArgName::PackedRef3 => 3,
            ArgName::PackedRef4 => 4,
            ArgName::PackedRef5 => 5,
        }
    }
}
