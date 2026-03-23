#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum RomMemoryTag {
    ProcessTable = 0,
    MustBurn = 1,
    IsUtxo = 2,
    Interfaces = 3,
    IsToken = 4,
}

impl RomMemoryTag {
    pub const fn lut_index(self) -> usize {
        self as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum RamMemoryTag {
    ExpectedInput = 0,
    Activation = 1,
    Initialized = 2,
    Finalized = 3,
    DidBurn = 4,
    Ownership = 5,
    Init = 6,
    RefArena = 7,
    RefSizes = 8,
    RefBases = 9,
    HandlerStackArenaProcess = 10,
    HandlerStackArenaNextPtr = 11,
    HandlerStackHeads = 12,
    TraceCommitments = 13,
    ExpectedResumer = 14,
    OnYield = 15,
    YieldTo = 16,
    InitCaller = 17,
}

impl RamMemoryTag {
    pub const fn mem_index(self) -> usize {
        self as usize
    }
}
