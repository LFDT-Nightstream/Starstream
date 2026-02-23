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
    HandlerStackArenaProcess = 9,
    HandlerStackArenaNextPtr = 10,
    HandlerStackHeads = 11,
    TraceCommitments = 12,
    ExpectedResumer = 13,
    OnYield = 14,
    YieldTo = 15,
    InitCaller = 16,
}

impl RamMemoryTag {
    pub const fn mem_index(self) -> usize {
        self as usize
    }
}
