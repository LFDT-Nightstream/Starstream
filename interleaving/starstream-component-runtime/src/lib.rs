mod abi;
mod component;
mod executor;
mod state;
mod transaction_session;
mod wasmtime;

pub use abi::{
    FUNCTION_ID_INIT, FUNCTION_ID_STEP, HostImport, HostImportCall,
    TEMPORARY_STARSTREAM_SCALAR_COMPONENT_WIT, TemporaryAbi, host_import_name,
};
pub use component::{
    ComponentHostState, ComponentStore, ScalarComponent, ScalarComponentError,
    WasmtimeComponentStarstreamExecutor, component_runtime,
};
pub use executor::{ExecutorError, HostImportOutcome, ProgramDefinition, StarstreamExecutor};
pub use state::{
    ProcessDefinition, ProcessKind, ProcessSlot, ProgramHash, RefArena, ResourceTable,
    StarstreamState,
};
pub use transaction_session::{SessionInputBinding, TransactionSession, TransactionSessionError};
pub use wasmtime::{
    GuestModule, GuestRuntime, GuestStore, WasmtimeGuest, WasmtimeRuntimeError,
    WasmtimeStarstreamExecutor, default_runtime,
};
