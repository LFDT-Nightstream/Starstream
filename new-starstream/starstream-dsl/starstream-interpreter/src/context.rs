use std::collections::BTreeMap;

use starstream_types::StarstreamProgram;

#[derive(Debug, Clone)]
pub enum Value {
    // Primitive values
    Number(i64),
    Boolean(bool),
    // Linear handle types
    Utxo(u64),
}

/// Instruction context: manages which instructions are left to process
#[derive(Debug, Clone)]
pub struct InstructionContext {
    pub program: StarstreamProgram,
    // no need to explicitly track the program counter
    // it's implicit from the Rust program counter
}

#[derive(Debug, Clone, Default)]
pub struct ScopeStack {
    pub variables: BTreeMap<String, Value>,
}

#[derive(Debug, Clone, Default)]
pub struct LocalMemoryContext {
    pub scope_stack: Vec<ScopeStack>,
}

/// Persistent memory context: shared variables that are globally referable
#[derive(Debug, Clone, Default)]
pub struct PersistentMemoryContext {
    pub global_variables: BTreeMap<String, i64>,
    pub heap: BTreeMap<usize, i64>,
    pub next_heap_address: usize,
}

#[derive(Debug, Clone)]
pub enum Type {
    Struct(Vec<(String, Type)>),
    Enum(Vec<(String, Vec<(String, Type)>)>),
}

/// Type context: manages type definitions and their identities
#[derive(Debug, Clone, Default)]
pub struct TypeContext {
    // Type definition and hash identity
    pub type_definitions: BTreeMap<String, (Type, u64)>,
}

/// Resource context: manages references to externally-managed resources
#[derive(Debug, Clone, Default)]
pub struct ResourceContext {
    pub tokens: Vec<i64>,
    // UTXOs are not ambient context.
}


/// Execution environment containing all contexts as defined in the language specification
#[derive(Debug, Clone)]
pub struct ExecutionEnvironment {
    pub instruction_context: InstructionContext,
    pub local_memory_context: LocalMemoryContext,
    pub persistent_memory_context: PersistentMemoryContext,
    pub type_context: TypeContext,
    pub resource_context: ResourceContext,
}

impl ExecutionEnvironment {
    pub fn new(program: StarstreamProgram) -> Self {
        Self {
            instruction_context: InstructionContext { program },
            local_memory_context: LocalMemoryContext::default(),
            persistent_memory_context: PersistentMemoryContext::default(),
            type_context: TypeContext::default(),
            resource_context: ResourceContext::default(),
        }
    }
}

