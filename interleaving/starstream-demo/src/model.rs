use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "kebab-case")]
pub enum ContractId {
    Coord,
    UtxoA,
    UtxoB,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DemoContracts {
    pub coord_wat: String,
    pub utxo_a_wat: String,
    pub utxo_b_wat: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DemoInitialStates {
    pub utxo_a: u64,
    pub utxo_b: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompileArtifact {
    pub wat_pretty: Option<String>,
    pub wit: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ContractCompileResult {
    pub contract: ContractId,
    pub ok: bool,
    pub artifact: Option<CompileArtifact>,
    pub error: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompileState {
    pub generation: u64,
    pub results: Vec<ContractCompileResult>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TraceEventView {
    pub process: String,
    pub effect: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExecutionSnapshot {
    pub step_index: usize,
    pub total_steps: usize,
    pub active_process: Option<String>,
    pub highlighted_edge: Option<String>,
    pub per_process_traces: Vec<ProcessTraceView>,
    pub interleaved_trace: Vec<TraceEventView>,
    pub verification_status: StepStatus,
    pub semantics_ok: Option<bool>,
    pub semantics_message: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessTraceView {
    pub process: String,
    pub effects: Vec<String>,
    pub processed: usize,
    pub commitment: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DemoSnapshot {
    pub contracts: DemoContracts,
    pub initial_states: DemoInitialStates,
    pub compile: CompileState,
    pub execution: ExecutionSnapshot,
    pub proof_output: Option<String>,
    pub proof_public_statement: Option<String>,
    pub proof_transaction_debug: Option<String>,
    pub ledger_before_debug: Option<String>,
    pub ledger_after_debug: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EditorFile {
    pub id: String,
    pub label: String,
    pub language: String,
    pub source: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UpdateContractRequest {
    pub source: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UpdateEditorFileRequest {
    pub source: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UpdateInitialStatesRequest {
    pub utxo_a: u64,
    pub utxo_b: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StatusResponse {
    pub ok: bool,
    pub message: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "status", content = "detail", rename_all = "kebab-case")]
pub enum StepStatus {
    Idle,
    Ready,
    Running,
    Completed,
    NotImplemented(String),
    Error(String),
}
