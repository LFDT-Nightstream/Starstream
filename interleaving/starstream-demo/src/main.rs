mod defaults;
mod model;
mod state;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse};
use axum::routing::{get, post, put};
use axum::{Json, Router};
use model::{
    ContractId, DemoSnapshot, EditorFile, StatusResponse, UpdateContractRequest,
    UpdateEditorFileRequest, UpdateInitialStatesRequest,
};
use state::DemoBackendState;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::RwLock;

type SharedState = Arc<RwLock<DemoBackendState>>;
const INDEX_HTML: &str = include_str!("../ui/index.html");
const APP_JS: &str = include_str!("../ui/app.js");
const STYLES_CSS: &str = include_str!("../ui/styles.css");

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let state = Arc::new(RwLock::new(DemoBackendState::new()));
    let app = Router::new()
        .route("/", get(index))
        .route("/app.js", get(app_js))
        .route("/styles.css", get(styles_css))
        .route("/health", get(health))
        .route("/api/snapshot", get(get_snapshot))
        .route("/api/scenario/default", post(load_default_scenario))
        .route("/api/contracts/{contract}", put(update_contract))
        .route("/api/initial-states", put(update_initial_states))
        .route(
            "/api/contracts/{contract}/editor-files",
            get(get_editor_files),
        )
        .route(
            "/api/contracts/{contract}/editor-files/{file_id}",
            put(update_editor_file),
        )
        .route("/api/compile", post(compile_contracts))
        .route("/api/trace", post(trace_execution))
        .route("/api/execution/reset", post(reset_execution))
        .route("/api/execution/step", post(step_execution))
        .route("/api/execution/run", post(run_to_completion))
        .route("/api/verify-semantics", post(verify_semantics))
        .route("/api/fold", post(fold_proof))
        .route("/api/apply-transaction", post(apply_transaction))
        .with_state(state);

    let addr = SocketAddr::from(([127, 0, 0, 1], 4317));
    eprintln!("starstream-demo backend listening on http://{addr}");
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;
    Ok(())
}

async fn index() -> Html<&'static str> {
    Html(INDEX_HTML)
}

async fn app_js() -> impl IntoResponse {
    (
        [(
            axum::http::header::CONTENT_TYPE,
            "application/javascript; charset=utf-8",
        )],
        APP_JS,
    )
}

async fn styles_css() -> impl IntoResponse {
    (
        [(axum::http::header::CONTENT_TYPE, "text/css; charset=utf-8")],
        STYLES_CSS,
    )
}

async fn health() -> Json<StatusResponse> {
    Json(StatusResponse {
        ok: true,
        message: "starstream-demo backend is healthy".to_owned(),
    })
}

async fn get_snapshot(State(state): State<SharedState>) -> Json<DemoSnapshot> {
    let guard = state.read().await;
    Json(guard.snapshot())
}

async fn load_default_scenario(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.reset_scenario())
}

async fn update_contract(
    State(state): State<SharedState>,
    Path(contract): Path<ContractId>,
    Json(request): Json<UpdateContractRequest>,
) -> Result<Json<StatusResponse>, (StatusCode, Json<StatusResponse>)> {
    let mut guard = state.write().await;
    Ok(Json(guard.update_contract(contract, request.source)))
}

async fn update_initial_states(
    State(state): State<SharedState>,
    Json(request): Json<UpdateInitialStatesRequest>,
) -> Result<Json<StatusResponse>, (StatusCode, Json<StatusResponse>)> {
    let mut guard = state.write().await;
    Ok(Json(
        guard.update_initial_states(request.utxo_a, request.utxo_b),
    ))
}

async fn get_editor_files(
    State(state): State<SharedState>,
    Path(contract): Path<ContractId>,
) -> Result<Json<Vec<EditorFile>>, (StatusCode, Json<StatusResponse>)> {
    let guard = state.read().await;
    match guard.editor_files(contract) {
        Ok(files) => Ok(Json(files)),
        Err(err) => Err((
            StatusCode::BAD_REQUEST,
            Json(StatusResponse {
                ok: false,
                message: format!("{err:#}"),
            }),
        )),
    }
}

async fn update_editor_file(
    State(state): State<SharedState>,
    Path((contract, file_id)): Path<(ContractId, String)>,
    Json(request): Json<UpdateEditorFileRequest>,
) -> Result<Json<StatusResponse>, (StatusCode, Json<StatusResponse>)> {
    let mut guard = state.write().await;
    match guard.update_editor_file(contract, &file_id, request.source) {
        Ok(status) => Ok(Json(status)),
        Err(err) => Err((
            StatusCode::BAD_REQUEST,
            Json(StatusResponse {
                ok: false,
                message: format!("{err:#}"),
            }),
        )),
    }
}

async fn compile_contracts(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.compile_contracts())
}

async fn trace_execution(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.trace_execution())
}

async fn reset_execution(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.reset_execution())
}

async fn step_execution(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.step_execution())
}

async fn run_to_completion(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.run_to_completion())
}

async fn verify_semantics(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.verify_semantics())
}

async fn fold_proof(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.fold_proof())
}

async fn apply_transaction(State(state): State<SharedState>) -> Json<StatusResponse> {
    let mut guard = state.write().await;
    Json(guard.apply_transaction_to_ledger())
}
