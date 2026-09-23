//! Local browser prototype. All compilation, execution, and proving is native.

#[path = "../../tests/common/mod.rs"]
mod common;
mod editing;
mod model;
#[path = "../../tests/common/proof.rs"]
mod proof_config;
mod proving;

use std::sync::{Arc, Mutex, mpsc};
use std::time::Instant;

use serde::Deserialize;
use serde_json::{Value, json};
use tiny_http::{Header, Method, Request, Response, Server};
use wasmtime::{bail, ensure};

use model::{ProofStatus, State};
use proving::{Session, Shared, phase};

const ADDRESS: &str = "127.0.0.1:4317";

#[derive(Deserialize)]
#[serde(tag = "action", rename_all = "snake_case", deny_unknown_fields)]
enum Command {
    Run {
        example: String,
        source: Option<String>,
    },
    Rerun {
        run_id: u64,
        source: String,
    },
    SetTraceLock {
        run_id: u64,
        target: usize,
        locked: bool,
    },
    CheckTrace {
        run_id: u64,
        target: usize,
    },
    EditRow {
        run_id: u64,
        target: usize,
        row: usize,
        raw: String,
    },
    CheckTransaction {
        run_id: u64,
    },
    Interleave {
        run_id: u64,
    },
    UpdateStatement {
        run_id: u64,
    },
    Prove {
        run_id: u64,
        target: usize,
    },
    ProveAll {
        run_id: u64,
    },
    Verify {
        run_id: u64,
    },
}

impl Command {
    fn validate(&self, state: &State) -> wasmtime::Result<()> {
        ensure!(!state.busy, "A job is already running");
        let run_id = match self {
            Self::Run { example, source } => {
                ensure!(model::source(example).is_some(), "Unknown example");
                if let Some(source) = source {
                    ensure!(
                        !source.trim().is_empty() && source.len() <= 32_768,
                        "Source must be 1–32768 bytes"
                    );
                }
                return Ok(());
            }
            Self::Rerun { run_id, source } => {
                ensure!(
                    !source.trim().is_empty() && source.len() <= 32_768,
                    "Source must be 1–32768 bytes"
                );
                *run_id
            }
            Self::SetTraceLock { run_id, target, .. }
            | Self::CheckTrace { run_id, target }
            | Self::EditRow { run_id, target, .. } => {
                ensure!(*target + 1 < state.proofs.len(), "Unknown Wasm trace");
                *run_id
            }
            Self::CheckTransaction { run_id }
            | Self::Interleave { run_id }
            | Self::UpdateStatement { run_id } => *run_id,
            Self::Prove { run_id, target } => {
                ensure!(*target < state.proofs.len(), "Unknown trace");
                if *target + 1 == state.proofs.len() {
                    ensure!(
                        state
                            .execution
                            .as_ref()
                            .is_some_and(|view| view["interleaving_ready"].as_bool() == Some(true)),
                        "Build the interleaving first"
                    );
                    ensure!(
                        state.proofs[..*target]
                            .iter()
                            .all(|p| p.status == "Verified"),
                        "Prove every Wasm trace first"
                    );
                }
                *run_id
            }
            Self::ProveAll { run_id } => {
                ensure!(
                    state
                        .execution
                        .as_ref()
                        .is_some_and(|view| view["interleaving_ready"].as_bool() == Some(true)),
                    "Build the interleaving first"
                );
                *run_id
            }
            Self::Verify { run_id } => {
                ensure!(
                    !state.proofs.is_empty() && state.proofs.iter().all(|p| p.status == "Verified"),
                    "Prove every trace first"
                );
                *run_id
            }
        };
        ensure!(
            state.execution.is_some() && state.run_id == run_id,
            "This execution is stale; refresh the page"
        );
        Ok(())
    }
}

fn reset_proof(state: &mut State, index: usize) {
    let label = state.proofs[index].label.clone();
    state.proofs[index] = ProofStatus {
        label,
        status: "Ready".into(),
        ..ProofStatus::default()
    };
}

fn reset_transaction_proof(state: &mut State) {
    reset_proof(state, state.proofs.len() - 1);
    state.transaction_check = None;
    state.verification = None;
}

fn execute(
    command: Command,
    session: &mut Session,
    state: &Shared,
    runtime: &tokio::runtime::Runtime,
) -> wasmtime::Result<()> {
    match command {
        Command::Run { example, source } => {
            *session = Session::default();
            {
                let mut state = state.lock().unwrap();
                state.run_id += 1;
                state.revision += 1;
                state.execution = None;
                state.proofs.clear();
                state.trace_checks.clear();
                state.transaction_check = None;
                state.verification = None;
            }
            phase(state, None, "Compiling and tracing");
            let started = Instant::now();
            let source = source.unwrap_or_else(|| model::source(&example).unwrap());
            let execution =
                runtime.block_on(common::trace_coordination_script(&source, "example"))?;
            let mut view = model::execution_view(
                &execution,
                &example,
                started.elapsed().as_secs_f64() * 1000.0,
            )?;
            view["source"] = json!(source);
            let mut state = state.lock().unwrap();
            state.proofs = (0..=execution.instances.len())
                .map(|index| ProofStatus {
                    label: if index == execution.instances.len() {
                        "Interleaving".into()
                    } else {
                        model::label(index)
                    },
                    status: "Ready".into(),
                    ..ProofStatus::default()
                })
                .collect();
            state.trace_checks = vec![None; execution.instances.len()];
            *session = Session::new(execution, view);
            state.execution = Some(session.view()?);
        }
        Command::Rerun { source, .. } => {
            phase(state, None, "Compiling and tracing");
            let example = session.captures[session.latest].view["example"]
                .as_str()
                .unwrap()
                .to_owned();
            let started = Instant::now();
            let candidate =
                runtime.block_on(common::trace_coordination_script(&source, "example"))?;
            ensure!(
                candidate.instances.len() == session.selected.len(),
                "Source changed the number of Wasm instances"
            );
            let mut view = model::execution_view(
                &candidate,
                &example,
                started.elapsed().as_secs_f64() * 1000.0,
            )?;
            view["source"] = json!(source);
            let replaced = session.replace_unlocked(candidate, view);
            let mut state = state.lock().unwrap();
            for &index in &replaced {
                session.roots.remove(&common::coroutine_id(index));
                reset_proof(&mut state, index);
                state.trace_checks[index] = None;
            }
            if !replaced.is_empty() {
                session.invalidate_interleaving_proof();
                reset_transaction_proof(&mut state);
            }
            state.execution = Some(session.view()?);
            state.revision += 1;
        }
        Command::SetTraceLock { target, locked, .. } => {
            if session.locked[target] == locked {
                return Ok(());
            }
            session.locked[target] = locked;
            let mut state = state.lock().unwrap();
            state.execution = Some(session.view()?);
            state.revision += 1;
        }
        Command::CheckTrace { target, .. } => {
            phase(state, None, "Checking Wasm trace");
            state.lock().unwrap().active = Some(target);
            let result = session.check_trace(target)?;
            state.lock().unwrap().trace_checks[target] = Some(result);
        }
        Command::EditRow {
            target, row, raw, ..
        } => {
            ensure!(raw.len() <= 16_384, "Row JSON is too large");
            let capture = session.selected[target];
            let edited =
                editing::apply(&mut session.captures[capture].execution, target, row, &raw)?;
            session.captures[capture].view["traces"][target]["normalized"][row] = edited;
            let current_root = session.source_execution(target)?.instances[target]
                .trace
                .last()
                .unwrap()
                .state_after
                .comm_chain;
            session.captures[capture].view["traces"][target]["root"] =
                json!(model::root(current_root));
            session.roots.remove(&common::coroutine_id(target));
            session.invalidate_interleaving_proof();
            let mut state = state.lock().unwrap();
            state.trace_checks[target] = None;
            reset_proof(&mut state, target);
            reset_transaction_proof(&mut state);
            state.execution = Some(session.view()?);
            state.revision += 1;
        }
        Command::CheckTransaction { .. } => {
            phase(state, None, "Checking transaction");
            let result = session.check_transaction()?;
            state.lock().unwrap().transaction_check = Some(result);
        }
        Command::Interleave { .. } => {
            phase(state, None, "Interleaving host events");
            session.invalidate_interleaving_proof();
            let result = session.build_interleaving();
            let mut state = state.lock().unwrap();
            reset_transaction_proof(&mut state);
            state.execution = Some(session.view()?);
            state.revision += 1;
            result?;
        }
        Command::UpdateStatement { .. } => {
            if session.statement_current {
                return Ok(());
            }
            session.update_statement();
            session.invalidate_interleaving_proof();
            let mut state = state.lock().unwrap();
            reset_transaction_proof(&mut state);
            state.execution = Some(session.view()?);
            state.revision += 1;
        }
        Command::Prove { target, .. } => session.prove(target, state)?,
        Command::ProveAll { .. } => {
            let count = session.selected.len();
            for index in 0..=count {
                session.prove(index, state)?;
            }
        }
        Command::Verify { .. } => {
            state.lock().unwrap().verification = None;
            let result = session.verify(state)?;
            state.lock().unwrap().verification = Some(result);
        }
    }
    Ok(())
}

fn worker(receiver: mpsc::Receiver<Command>, state: Shared) {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .build()
        .expect("native runtime");
    let mut session = Session::default();
    for command in receiver {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            execute(command, &mut session, &state, &runtime)
        }));
        let result = match result {
            Ok(result) => result,
            Err(_) => {
                session = Session::default();
                let mut state = state.lock().unwrap();
                state.execution = None;
                state.proofs.clear();
                state.trace_checks.clear();
                state.transaction_check = None;
                state.active = None;
                state.verification = None;
                Err(wasmtime::format_err!(
                    "Native worker panicked; run the script again (see terminal)"
                ))
            }
        };
        let mut state = state.lock().unwrap();
        if let Err(error) = result {
            eprintln!("Demo job failed: {error:#}");
            if let Some(index) = state.active {
                state.proofs[index].status = "Failed".into();
            }
            state.error = Some(format!("{error:#}"));
        }
        state.busy = false;
        state.active = None;
        state.phase = "Idle".into();
    }
}

fn respond(request: Request, status: u16, content_type: &str, body: impl Into<Vec<u8>>) {
    let response = Response::from_data(body.into())
        .with_status_code(status)
        .with_header(Header::from_bytes("Content-Type", content_type).unwrap())
        .with_header(Header::from_bytes("Cache-Control", "no-store").unwrap())
        .with_header(Header::from_bytes("X-Content-Type-Options", "nosniff").unwrap());
    if let Err(error) = request.respond(response) {
        eprintln!("HTTP response: {error}");
    }
}

fn json_response(request: Request, status: u16, value: Value) {
    respond(
        request,
        status,
        "application/json",
        serde_json::to_vec(&value).unwrap(),
    );
}

fn submit(
    request: &mut Request,
    sender: &mpsc::Sender<Command>,
    state: &Shared,
) -> wasmtime::Result<()> {
    // Custom header prevents a different website from submitting local jobs.
    ensure!(
        request
            .headers()
            .iter()
            .any(|h| h.field.equiv("X-Starstream-Demo") && h.value.as_str() == "1"),
        "Missing demo request header"
    );
    if let Some(origin) = request.headers().iter().find(|h| h.field.equiv("Origin")) {
        ensure!(
            origin.value.as_str() == format!("http://{ADDRESS}"),
            "Unexpected origin"
        );
    }
    ensure!(
        request.body_length().is_some_and(|len| len <= 65_536),
        "Request body is too large or has no length"
    );
    let command: Command = serde_json::from_reader(request.as_reader())?;
    let mut state = state.lock().unwrap();
    command.validate(&state)?;
    state.busy = true;
    state.error = None;
    state.phase = "Queued".into();
    if sender.send(command).is_err() {
        state.busy = false;
        bail!("The native worker stopped");
    }
    Ok(())
}

fn main() -> wasmtime::Result<()> {
    let state = Arc::new(Mutex::new(State::default()));
    let (sender, receiver) = mpsc::channel();
    let worker_state = Arc::clone(&state);
    std::thread::spawn(move || worker(receiver, worker_state));
    let server = Server::http(ADDRESS).map_err(|error| wasmtime::format_err!("{error}"))?;
    println!("Starstream demo: http://{ADDRESS}");
    for mut request in server.incoming_requests() {
        match (request.method(), request.url()) {
            (&Method::Get, "/") => respond(
                request,
                200,
                "text/html; charset=utf-8",
                include_str!("static/index.html"),
            ),
            (&Method::Get, "/app.js") => respond(
                request,
                200,
                "text/javascript; charset=utf-8",
                include_str!("static/app.js"),
            ),
            (&Method::Get, "/style.css") => respond(
                request,
                200,
                "text/css; charset=utf-8",
                include_str!("static/style.css"),
            ),
            (&Method::Get, "/starstream.svg") => respond(
                request,
                200,
                "image/svg+xml",
                include_str!("../../../docs/starstream.svg"),
            ),
            (&Method::Get, "/api/examples") => json_response(
                request,
                200,
                json!({"examples": [
                    {"id": "small", "label": "Simple counter", "sources": model::sources("small")},
                    {"id": "dynamic", "label": "Oracle and consumer", "sources": model::sources("dynamic")}
                ]}),
            ),
            (&Method::Get, "/api/state") => {
                let value = serde_json::to_value(&*state.lock().unwrap())?;
                json_response(request, 200, value);
            }
            (&Method::Get, "/api/execution") => {
                let state = state.lock().unwrap();
                json_response(
                    request,
                    200,
                    json!({"run_id": state.run_id, "revision": state.revision, "execution": state.execution}),
                );
            }
            (&Method::Post, "/api/command") => match submit(&mut request, &sender, &state) {
                Ok(()) => json_response(request, 202, json!({"accepted": true})),
                Err(error) => json_response(request, 409, json!({"error": error.to_string()})),
            },
            _ => json_response(request, 404, json!({"error": "Not found"})),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(target_vendor = "apple")]
    #[tokio::test]
    #[ignore = "manual Metal timing"]
    async fn dynamic_interleaving_metal_timing() -> wasmtime::Result<()> {
        use starstream_interleaving_prover::proving::TransactionProofContext;

        let started = Instant::now();
        let execution =
            common::trace_coordination_script(&model::source("dynamic").unwrap(), "example")
                .await?;
        eprintln!("capture: {:.2}s", started.elapsed().as_secs_f64());
        eprintln!(
            "Wasm rows: {:?}, transaction steps: {}",
            execution
                .instances
                .iter()
                .map(|instance| instance.trace.len())
                .collect::<Vec<_>>(),
            execution.transaction.trace.0.len()
        );
        let started = Instant::now();
        let context = TransactionProofContext::new(3, common::interleaving_params(), [7; 32])?;
        let mut metal = neo_prover_metal::MetalNifsProver::new()?;
        eprintln!("interleaving prep: {:.2}s", started.elapsed().as_secs_f64());
        let started = Instant::now();
        let proof = context.prove_with_nifs_adapter(
            &mut metal,
            &execution.transaction.trace,
            &execution.transaction.statement,
            &execution.commitments,
        )?;
        eprintln!(
            "interleaving prove: {:.2}s",
            started.elapsed().as_secs_f64()
        );
        let started = Instant::now();
        context.verify_with_opening_backend(
            &proof,
            &execution.transaction.statement,
            &execution.commitments,
            metal.final_witness_opening_backend(),
        )?;
        eprintln!(
            "interleaving verify: {:.2}s",
            started.elapsed().as_secs_f64()
        );
        Ok(())
    }

    #[test]
    fn jobs_reject_stale_runs_and_premature_verification() {
        let mut state = State {
            run_id: 2,
            execution: Some(json!({"interleaving_ready": true})),
            proofs: vec![ProofStatus {
                status: "Ready".into(),
                ..ProofStatus::default()
            }],
            ..State::default()
        };
        assert!(Command::ProveAll { run_id: 1 }.validate(&state).is_err());
        assert!(Command::ProveAll { run_id: 2 }.validate(&state).is_ok());
        assert!(Command::Verify { run_id: 2 }.validate(&state).is_err());
        state.busy = true;
        assert!(
            Command::Run {
                example: "dynamic".into(),
                source: None,
            }
            .validate(&state)
            .is_err()
        );
    }

    #[tokio::test]
    async fn gather_edit_is_bound_to_its_source() -> wasmtime::Result<()> {
        use neo_wasm::ir::WasmHostEventSlotKind;

        let mut execution =
            common::trace_coordination_script(common::STORAGE_COUNTER, "example").await?;
        common::check_wasm_constraints(&execution)?;
        let (instance_index, row_index) = execution
            .instances
            .iter()
            .enumerate()
            .find_map(|(instance_index, instance)| {
                instance
                    .trace
                    .iter()
                    .position(|row| {
                        row.host_event_rom_slot
                            .is_some_and(|slot| slot.kind == WasmHostEventSlotKind::Arg)
                            && row.stack_read0.is_some_and(|read| read.value_lo == 13)
                    })
                    .map(|row_index| (instance_index, row_index))
            })
            .expect("an argument gather row");
        let row = &execution.instances[instance_index].trace[row_index];
        let mut json = model::normalized_row(row, row_index);
        json["gather_value"] = json!("14");
        editing::apply(&mut execution, instance_index, row_index, &json.to_string())?;
        let result = common::check_wasm_constraints(&execution);
        assert!(result.is_err());
        let row = &execution.instances[instance_index].trace[row_index];
        let mut json = model::normalized_row(row, row_index);
        json["stack_read0"]["value_lo"] = json!(14);
        editing::apply(&mut execution, instance_index, row_index, &json.to_string())?;
        let result = common::check_wasm_constraints(&execution);
        assert!(result.is_err());
        Ok(())
    }

    #[tokio::test]
    async fn source_edit_produces_independently_valid_traces_and_new_roots() -> wasmtime::Result<()>
    {
        let original =
            common::trace_coordination_script(common::STORAGE_COUNTER, "example").await?;
        let edited_source = common::STORAGE_COUNTER.replace("counter.add(13)", "counter.add(14)");
        let edited = common::trace_coordination_script(&edited_source, "example").await?;
        assert_eq!(original.instances.len(), edited.instances.len());
        common::check_wasm_instance(&edited, 0)?;
        common::check_wasm_instance(&edited, 1)?;
        assert_ne!(
            original.commitments[&common::coroutine_id(0)],
            edited.commitments[&common::coroutine_id(0)]
        );
        assert_ne!(
            original.commitments[&common::coroutine_id(1)],
            edited.commitments[&common::coroutine_id(1)]
        );
        let mixed = starstream_interleaving_spec::interleaver::interleave_transaction(&[
            edited.instances[0].steps.clone(),
            original.instances[1].steps.clone(),
        ])?;
        let mut roots = original.commitments.clone();
        roots.insert(
            common::coroutine_id(0),
            edited.commitments[&common::coroutine_id(0)],
        );
        assert!(
            starstream_interleaving_prover::verify_transaction_sat(
                &mixed.trace,
                3,
                &original.transaction.statement,
                &roots,
            )
            .is_err(),
            "a changed caller must not match the old callee"
        );
        let mut session = Session::new(original, json!({"outputs": []}));
        session.locked[1] = true;
        assert_eq!(
            session.replace_unlocked(edited, json!({"outputs": []})),
            [0]
        );
        let trace_check = session.check_trace(0)?;
        assert_eq!(trace_check["accepted"], true);
        let transaction_check = session.check_transaction()?;
        assert_eq!(transaction_check["accepted"], false);
        session.selected[1] = session.latest;
        session.build_interleaving()?;
        assert_eq!(session.check_transaction()?["accepted"], false);
        session.update_statement();
        assert_eq!(session.check_transaction()?["accepted"], true);
        let next_source = common::STORAGE_COUNTER.replace("counter.add(13)", "counter.add(15)");
        let next = common::trace_coordination_script(&next_source, "example").await?;
        session.locked = vec![true, false];
        assert_eq!(session.replace_unlocked(next, json!({"outputs": []})), [1]);
        assert_eq!(session.check_trace(0)?["accepted"], true);
        assert_eq!(session.check_trace(1)?["accepted"], true);
        assert_eq!(session.check_transaction()?["accepted"], false);
        session.selected[1] = session.selected[0];
        assert_eq!(session.check_transaction()?["accepted"], true);
        Ok(())
    }

    #[test]
    fn locked_traces_keep_their_capture_across_reruns() -> wasmtime::Result<()> {
        let runtime = tokio::runtime::Builder::new_current_thread().build()?;
        let state = Arc::new(Mutex::new(State::default()));
        let mut session = Session::default();
        let original = model::source("small").unwrap();
        execute(
            Command::Run {
                example: "small".into(),
                source: Some(original.clone()),
            },
            &mut session,
            &state,
            &runtime,
        )?;
        assert_eq!(
            session.view()?["outputs"][0]["fields"]
                .as_array()
                .unwrap()
                .iter()
                .find(|field| field["name"] == "count")
                .unwrap()["value"],
            "56"
        );
        execute(
            Command::SetTraceLock {
                run_id: 1,
                target: 1,
                locked: true,
            },
            &mut session,
            &state,
            &runtime,
        )?;
        execute(
            Command::Interleave { run_id: 1 },
            &mut session,
            &state,
            &runtime,
        )?;
        for value in [14, 15] {
            execute(
                Command::Rerun {
                    run_id: 1,
                    source: original.replace(
                        "CounterUtxo::new(55)",
                        &format!("CounterUtxo::new({value})"),
                    ),
                },
                &mut session,
                &state,
                &runtime,
            )?;
        }
        assert_eq!(session.captures.len(), 2);
        assert_eq!(session.selected[1], 0);
        assert!(!session.statement_current);
        assert_eq!(session.view()?["statement_differs"], true);
        assert_ne!(
            session.view()?["outputs"],
            session.view()?["latest_outputs"]
        );
        assert_eq!(session.check_trace(0)?["accepted"], true);
        assert_eq!(session.check_trace(1)?["accepted"], true);
        assert_eq!(session.view()?["interleaving_stale"], true);
        assert_eq!(session.check_transaction()?["accepted"], false);
        session.build_interleaving()?;
        assert_eq!(session.view()?["interleaving_stale"], false);
        let mismatch = session.check_transaction()?;
        assert_eq!(mismatch["accepted"], false);
        let failure_step = mismatch["failure_step"].as_u64().unwrap() as usize;
        assert!(matches!(
            session.interleaved.as_ref().unwrap().trace.0[failure_step],
            starstream_interleaving_spec::Step::EnterConstructor { .. }
        ));
        assert_eq!(
            session.view()?["traces"][1]["root"],
            session.captures[0].view["traces"][1]["root"]
        );
        Ok(())
    }

    #[tokio::test]
    async fn dynamic_example_captures_all_programs_and_actual_storage() -> wasmtime::Result<()> {
        let execution =
            common::trace_coordination_script(&model::source("dynamic").unwrap(), "example")
                .await?;
        assert_eq!(execution.instances.len(), 3);
        common::check_wasm_constraints(&execution)?;
        starstream_interleaving_prover::verify_transaction_sat(
            &execution.transaction.trace,
            3,
            &execution.transaction.statement,
            &execution.commitments,
        )?;
        let view = model::execution_view(&execution, "dynamic", 0.0)?;
        assert!(
            view["generated"][0]["source"]
                .as_str()
                .unwrap()
                .contains("(module")
        );
        assert!(
            view["generated"][1]["source"]
                .as_str()
                .unwrap()
                .contains("(component")
        );
        assert_eq!(
            view["outputs"][0]["fields"]
                .as_array()
                .unwrap()
                .iter()
                .find(|field| field["name"] == "reading")
                .unwrap()["value"],
            "3"
        );
        assert_eq!(
            view["outputs"][1]["fields"]
                .as_array()
                .unwrap()
                .iter()
                .find(|field| field["name"] == "count")
                .unwrap()["value"],
            "13"
        );
        assert_eq!(
            execution.instances[0]
                .steps
                .0
                .iter()
                .filter(|step| matches!(
                    step,
                    starstream_interleaving_spec::Step::CallMethod { .. }
                ))
                .count(),
            4
        );
        let mut session = Session::new(execution, view);
        session.build_interleaving()?;
        let display = session.view()?;
        let rows = display["interleaving"].as_array().unwrap();
        assert!(
            rows.iter()
                .any(|row| row["kind"] == "call_method" && row["source"] == "Coord")
        );
        assert!(
            rows.iter()
                .any(|row| row["kind"] == "enter_method" && row["source"] == "UTXO 0")
        );
        assert!(
            rows.iter()
                .any(|row| row["kind"] == "enter_method" && row["source"] == "UTXO 1")
        );
        assert!(
            rows.iter()
                .any(|row| row["kind"] == "finish_transaction" && row["source"] == "Transaction")
        );
        Ok(())
    }
}
