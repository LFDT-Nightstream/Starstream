use std::fs;
use std::path::Path;
use std::process::Command;

use serde_json::Value;
use starstream_interleaving_spec::{
    ExecutionEvent, ExecutionTrace, MethodHash, ResourceHandle, StarstreamValue, interleave_traces,
};

const GENERATED_TRACES: usize = 32;

#[test]
fn quint_generated_traces_agree_with_the_interleaver() {
    let output_dir = tempfile::tempdir().expect("create temporary ITF output directory");
    let output_pattern = output_dir.path().join("trace_{seq}.itf.json");
    let spec = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("spec")
        .join("starstream.qnt");
    let output = Command::new("quint")
        .arg("run")
        .arg(spec)
        .arg("--main=starstream_sim")
        .arg("--max-samples=64")
        .arg(format!("--n-traces={GENERATED_TRACES}"))
        .arg("--max-steps=30")
        .arg("--seed=0x51eed")
        .arg("--n-threads=1")
        .arg("--backend=rust")
        .arg("--mbt")
        .arg("--verbosity=0")
        .arg(format!("--out-itf={}", output_pattern.display()))
        .output()
        .expect("invoke the repository-pinned Quint executable");
    assert!(
        output.status.success(),
        "Quint simulation failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let mut paths = fs::read_dir(output_dir.path())
        .expect("read generated ITF traces")
        .map(|entry| entry.expect("read generated ITF entry").path())
        .collect::<Vec<_>>();
    paths.sort();
    assert_eq!(
        paths.len(),
        GENERATED_TRACES,
        "Quint should emit every requested trace"
    );

    let mut complete_traces = 0;
    let mut incomplete_traces = 0;
    let mut saw_method_call = false;
    let mut saw_coroutine_return = false;

    for path in paths {
        let source = fs::read_to_string(&path).expect("read generated ITF trace");
        let itf: Value = serde_json::from_str(&source).expect("parse generated ITF trace");
        let decoded = decode_trace(&itf);

        if !decoded.complete {
            incomplete_traces += 1;
            assert!(
                interleave_traces(&decoded.process_local).is_err(),
                "incomplete Quint trace {} produced a complete merge:\n\
                 global trace: {:?}\nprocess-local traces: {:?}",
                path.display(),
                decoded.global,
                decoded.process_local,
            );
            continue;
        }

        complete_traces += 1;
        saw_method_call |= decoded
            .global
            .iter()
            .any(|event| matches!(event, ExecutionEvent::CallMethod { .. }));
        saw_coroutine_return |= decoded
            .global
            .iter()
            .any(|event| matches!(event, ExecutionEvent::CoroutineReturn));

        let merged = interleave_traces(&decoded.process_local).unwrap_or_else(|error| {
            panic!(
                "failed to interleave Quint trace {}: {error}\nglobal trace: {:?}\n\
                 process-local traces: {:?}",
                path.display(),
                decoded.global,
                decoded.process_local,
            )
        });
        assert_eq!(
            merged,
            decoded.global,
            "split/interleave changed Quint trace {}",
            path.display()
        );
    }

    assert!(
        complete_traces >= 8,
        "expected several complete traces, observed {complete_traces}"
    );
    assert!(
        incomplete_traces > 0,
        "expected the seeded simulation to exercise at least one incomplete trace"
    );
    assert!(
        saw_method_call,
        "generated traces should cover method-call scheduling"
    );
    assert!(
        saw_coroutine_return,
        "generated traces should cover post-yield coroutine completion"
    );
}

struct DecodedTrace {
    global: ExecutionTrace,
    process_local: Vec<ExecutionTrace>,
    complete: bool,
}

fn decode_trace(itf: &Value) -> DecodedTrace {
    let states = itf["states"]
        .as_array()
        .expect("ITF trace must contain a states array");
    let final_state = states.last().expect("ITF trace must contain an init state");
    let process_count = usize::try_from(bigint(&final_state["state"]["next_utxo_id"])).unwrap() + 1;
    let mut process_events = vec![Vec::new(); process_count];
    let mut global_events = Vec::with_capacity(states.len());
    global_events.push(ExecutionEvent::Init);

    for index in 1..states.len() {
        // ITF state `index` is the result of the action recorded on that state.
        // The event belongs to the coroutine that owned control beforehand.
        // In particular, NewUtxo and ReturnControl both change `curr`.
        let actor = process_index(&states[index - 1]["state"]["curr"]);
        let event = decode_event(&states[index]);
        process_events[actor].push(event.clone());
        global_events.push(event);
    }

    DecodedTrace {
        global: ExecutionTrace::new(global_events),
        process_local: process_events
            .into_iter()
            .map(ExecutionTrace::new)
            .collect(),
        complete: action_taken(final_state) == "coord_return",
    }
}

fn decode_event(state: &Value) -> ExecutionEvent {
    let picks = &state["mbt::nondetPicks"];
    match action_taken(state) {
        "simulate_new_utxo" => ExecutionEvent::NewUtxo {
            arguments: starstream_value(some_pick(picks, "arguments")),
            resource: ResourceHandle(u32::try_from(bigint(some_pick(picks, "resource"))).unwrap()),
        },
        "abis_clear" => ExecutionEvent::ClearAbi,
        "simulate_advertise_method" => ExecutionEvent::AdvertiseMethod {
            method: method_hash(some_pick(picks, "method")),
        },
        "return_control" => ExecutionEvent::ReturnControl,
        "simulate_call_method" => ExecutionEvent::CallMethod {
            resource: ResourceHandle(
                u32::try_from(bigint(&some_pick(picks, "key")["handle"])).unwrap(),
            ),
            method: method_hash(some_pick(picks, "method")),
            arguments: starstream_value(some_pick(picks, "arguments")),
        },
        "coroutine_return" => ExecutionEvent::CoroutineReturn,
        "coord_return" => ExecutionEvent::CoordReturn,
        action => panic!("unsupported Quint simulation action `{action}`"),
    }
}

fn action_taken(state: &Value) -> &str {
    state["mbt::actionTaken"]
        .as_str()
        .expect("ITF state must name the action that produced it")
}

fn some_pick<'a>(picks: &'a Value, name: &str) -> &'a Value {
    let pick = &picks[name];
    assert_eq!(
        pick["tag"].as_str(),
        Some("Some"),
        "nondeterministic pick `{name}` must be present"
    );
    &pick["value"]
}

fn starstream_value(value: &Value) -> StarstreamValue {
    StarstreamValue(
        value
            .as_array()
            .expect("simulated Starstream value must be a list")
            .iter()
            .map(|limb| u32::try_from(bigint(limb)).unwrap())
            .collect(),
    )
}

fn method_hash(value: &Value) -> MethodHash {
    let method = value
        .as_str()
        .expect("simulated method identifier must be a string");
    let index = method
        .strip_prefix('m')
        .expect("simulated methods must use the mN form")
        .parse()
        .expect("simulated method suffix must be numeric");
    MethodHash([index, 0, 0, 0])
}

fn process_index(value: &Value) -> usize {
    let id = usize::try_from(bigint(&value["value"])).unwrap();
    match value["tag"]
        .as_str()
        .expect("coroutine ID must have a variant tag")
    {
        "Coord" => {
            assert_eq!(id, 0, "the current model has one entrypoint coordinator");
            0
        }
        "Utxo" => id + 1,
        tag => panic!("unknown coroutine ID variant `{tag}`"),
    }
}

fn bigint(value: &Value) -> u64 {
    value["#bigint"]
        .as_str()
        .expect("ITF integer must use the #bigint encoding")
        .parse()
        .expect("ITF #bigint must be an unsigned integer")
}
