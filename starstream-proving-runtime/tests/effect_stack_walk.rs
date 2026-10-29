mod common;

use common::trace_coordination_script;
use starstream_interleaving_prover::verify_transaction_sat;
use starstream_interleaving_spec::Step;
use starstream_proving_runtime::effect_hash_from_names;

// The raiser's own `try` registers only `Other`, so `Ping` skips it. The
// coordinator's `Ping` handler raises `Nested`, which only the enclosing `try`
// handles: a sibling clause of the selected scope would not be visible.
const OUTER_HANDLER: &str = r#"
abi Effects {
    effect Ping();
    effect Nested();
    effect Other();
    fn ping();
}
utxo Raiser {
    main fn new() { yield(Effects); }
    impl Effects {
        fn ping() {
            try {
                raise Ping();
            } with Other() {
            }
        }
    }
}
script fn main() {
    let raiser = Raiser::new();
    try {
        try {
            raiser.ping();
        } with Ping() {
            raise Nested();
        }
    } with Nested() {
    }
}
"#;

fn effect(name: &str) -> starstream_interleaving_spec::MethodHash {
    effect_hash_from_names("effects", name)
}

/// Each skip and handler entry, in transaction order. A skip searches for the
/// pending effect, so it has none of its own.
fn search_steps(
    steps: &[Step],
) -> Vec<(
    &'static str,
    Option<starstream_interleaving_spec::MethodHash>,
)> {
    steps
        .iter()
        .filter_map(|step| match step {
            Step::SkipEffectHandler => Some(("skip", None)),
            Step::EnterEffectHandler { effect, .. } => Some(("enter", Some(*effect))),
            _ => None,
        })
        .collect()
}

#[tokio::test]
async fn nested_effect_reaches_the_enclosing_scope_past_the_raisers_own() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(OUTER_HANDLER, "main").await?;
    common::check_wasm_constraints(&execution)?;
    verify_transaction_sat(
        &execution.transaction.trace,
        3,
        &execution.transaction.statement,
        &execution.commitments,
    )
    .map_err(|error| wasmtime::format_err!("circuit rejected scope walk: {error}"))?;
    assert_eq!(
        search_steps(&execution.transaction.execution.0),
        [
            ("skip", None),
            ("enter", Some(effect("ping"))),
            ("enter", Some(effect("nested"))),
        ]
    );
    // Skips are host steps: no instance's own trace records them.
    assert!(
        execution
            .semantic_traces()
            .iter()
            .flat_map(|trace| &trace.0)
            .all(|step| !matches!(step, Step::SkipEffectHandler))
    );
    // A host cannot omit the skip and enter the coordinator's scope directly.
    let mut skipped = execution.transaction.trace.clone();
    let skip = skipped
        .0
        .iter()
        .position(|step| matches!(step, Step::SkipEffectHandler))
        .unwrap();
    skipped.0.remove(skip);
    assert!(
        verify_transaction_sat(
            &skipped,
            3,
            &execution.transaction.statement,
            &execution.commitments
        )
        .is_err()
    );
    Ok(())
}

#[tokio::test]
async fn outer_handler_can_construct_a_utxo_that_raises_through_it() -> wasmtime::Result<()> {
    // The handler constructs a UTXO whose constructor raises `Nested`, so the
    // constructor's search starts from the handler's visible chain.
    let source = OUTER_HANDLER.replace(
        "raise Nested();\n        }",
        "let nested = NestedRaiser::new();\n        }",
    ) + r#"
        utxo NestedRaiser {
            main fn new() { raise Nested(); }
            impl Effects { fn ping() {} }
        }
        "#;
    let execution = trace_coordination_script(&source, "main").await?;
    common::check_wasm_constraints(&execution)?;
    verify_transaction_sat(
        &execution.transaction.trace,
        3,
        &execution.transaction.statement,
        &execution.commitments,
    )
    .map_err(|error| wasmtime::format_err!("circuit rejected nested invocation: {error}"))?;
    assert_eq!(execution.semantic_traces().len(), 3);
    assert_eq!(
        search_steps(&execution.transaction.execution.0),
        [
            ("skip", None),
            ("enter", Some(effect("ping"))),
            ("enter", Some(effect("nested"))),
        ]
    );
    Ok(())
}

#[tokio::test]
#[ignore = "requires Quint; run with --include-ignored"]
async fn traced_scope_walks_satisfy_the_quint_specification() -> wasmtime::Result<()> {
    let verifier = starstream_interleaving_spec::QuintVerifier::new()
        .map_err(|error| wasmtime::format_err!("Quint is unavailable: {error}"))?;
    let constructed = OUTER_HANDLER.replace(
        "raise Nested();\n        }",
        "let nested = NestedRaiser::new();\n        }",
    ) + "utxo NestedRaiser { main fn new() { raise Nested(); } impl Effects { fn ping() {} } }";
    for source in [OUTER_HANDLER.to_owned(), constructed] {
        let transaction = trace_coordination_script(&source, "main")
            .await?
            .transaction;
        verifier
            .verify(&transaction.execution)
            .map_err(|error| wasmtime::format_err!("Quint rejected the execution: {error}"))?;
    }
    Ok(())
}
