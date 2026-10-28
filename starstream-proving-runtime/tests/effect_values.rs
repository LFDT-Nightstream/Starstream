mod common;

use common::trace_coordination_script;
use starstream_interleaving_prover::verify_transaction_sat;
use starstream_interleaving_spec::{StarstreamValue, Step};
use starstream_proving_runtime::{effect_hash_from_names, flat_value_root};
use wasmparser::ValType;
use wasmtime::component::Val;

#[tokio::test]
async fn outer_handler_receives_arguments_and_returns_a_wide_scalar() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(
        r#"
        abi Effects {
            effect Compute(a: u64, b: u64, c: u64) -> u64;
            fn run();
        }
        utxo Raiser {
            storage { let mut answer: u64; }
            main fn new() { answer = 0; yield(Effects); }
            impl Effects {
                fn run() { answer = disclose(raise Compute(4294967298, 7, 9)); }
            }
        }
        script fn main() {
            let raiser = Raiser::new();
            try { raiser.run(); } with Compute(a, b, c) { a }
        }
    "#,
        "main",
    )
    .await?;
    common::check_wasm_constraints(&execution)?;
    verify_transaction_sat(
        &execution.transaction.trace,
        3,
        &execution.transaction.statement,
        &execution.commitments,
    )
    .map_err(|e| wasmtime::format_err!("effect values rejected: {e}"))?;
    let effect = effect_hash_from_names("effects", "compute");
    let arguments = flat_value_root(&[ValType::I64; 3], &[4294967298, 7, 9]);
    assert!(
        execution
            .transaction
            .execution
            .0
            .contains(&Step::RaiseEffect {
                effect,
                arguments: arguments.clone()
            })
    );
    assert_eq!(execution.transaction.execution.0.iter().filter(|step| matches!(step,
        Step::EnterEffectHandler { effect: id, arguments: args, .. } if *id == effect && args == &arguments)).count(), 1);
    let result = flat_value_root(&[ValType::I64], &[4294967298]);
    assert!(
        execution
            .transaction
            .execution
            .0
            .contains(&Step::ResumeEffect {
                result: result.clone().into()
            })
    );
    assert!(
        execution
            .storage
            .iter()
            .flatten()
            .any(|(name, value)| name == "answer" && *value == Val::S64(4294967298))
    );
    // Changing either argument or resumed value must invalidate the trace.
    for tamper_arguments in [true, false] {
        let mut trace = execution.transaction.trace.clone();
        let step = trace
            .0
            .iter_mut()
            .find(|step| {
                if tamper_arguments {
                    matches!(step, Step::EnterEffectHandler { .. })
                } else {
                    matches!(step, Step::ResumeEffect { .. })
                }
            })
            .unwrap();
        match step {
            Step::EnterEffectHandler { arguments, .. } => *arguments = StarstreamValue::UNIT_VALUE,
            Step::ResumeEffect { result } => result.0 = StarstreamValue::UNIT_VALUE,
            _ => unreachable!(),
        }
        assert!(
            verify_transaction_sat(
                &trace,
                3,
                &execution.transaction.statement,
                &execution.commitments
            )
            .is_err()
        );
    }
    Ok(())
}

#[tokio::test]
async fn handler_results_preserve_scalar_widths_and_signed_bits() -> wasmtime::Result<()> {
    for (ty, literal, flat, bits) in [
        ("bool", "true", ValType::I32, 1),
        ("u8", "255", ValType::I32, 255),
        ("u16", "65535", ValType::I32, 65535),
        ("i32", "-7", ValType::I32, u32::MAX as u64 - 6),
        ("i64", "-7", ValType::I64, u64::MAX - 6),
    ] {
        let source = format!(
            r#"
            abi Effects {{ effect Compute(input: {ty}) -> {ty}; fn run(); }}
            utxo Raiser {{
                main fn new() {{ yield(Effects); }}
                impl Effects {{ fn run() {{ let value = raise Compute({literal}); }} }}
            }}
            script fn main() {{
                let raiser = Raiser::new();
                try {{ raiser.run(); }} with Compute(input) {{ input }}
            }}
        "#
        );
        let execution = trace_coordination_script(&source, "main").await?;
        common::check_wasm_constraints(&execution)?;
        verify_transaction_sat(
            &execution.transaction.trace,
            3,
            &execution.transaction.statement,
            &execution.commitments,
        )
        .map_err(|e| wasmtime::format_err!("{ty} effect rejected: {e}"))?;
        assert!(
            execution
                .transaction
                .execution
                .0
                .contains(&Step::ResumeEffect {
                    result: flat_value_root(&[flat], &[bits]).into(),
                }),
            "incorrect {ty} result"
        );
    }
    Ok(())
}
