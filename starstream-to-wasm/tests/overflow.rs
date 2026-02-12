use starstream_compiler::TypecheckOptions;
use wasmtime::{Config, Engine, Linker, Module, Store};

/// Helper to compile a Starstream program
fn compile_program(source: &str) -> Vec<u8> {
    let parse_output = starstream_compiler::parse_program(source);
    let (program, errors) = parse_output.into_output_errors();
    assert!(errors.is_empty(), "Parse errors: {:?}", errors);

    let program = program.expect("No program parsed");
    let success = starstream_compiler::typecheck_program(
        &program,
        TypecheckOptions {
            capture_traces: false,
        },
    )
    .expect("Typecheck failed");

    let (wasm, errors) = starstream_to_wasm::compile(&success.program);
    assert!(errors.is_empty(), "Compile errors: {:?}", errors);

    wasm.expect("No WASM generated")
}

/// Helper to execute WASM and check if it traps
fn execute_wasm(wasm: &[u8], func_name: &str, params: &[i64]) -> Result<Vec<i64>, String> {
    let mut config = Config::new();
    config.wasm_component_model(false);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_binary(&engine, wasm).unwrap();

    let linker = Linker::new(&engine);
    let mut store = Store::new(&engine, ());

    let instance = linker.instantiate(&mut store, &module).unwrap();
    let func = instance
        .get_func(&mut store, func_name)
        .unwrap_or_else(|| panic!("Function {} not found", func_name));

    let mut results = vec![wasmtime::Val::I64(0)];
    let params: Vec<wasmtime::Val> = params.iter().map(|&p| wasmtime::Val::I64(p)).collect();

    match func.call(&mut store, &params, &mut results) {
        Ok(_) => {
            let result: Vec<i64> = results.iter().map(|v| v.i64().unwrap()).collect();
            Ok(result)
        }
        Err(e) => Err(format!("{}", e)),
    }
}

// ============================================================================
// Tests that SHOULD overflow (trap at runtime)
// ============================================================================

#[test]
fn test_overflow_i64_max_plus_one() {
    let source = r#"
script fn overflow_test() -> i64 {
    9223372036854775807 + 1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_i64_max_plus_max() {
    let source = r#"
script fn overflow_test() -> i64 {
    9223372036854775807 + 9223372036854775807
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_i64_min_minus_one() {
    let source = r#"
script fn overflow_test() -> i64 {
    -9223372036854775807 - 1 + -1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_i64_min_plus_min() {
    let source = r#"
script fn overflow_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min + min
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_large_positives() {
    let source = r#"
script fn overflow_test() -> i64 {
    5000000000000000000 + 5000000000000000000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_large_negatives() {
    let source = r#"
script fn overflow_test() -> i64 {
    -5000000000000000000 + -5000000000000000000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-test", &[]);
    assert!(result.is_err(), "Should trap on overflow");
}

#[test]
fn test_overflow_with_parameters() {
    let source = r#"
script fn overflow_test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Test with values that overflow
    let result = execute_wasm(&wasm, "overflow-test", &[i64::MAX, 1]);
    assert!(result.is_err(), "Should trap on overflow with MAX + 1");

    let result = execute_wasm(&wasm, "overflow-test", &[i64::MIN, -1]);
    assert!(result.is_err(), "Should trap on overflow with MIN + -1");
}

// ============================================================================
// Tests that should NOT overflow (return successful results)
// ============================================================================

#[test]
fn test_no_overflow_max_plus_zero() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    9223372036854775807 + 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MAX);
}

#[test]
fn test_no_overflow_min_plus_zero() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    -9223372036854775807 - 1 + 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MIN);
}

#[test]
fn test_no_overflow_max_plus_min() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    9223372036854775807 + (-9223372036854775807 - 1)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(
        result.is_ok(),
        "Should not trap - different signs never overflow"
    );
    assert_eq!(result.unwrap()[0], -1);
}

#[test]
fn test_no_overflow_max_minus_one() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    9223372036854775807 + -1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MAX - 1);
}

#[test]
fn test_no_overflow_min_plus_one() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    -9223372036854775807 - 1 + 1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MIN + 1);
}

#[test]
fn test_no_overflow_boundary_positive() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    4611686018427387903 + 4611686018427387903
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap at boundary");
    assert_eq!(result.unwrap()[0], 9223372036854775806);
}

#[test]
fn test_no_overflow_boundary_negative() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    -4611686018427387904 + -4611686018427387904
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap at boundary");
    assert_eq!(result.unwrap()[0], i64::MIN);
}

#[test]
fn test_no_overflow_mixed_signs() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    5000000000000000000 + -3000000000000000000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap - different signs");
    assert_eq!(result.unwrap()[0], 2000000000000000000);
}

#[test]
fn test_no_overflow_zero_plus_zero() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    0 + 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 0);
}

#[test]
fn test_no_overflow_small_positive() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    100 + 200
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 300);
}

#[test]
fn test_no_overflow_small_negative() {
    let source = r#"
script fn no_overflow_test() -> i64 {
    -100 + -200
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], -300);
}

#[test]
fn test_no_overflow_with_parameters() {
    let source = r#"
script fn no_overflow_test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Test with values that don't overflow
    let result = execute_wasm(&wasm, "no-overflow-test", &[100, 200]);
    assert!(result.is_ok(), "Should not trap with small values");
    assert_eq!(result.unwrap()[0], 300);

    let result = execute_wasm(&wasm, "no-overflow-test", &[i64::MAX, 0]);
    assert!(result.is_ok(), "Should not trap with MAX + 0");
    assert_eq!(result.unwrap()[0], i64::MAX);

    let result = execute_wasm(&wasm, "no-overflow-test", &[i64::MAX, i64::MIN]);
    assert!(result.is_ok(), "Should not trap with opposite signs");
    assert_eq!(result.unwrap()[0], -1);
}

// ============================================================================
// Tests for the overflow detection formula itself
// ============================================================================

#[test]
fn test_overflow_formula_positive_overflow() {
    // Tests the formula: (x ^ y) >= 0 && (sum ^ x) < 0
    // For positive overflow: both positive, sum negative
    let source = r#"
script fn test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Both positive, sum wraps to negative
    let result = execute_wasm(&wasm, "test", &[i64::MAX, 1]);
    assert!(result.is_err(), "Positive overflow not detected");
}

#[test]
fn test_overflow_formula_negative_overflow() {
    // For negative overflow: both negative, sum wraps to positive
    let source = r#"
script fn test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Both negative, sum wraps to positive
    let result = execute_wasm(&wasm, "test", &[i64::MIN, -1]);
    assert!(result.is_err(), "Negative overflow not detected");
}

#[test]
fn test_overflow_formula_different_signs_never_overflow() {
    // When signs differ, (x ^ y) < 0, so overflow check short-circuits
    let source = r#"
script fn test(x: i64, y: i64) -> i64 {
    x + y
}
"#;

    let wasm = compile_program(source);

    // Different signs - should never overflow
    let result = execute_wasm(&wasm, "test", &[i64::MAX, -1]);
    assert!(result.is_ok(), "Different signs should not overflow");

    let result = execute_wasm(&wasm, "test", &[i64::MIN, 1]);
    assert!(result.is_ok(), "Different signs should not overflow");

    let result = execute_wasm(&wasm, "test", &[i64::MAX, i64::MIN]);
    assert!(result.is_ok(), "Different signs should not overflow");
}

// ============================================================================
// Tests for checked SUBTRACTION - overflow cases
// ============================================================================

#[test]
fn test_overflow_sub_i64_min_minus_one() {
    let source = r#"
script fn overflow_sub_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min - 1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-sub-test", &[]);
    assert!(result.is_err(), "Should trap on MIN - 1");
}

#[test]
fn test_overflow_sub_i64_min_minus_positive() {
    let source = r#"
script fn overflow_sub_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min - 1000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-sub-test", &[]);
    assert!(result.is_err(), "Should trap on MIN - positive");
}

#[test]
fn test_overflow_sub_i64_max_minus_negative() {
    let source = r#"
script fn overflow_sub_test() -> i64 {
    9223372036854775807 - (-1)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-sub-test", &[]);
    assert!(result.is_err(), "Should trap on MAX - (-1)");
}

#[test]
fn test_overflow_sub_positive_minus_large_negative() {
    let source = r#"
script fn overflow_sub_test() -> i64 {
    5000000000000000000 - (-5000000000000000000)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-sub-test", &[]);
    assert!(result.is_err(), "Should trap on positive - large negative");
}

#[test]
fn test_overflow_sub_negative_minus_positive() {
    let source = r#"
script fn overflow_sub_test() -> i64 {
    -5000000000000000000 - 5000000000000000000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-sub-test", &[]);
    assert!(result.is_err(), "Should trap on negative - positive");
}

#[test]
fn test_overflow_sub_with_parameters() {
    let source = r#"
script fn overflow_sub_test(x: i64, y: i64) -> i64 {
    x - y
}
"#;

    let wasm = compile_program(source);

    // MIN - 1 should overflow
    let result = execute_wasm(&wasm, "overflow-sub-test", &[i64::MIN, 1]);
    assert!(result.is_err(), "Should trap on MIN - 1");

    // MAX - (-1) should overflow
    let result = execute_wasm(&wasm, "overflow-sub-test", &[i64::MAX, -1]);
    assert!(result.is_err(), "Should trap on MAX - (-1)");

    // 0 - MIN should overflow (MIN is -2^63, negating it overflows)
    let result = execute_wasm(&wasm, "overflow-sub-test", &[0, i64::MIN]);
    assert!(result.is_err(), "Should trap on 0 - MIN");
}

// ============================================================================
// Tests for checked SUBTRACTION - non-overflow cases
// ============================================================================

#[test]
fn test_no_overflow_sub_max_minus_zero() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    9223372036854775807 - 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MAX);
}

#[test]
fn test_no_overflow_sub_min_minus_zero() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min - 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MIN);
}

#[test]
fn test_no_overflow_sub_zero_minus_zero() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    0 - 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 0);
}

#[test]
fn test_no_overflow_sub_positive_minus_positive() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    500 - 200
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 300);
}

#[test]
fn test_no_overflow_sub_negative_minus_negative() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    -200 - (-500)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 300);
}

#[test]
fn test_no_overflow_sub_max_minus_positive() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    9223372036854775807 - 1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MAX - 1);
}

#[test]
fn test_no_overflow_sub_min_minus_negative() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min - (-1)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MIN + 1);
}

#[test]
fn test_no_overflow_sub_max_minus_max() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    9223372036854775807 - 9223372036854775807
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(
        result.is_ok(),
        "Should not trap when subtracting equal values"
    );
    assert_eq!(result.unwrap()[0], 0);
}

#[test]
fn test_no_overflow_sub_min_minus_min() {
    let source = r#"
script fn no_overflow_sub_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min - min
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[]);
    assert!(
        result.is_ok(),
        "Should not trap when subtracting equal values"
    );
    assert_eq!(result.unwrap()[0], 0);
}

#[test]
fn test_no_overflow_sub_with_parameters() {
    let source = r#"
script fn no_overflow_sub_test(x: i64, y: i64) -> i64 {
    x - y
}
"#;

    let wasm = compile_program(source);

    // Test with values that don't overflow
    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[300, 100]);
    assert!(result.is_ok(), "Should not trap with small values");
    assert_eq!(result.unwrap()[0], 200);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[i64::MAX, 0]);
    assert!(result.is_ok(), "Should not trap with MAX - 0");
    assert_eq!(result.unwrap()[0], i64::MAX);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[i64::MAX, i64::MAX]);
    assert!(result.is_ok(), "Should not trap with equal values");
    assert_eq!(result.unwrap()[0], 0);

    let result = execute_wasm(&wasm, "no-overflow-sub-test", &[0, 100]);
    assert!(result.is_ok(), "Should not trap with 0 - positive");
    assert_eq!(result.unwrap()[0], -100);
}

// ============================================================================
// Tests for the subtraction overflow detection formula
// ============================================================================

#[test]
fn test_overflow_sub_formula_positive_minus_negative() {
    // Tests the formula: (x ^ y) < 0 && (diff ^ x) < 0
    // Positive - Negative can overflow to negative
    let source = r#"
script fn test_sub(x: i64, y: i64) -> i64 {
    x - y
}
"#;

    let wasm = compile_program(source);

    // Positive minus large negative wraps to negative
    let result = execute_wasm(&wasm, "test-sub", &[i64::MAX, -1]);
    assert!(result.is_err(), "Overflow not detected: MAX - (-1)");
}

#[test]
fn test_overflow_sub_formula_negative_minus_positive() {
    // Negative - Positive can overflow to positive
    let source = r#"
script fn test_sub(x: i64, y: i64) -> i64 {
    x - y
}
"#;

    let wasm = compile_program(source);

    // Negative minus positive wraps to positive
    let result = execute_wasm(&wasm, "test-sub", &[i64::MIN, 1]);
    assert!(result.is_err(), "Overflow not detected: MIN - 1");
}

#[test]
fn test_overflow_sub_formula_same_signs_never_overflow() {
    // When both operands have same sign, subtraction cannot overflow
    let source = r#"
script fn test_sub(x: i64, y: i64) -> i64 {
    x - y
}
"#;

    let wasm = compile_program(source);

    // Both positive - should never overflow
    let result = execute_wasm(&wasm, "test-sub", &[i64::MAX, 1]);
    assert!(result.is_ok(), "Same signs should not overflow");
    assert_eq!(result.unwrap()[0], i64::MAX - 1);

    // Both negative - should never overflow
    let result = execute_wasm(&wasm, "test-sub", &[i64::MIN, -1]);
    assert!(result.is_ok(), "Same signs should not overflow");
    assert_eq!(result.unwrap()[0], i64::MIN + 1);

    let result = execute_wasm(&wasm, "test-sub", &[i64::MAX, i64::MAX]);
    assert!(result.is_ok(), "Same signs should not overflow");
    assert_eq!(result.unwrap()[0], 0);
}

// ============================================================================
// Tests for checked MULTIPLICATION - overflow cases
// ============================================================================

#[test]
fn test_overflow_mul_i64_max_times_two() {
    let source = r#"
script fn overflow_mul_test() -> i64 {
    9223372036854775807 * 2
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-mul-test", &[]);
    assert!(result.is_err(), "Should trap on MAX * 2");
}

#[test]
fn test_overflow_mul_i64_min_times_neg_one() {
    let source = r#"
script fn overflow_mul_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min * (-1)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-mul-test", &[]);
    assert!(result.is_err(), "Should trap on MIN * -1");
}

#[test]
fn test_overflow_mul_neg_one_times_i64_min() {
    let source = r#"
script fn overflow_mul_test() -> i64 {
    let min = -9223372036854775807 - 1;
    (-1) * min
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-mul-test", &[]);
    assert!(result.is_err(), "Should trap on -1 * MIN");
}

#[test]
fn test_overflow_mul_large_positive_values() {
    let source = r#"
script fn overflow_mul_test() -> i64 {
    4611686018427387904 * 2
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-mul-test", &[]);
    assert!(
        result.is_err(),
        "Should trap on large positive multiplication"
    );
}

#[test]
fn test_overflow_mul_large_negative_values() {
    let source = r#"
script fn overflow_mul_test() -> i64 {
    (-4611686018427387904) * 3
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-mul-test", &[]);
    assert!(
        result.is_err(),
        "Should trap on large negative multiplication"
    );
}

#[test]
fn test_overflow_mul_positive_negative_large() {
    let source = r#"
script fn overflow_mul_test() -> i64 {
    4611686018427387904 * (-3)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-mul-test", &[]);
    assert!(
        result.is_err(),
        "Should trap on large mixed sign multiplication"
    );
}

#[test]
fn test_overflow_mul_max_times_max() {
    let source = r#"
script fn overflow_mul_test() -> i64 {
    9223372036854775807 * 9223372036854775807
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-mul-test", &[]);
    assert!(result.is_err(), "Should trap on MAX * MAX");
}

#[test]
fn test_overflow_mul_min_times_two() {
    let source = r#"
script fn overflow_mul_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min * 2
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "overflow-mul-test", &[]);
    assert!(result.is_err(), "Should trap on MIN * 2");
}

#[test]
fn test_overflow_mul_with_parameters() {
    let source = r#"
script fn overflow_mul_test(x: i64, y: i64) -> i64 {
    x * y
}
"#;

    let wasm = compile_program(source);

    // MAX * 2 should overflow
    let result = execute_wasm(&wasm, "overflow-mul-test", &[i64::MAX, 2]);
    assert!(result.is_err(), "Should trap on MAX * 2");

    // MIN * -1 should overflow
    let result = execute_wasm(&wasm, "overflow-mul-test", &[i64::MIN, -1]);
    assert!(result.is_err(), "Should trap on MIN * -1");

    // MIN * 2 should overflow
    let result = execute_wasm(&wasm, "overflow-mul-test", &[i64::MIN, 2]);
    assert!(result.is_err(), "Should trap on MIN * 2");
}

// ============================================================================
// Tests for checked MULTIPLICATION - non-overflow cases
// ============================================================================

#[test]
fn test_no_overflow_mul_small_values() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    123 * 456
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 56088);
}

#[test]
fn test_no_overflow_mul_zero_times_max() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    0 * 9223372036854775807
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 0);
}

#[test]
fn test_no_overflow_mul_max_times_zero() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    9223372036854775807 * 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 0);
}

#[test]
fn test_no_overflow_mul_one_times_max() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    1 * 9223372036854775807
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MAX);
}

#[test]
fn test_no_overflow_mul_max_times_one() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    9223372036854775807 * 1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MAX);
}

#[test]
fn test_no_overflow_mul_min_times_zero() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min * 0
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 0);
}

#[test]
fn test_no_overflow_mul_min_times_one() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    let min = -9223372036854775807 - 1;
    min * 1
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], i64::MIN);
}

#[test]
fn test_no_overflow_mul_negative_one_times_positive() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    (-1) * 1000
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], -1000);
}

#[test]
fn test_no_overflow_mul_boundary_sqrt_max() {
    // Use a value slightly under the overflow threshold
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    3037000499 * 3037000499
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap at boundary");
    // 3037000499 * 3037000499 = 9223372030926249001
    assert_eq!(result.unwrap()[0], 9223372030926249001);
}

#[test]
fn test_no_overflow_mul_negative_times_negative() {
    let source = r#"
script fn no_overflow_mul_test() -> i64 {
    (-50) * (-100)
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[]);
    assert!(result.is_ok(), "Should not trap");
    assert_eq!(result.unwrap()[0], 5000);
}

#[test]
fn test_no_overflow_mul_with_parameters() {
    let source = r#"
script fn no_overflow_mul_test(x: i64, y: i64) -> i64 {
    x * y
}
"#;

    let wasm = compile_program(source);

    // Test with values that don't overflow
    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[10, 20]);
    assert!(result.is_ok(), "Should not trap with small values");
    assert_eq!(result.unwrap()[0], 200);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[i64::MAX, 0]);
    assert!(result.is_ok(), "Should not trap with MAX * 0");
    assert_eq!(result.unwrap()[0], 0);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[i64::MAX, 1]);
    assert!(result.is_ok(), "Should not trap with MAX * 1");
    assert_eq!(result.unwrap()[0], i64::MAX);

    let result = execute_wasm(&wasm, "no-overflow-mul-test", &[i64::MIN, 1]);
    assert!(result.is_ok(), "Should not trap with MIN * 1");
    assert_eq!(result.unwrap()[0], i64::MIN);
}

// ============================================================================
// Tests for the multiplication overflow detection
// ============================================================================

#[test]
fn test_overflow_mul_formula_product_division_check() {
    // Tests that if product / y != x, then overflow occurred
    let source = r#"
script fn test_mul(x: i64, y: i64) -> i64 {
    x * y
}
"#;

    let wasm = compile_program(source);

    // Large values where product overflows
    let result = execute_wasm(&wasm, "test-mul", &[i64::MAX, 2]);
    assert!(result.is_err(), "Should detect overflow via division check");

    let result = execute_wasm(&wasm, "test-mul", &[i64::MAX / 2 + 1, 3]);
    assert!(result.is_err(), "Should detect overflow via division check");
}

#[test]
fn test_overflow_mul_special_case_min_times_neg_one() {
    // Special case: MIN * -1 must trap
    let source = r#"
script fn test_mul(x: i64, y: i64) -> i64 {
    x * y
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "test-mul", &[i64::MIN, -1]);
    assert!(result.is_err(), "MIN * -1 should trap");

    let result = execute_wasm(&wasm, "test-mul", &[-1, i64::MIN]);
    assert!(result.is_err(), "-1 * MIN should trap");
}

#[test]
fn test_no_overflow_mul_zero_multiplicand() {
    // When multiplying by zero, never overflow
    let source = r#"
script fn test_mul(x: i64, y: i64) -> i64 {
    x * y
}
"#;

    let wasm = compile_program(source);

    let result = execute_wasm(&wasm, "test-mul", &[i64::MAX, 0]);
    assert!(result.is_ok(), "Should not overflow with zero");
    assert_eq!(result.unwrap()[0], 0);

    let result = execute_wasm(&wasm, "test-mul", &[0, i64::MAX]);
    assert!(result.is_ok(), "Should not overflow with zero");
    assert_eq!(result.unwrap()[0], 0);

    let result = execute_wasm(&wasm, "test-mul", &[i64::MIN, 0]);
    assert!(result.is_ok(), "Should not overflow with zero");
    assert_eq!(result.unwrap()[0], 0);
}
