use quickcheck_macros::quickcheck;
use starstream_dsl::{DefaultOpcodeHandler, OpcodeHandler};
use starstream_interpreter::interpreter::{ExecutionContext};
use starstream_types::*;

#[quickcheck]
fn quickcheck_swap_opcode(a: i64, b: i64) -> bool {
    let mut context = ExecutionContext::new();
    context.push(a).unwrap();
    context.push(b).unwrap();
    DefaultOpcodeHandler.handle_opcode(&Opcode::Swap, &mut context).unwrap();
    context.pop().unwrap() == a && context.pop().unwrap() == b && context.pop().is_err()
}

#[quickcheck]
fn quickcheck_add_opcode(a: i64, b: i64) -> bool {
    let Some(want) = a.checked_add(b) else { return true; };

    let mut context = ExecutionContext::new();
    context.push(a).unwrap();
    context.push(b).unwrap();
    
    // Test addition
    DefaultOpcodeHandler.handle_opcode(&Opcode::Add, &mut context).unwrap();
    let result = context.pop().unwrap();
    result == want
}

#[quickcheck]
fn quickcheck_equal_opcode(a: i64, b: i64) -> bool {
    let mut context = ExecutionContext::new();
    context.push(a).unwrap();
    context.push(b).unwrap();
    
    // Test equality
    DefaultOpcodeHandler.handle_opcode(&Opcode::Equal, &mut context).unwrap();
    let result = context.pop().unwrap();
    result == if a == b { 1 } else { 0 }
}

#[quickcheck]
fn quickcheck_dup_opcode(values: Vec<i64>) -> bool {
    if values.is_empty() {
        return true;
    }
    
    let mut context = ExecutionContext::new();
    
    // Push all values
    for &value in &values {
        context.push(value).unwrap();
    }
    
    // Test dup operation on the last value
    let last_value = *values.last().unwrap();
    DefaultOpcodeHandler.handle_opcode(&Opcode::Dup, &mut context).unwrap();
    
    // Check that we have the last value twice
    let duped = context.pop().unwrap();
    let original = context.pop().unwrap();
    
    duped == last_value && original == last_value
}

#[quickcheck]
fn quickcheck_store_and_load(name: String, value: i64) -> bool {
    if name.is_empty() {
        return true; // Skip empty names
    }
    
    let mut context = ExecutionContext::new();
    
    // Store variable
    context.push(value).unwrap();
    DefaultOpcodeHandler.handle_opcode(&Opcode::Store(name.clone()), &mut context).unwrap();
    
    // Load variable
    DefaultOpcodeHandler.handle_opcode(&Opcode::Load(name), &mut context).unwrap();
    let loaded_value = context.pop().unwrap();
    
    loaded_value == value
}

#[quickcheck]
fn quickcheck_and_opcode(a: i64, b: i64) -> bool {
    let mut context = ExecutionContext::new();
    context.push(a).unwrap();
    context.push(b).unwrap();
    
    // Test AND operation
    DefaultOpcodeHandler.handle_opcode(&Opcode::And, &mut context).unwrap();
    let and_result = context.pop().unwrap();
    let expected_and = if a != 0 && b != 0 { 1 } else { 0 };
    
    and_result == expected_and
}

#[quickcheck]
fn quickcheck_negate_opcode(value: i64) -> bool {
    // otherwise shrinking will stack overflow on this giant number
    if value == i64::MIN { return true; }

    let mut context = ExecutionContext::new();
    context.push(value).unwrap();
    
    // Test negation
    DefaultOpcodeHandler.handle_opcode(&Opcode::Negate, &mut context).unwrap();
    let negated = context.pop().unwrap();
    
    negated == -value
}