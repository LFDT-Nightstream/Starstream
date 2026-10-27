//! Cleans up how a function moves values through its locals.
//!
//! Codegen stores values in locals more often than it needs to, for example a
//! `let` binding copied into another local before its single use. Each rule
//! below is valid on its own, and they're applied until none applies:
//!
//! 1. `local.set N; local.get N` → `local.tee N`
//! 2. `local.tee N; drop` → `local.set N`
//! 3. `local.tee N`, where N is never read → nothing
//! 4. `local.set N`, where N is never read → `drop`
//! 5. `local.get`, `global.get` or a constant, then `drop` → nothing
//!
//! Declared locals no instruction refers to anymore are then removed and the
//! rest renumbered. Parameters are never removed.

use wasm_encoder::Function;
use wasm_encoder::reencode::{Error, Reencode, RoundtripReencoder};
use wasmparser::{FunctionBody, Operator, ValType};

/// Rewrite `body`, whose function has `params` parameters, or `None` if no
/// rule applies.
pub fn clean_up(body: &FunctionBody, params: u32) -> Result<Option<Function>, Error> {
    let mut locals = Vec::new();
    for group in body.get_locals_reader()? {
        let (count, ty) = group?;
        locals.extend(std::iter::repeat_n(ty, count as usize));
    }
    let mut operators = body
        .get_operators_reader()?
        .into_iter()
        .collect::<Result<Vec<_>, _>>()?;

    let mut changed = false;
    while rewrite(&mut operators) {
        changed = true;
    }
    changed |= remove_unused_locals(&mut operators, &mut locals, params);
    if !changed {
        return Ok(None);
    }

    let mut reencoder = RoundtripReencoder;
    let mut groups = Vec::<(u32, wasm_encoder::ValType)>::new();
    for ty in locals {
        let ty = reencoder.val_type(ty)?;
        match groups.last_mut() {
            Some((count, last)) if *last == ty => *count += 1,
            _ => groups.push((1, ty)),
        }
    }
    let mut function = Function::new(groups);
    for operator in operators {
        function.instruction(&reencoder.instruction(operator)?);
    }
    Ok(Some(function))
}

/// Apply one round of the rules, returning whether any applied.
///
/// Read counts are taken before the round, and rules only ever remove reads,
/// so a stale count can only keep a store that the next round removes.
fn rewrite(operators: &mut Vec<Operator<'_>>) -> bool {
    let mut reads = Vec::<usize>::new();
    for operator in operators.iter() {
        if let Operator::LocalGet { local_index } = operator {
            let index = *local_index as usize;
            if reads.len() <= index {
                reads.resize(index + 1, 0);
            }
            reads[index] += 1;
        }
    }
    let unread = |index: u32| reads.get(index as usize).is_none_or(|&count| count == 0);

    let mut rewritten = Vec::with_capacity(operators.len());
    let mut changed = false;
    let mut i = 0;
    while i < operators.len() {
        let next = operators.get(i + 1);
        match (&operators[i], next) {
            (Operator::LocalSet { local_index }, Some(Operator::LocalGet { local_index: get }))
                if local_index == get =>
            {
                rewritten.push(Operator::LocalTee {
                    local_index: *local_index,
                });
                i += 2;
            }
            (Operator::LocalTee { local_index }, Some(Operator::Drop)) => {
                rewritten.push(Operator::LocalSet {
                    local_index: *local_index,
                });
                i += 2;
            }
            (Operator::LocalTee { local_index }, _) if unread(*local_index) => {
                i += 1;
            }
            (Operator::LocalSet { local_index }, _) if unread(*local_index) => {
                rewritten.push(Operator::Drop);
                i += 1;
            }
            (
                Operator::LocalGet { .. }
                | Operator::GlobalGet { .. }
                | Operator::I32Const { .. }
                | Operator::I64Const { .. },
                Some(Operator::Drop),
            ) => {
                i += 2;
            }
            (operator, _) => {
                rewritten.push(operator.clone());
                i += 1;
                continue;
            }
        }
        changed = true;
    }
    *operators = rewritten;
    changed
}

/// Remove declared locals no operator refers to, renumbering the rest.
/// Returns whether any were removed.
fn remove_unused_locals(
    operators: &mut [Operator<'_>],
    locals: &mut Vec<ValType>,
    params: u32,
) -> bool {
    let mut used = vec![false; locals.len()];
    for operator in operators.iter() {
        if let Some(index) = local_index(operator)
            && index >= params
        {
            used[(index - params) as usize] = true;
        }
    }
    if used.iter().all(|&used| used) {
        return false;
    }

    let mut renumbered = Vec::with_capacity(locals.len());
    let mut next = params;
    for &used in &used {
        renumbered.push(next);
        if used {
            next += 1;
        }
    }
    for operator in operators.iter_mut() {
        if let Operator::LocalGet { local_index }
        | Operator::LocalSet { local_index }
        | Operator::LocalTee { local_index } = operator
            && *local_index >= params
        {
            *local_index = renumbered[(*local_index - params) as usize];
        }
    }
    let mut used = used.into_iter();
    locals.retain(|_| used.next().expect("one flag per local"));
    true
}

fn local_index(operator: &Operator<'_>) -> Option<u32> {
    match operator {
        Operator::LocalGet { local_index }
        | Operator::LocalSet { local_index }
        | Operator::LocalTee { local_index } => Some(*local_index),
        _ => None,
    }
}
