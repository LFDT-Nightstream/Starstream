use crate::interface::{Circuit, CircuitBuilder, CircuitBuilderVar};
use crate::l;

#[derive(Clone)]
struct Switch<Var> {
    switch: Var,
}

struct Switches<Var>(Vec<Var>);
#[must_use]
struct ConsumeSwitches;

impl<Var: CircuitBuilderVar> Switches<Var> {
    fn new() -> (Switches<Var>, ConsumeSwitches) {
        (Switches(vec![]), ConsumeSwitches)
    }
    fn alloc(&mut self, mut cb: impl CircuitBuilder<Var>) -> Switch<Var> {
        let switch = cb.alloc(l!("switch"));
        self.0.push(switch.clone());
        Switch { switch }
    }
    #[must_use]
    fn consume(self, _marker: ConsumeSwitches, mut cb: impl CircuitBuilder<Var>) -> Switch<Var> {
        let zero = cb.zero();
        let one = cb.one();

        let sum = self
            .0
            .iter()
            .fold(zero.clone(), |acc, switch| acc + switch.clone());

        // ensures that sum is # switches on
        for switch in &self.0 {
            cb.enforce(
                l!("switch must be binary"),
                switch.clone(),
                one.clone() - switch.clone(),
                zero.clone(),
            );
        }

        cb.enforce(
            l!("one switch or no switch"),
            sum.clone(),
            one.clone(),
            one.clone(),
        );

        Switch { switch: one - sum }
    }
}

fn cond_assert<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    a: Var,
    b: Var,
) {
    let zero = cb.zero();
    // Either switch is one, in which case this is `a - b = 0 <=> a = b`,
    // or switch is zero, in which case this is always true.
    cb.enforce(l!(), a - b, switch.switch, zero);
}

// NB: this requires that the 0th index of the memory is always zero,
// i.e. that *nullptr = 0
// FIXME: this could be done more efficiently probably
// FIXME: make this part of the interface
// FIXME: make variants that allocate old and new to reduce variables
fn cond_memory<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    namespace: Var,
    addr: Var,
    old: Var,
    new: Var,
) {
    let real_addr = cb.alloc(l!("real_addr"));
    let real_old = cb.alloc(l!("real_old"));
    let real_new = cb.alloc(l!("real_new"));
    cb.enforce(l!(), switch.switch.clone(), addr.clone(), real_addr.clone());
    cb.enforce(l!(), switch.switch.clone(), old.clone(), real_old.clone());
    cb.enforce(l!(), switch.switch.clone(), new.clone(), real_new.clone());
    cb.memory(namespace, real_addr, real_old, real_new);
}

/* This is supposed to be a WASM VM.
 * We don't support WASM as-is, but instead a simplified variant without types and
 * type checking, mainly affecting (dynamic) function calls which can be called invalidly
 * in our VM.
 *
 * TODO: We don't support memory for now as a simplification
 *       but will in the future.
 *
 * Local variables are represented as ordinary "slots" in the stack
 * and thus you can e.g. remove them with pop.
 * This simplifies function calling heavily.
 * Instructions to modify locals thus take an index into the stack,
 * meaning you need to calculate the correct index when compiling from
 * WASM to the IR (which should be trivial).
 * Most instructions work directly on fields and modulus operations
 * must be done separately for efficiency.
 *
 * TODO: range check via lookup
 *
 * We don't consider public input for now.
 *
 * TODO: implement techniques from EDEN/Zorp stuff for stack,
 *       figure out how to do memory better than Nebula,
 *       do lookups
 */
#[allow(non_camel_case_types)]
pub struct WASM_VM;

#[repr(u32)]
pub enum Instr {
    // not provable, hence it has no corresponding code
    Unreachable = 0,
    // {} -> {}
    Nop,
    // x -> {}
    Drop,
    // {} -> code[pc+1]
    Const,
    // x y -> x+y
    Add,
    // {} -> stack[sp - code[pc+1]]
    // NB: code[pc+1] must be greater than 0
    Get,
    // x -> {}; stack[sp - code[pc+1]] := x
    // NB: code[pc+1] must be greater than 1
    Set,
    // x -> stack[sp - code[pc+1]]; stack[sp - code[pc+1]] := x
    // NB: code[pc+1] must be greater than 0
    Swap,
    // c new_pc_namespace new_pc -> {}; pc := new_pc, pc_namespace := new_pc_namespace if c == 0, else if c != 0 then pc := pc + 1
    CondJump,
    // new_pc_namespace new_pc -> {}; pc := new_pc, pc_namespace := new_pc_namespace
    Jump,
    // ptr -> memory[ptr]
    Read,
    // ptr data -> {}; memory[ptr] = data
    Write,
    // {} -> code[pc+1] zeroes
    Alloc,
    // x y c -> y if c == 0
    // x y c -> x if c != 0
    Select,
    // not an instruction, signifies end of enum
    _End,
}

pub const N_GLOBAL_WITNESSES: usize = 2;
pub const WITNESS_OFFSET_NOP: usize = N_GLOBAL_WITNESSES;
pub const WITNESS_OFFSET_DROP: usize = WITNESS_OFFSET_NOP + 1;
pub const WITNESS_OFFSET_CONST: usize = WITNESS_OFFSET_DROP + 5;
pub const WITNESS_OFFSET_ADD: usize = WITNESS_OFFSET_CONST + 4;
pub const WITNESS_OFFSET_GET: usize = WITNESS_OFFSET_ADD + 42;
pub const WITNESS_OFFSET_SET: usize = WITNESS_OFFSET_GET + 8;
pub const WITNESS_OFFSET_SWAP: usize = WITNESS_OFFSET_SET + 9;
pub const WITNESS_OFFSET_COND_JUMP_C_NOT_0: usize = WITNESS_OFFSET_SWAP + 10;
pub const WITNESS_OFFSET_COND_JUMP_C_0: usize = WITNESS_OFFSET_COND_JUMP_C_NOT_0 + 14;
pub const WITNESS_OFFSET_JUMP: usize = WITNESS_OFFSET_COND_JUMP_C_0 + 10;
pub const WITNESS_OFFSET_READ: usize = WITNESS_OFFSET_JUMP + 7;
pub const WITNESS_OFFSET_WRITE: usize = WITNESS_OFFSET_READ + 9;
pub const WITNESS_OFFSET_ALLOC: usize = WITNESS_OFFSET_WRITE + 13;
pub const WITNESS_OFFSET_SELECT_C_NOT_0: usize = WITNESS_OFFSET_ALLOC + 1;
pub const WITNESS_OFFSET_SELECT_C_0: usize = WITNESS_OFFSET_SELECT_C_NOT_0 + 10;
pub const WITNESS_OFFSET_EMPTY_STEP: usize = WITNESS_OFFSET_SELECT_C_0 + 12;
pub const WITNESS_OFFSET_OPCODE_LOOKUP: usize = WITNESS_OFFSET_EMPTY_STEP;
pub const N_TOTAL_WITNESSES: usize = WITNESS_OFFSET_OPCODE_LOOKUP + 2;

#[allow(non_camel_case_types)]
pub enum WASM_IO {
    /// When we're in a UTXO, our stack switches to that of the UTXO.
    /// All coordination scripts share the same stack.
    ///
    /// The concrete namespace is decided by the "host".
    sp_namespace,
    /// This is the stack pointer. It starts at 0, and thus,
    /// it points to the first new element, and the value is
    /// also equal to the size of the stack.
    sp,
    /// This is the namespace used for looking up instructions
    /// and for performing linear memory operations.
    /// Although coordination scripts share the same stack,
    /// they all have different memories.
    ///
    /// NB: This means that if you have e.g. two UTXOs with the same code,
    /// that code must unfortunately be duplicated into two separate namespaces,
    /// since otherwise they would share the same memory.
    ///
    /// The concrete namespace is decided by the "host".
    pc_namespace,
    /// Points to the next instruction. Many instructions will
    /// also carry a constant at the cell one after the instruction.
    pc,
}

#[derive(Clone)]
struct WASMGlobal<Var> {
    // The variable containing the opcode, shared for convenience.
    opcode: Var,
    // The parameter to the instruction, or the next instruction.
    // Only used in the former case.
    param: Var,
    // The old state of the VM
    sp_namespace: Var,
    sp: Var,
    pc_namespace: Var,
    pc: Var,
    // The new state of the VM
    new_sp_namespace: Var,
    new_sp: Var,
    new_pc_namespace: Var,
    new_pc: Var,
}

fn visit_nop<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_nop"));
    cb.assert_offset(WITNESS_OFFSET_NOP);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Nop as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_sp_namespace,
        sp_namespace,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc_namespace,
        pc_namespace,
    );
}

fn visit_drop<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_drop"));
    cb.assert_offset(WITNESS_OFFSET_DROP);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Drop as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let prev = cb.alloc(l!("prev"));
    let zero = cb.zero();
    let one = cb.one();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        prev,
        zero,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_const<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: constant,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_const"));
    cb.assert_offset(WITNESS_OFFSET_CONST);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Const as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone(),
        zero,
        constant,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc,
        pc + one.clone() + one,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_add<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_add"));
    cb.assert_offset(WITNESS_OFFSET_ADD);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Add as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let first = cb.alloc(l!("first"));
    let second = cb.alloc(l!("second"));
    let one = cb.one();
    let two = cb.lit(2);
    let zero = cb.zero();
    // TODO: implement this with lookups instead
    let result_mod_2_32 = (0..32)
        .map(|i| {
            let bit = cb.alloc(l!("bit"));
            cb.enforce(l!(), bit.clone(), one.clone() - bit.clone(), zero.clone());
            bit * 2i128.pow(i)
        })
        .fold(zero.clone(), |acc, power| acc + power);
    let result = {
        let last_bit = cb.alloc(l!("last_bit"));
        cb.enforce(
            l!(),
            last_bit.clone(),
            one.clone() - last_bit.clone(),
            zero.clone(),
        );
        result_mod_2_32.clone() + last_bit * 2i128.pow(32)
    };
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        first.clone() + second.clone(),
        result.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        first.clone(),
        zero,
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two,
        second.clone(),
        result_mod_2_32,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_get<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: index,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_get"));
    cb.assert_offset(WITNESS_OFFSET_GET);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Get as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let value = cb.alloc(l!("value"));
    let zero = cb.zero();
    let one = cb.one();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - index,
        value.clone(),
        value.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone(),
        zero,
        value,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc,
        pc + one.clone() + one,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_set<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: index,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_set"));
    cb.assert_offset(WITNESS_OFFSET_SET);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Set as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let old = cb.alloc(l!("old"));
    let new = cb.alloc(l!("new"));
    let zero = cb.zero();
    let one = cb.one();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        new.clone(),
        zero,
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - index,
        old.clone(),
        new.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc,
        pc + one.clone() + one,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_swap<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: index,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_swap"));
    cb.assert_offset(WITNESS_OFFSET_SWAP);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Swap as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let x = cb.alloc(l!("x"));
    let y = cb.alloc(l!("y"));
    let one = cb.one();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        x.clone(),
        y.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - index,
        y.clone(),
        x.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc,
        pc + one.clone() + one,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_cond_jump_c_not_0<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_cond_jump_c_not_0"));
    cb.assert_offset(WITNESS_OFFSET_COND_JUMP_C_NOT_0);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::CondJump as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    let unused_pc = cb.alloc(l!("unused_pc"));
    let unused_pc_namespace = cb.alloc(l!("unused_pc_namespace"));
    let two = cb.lit(2);
    let three = cb.lit(3);
    let c = cb.alloc(l!("c"));
    let c_inv = cb.alloc(l!("c_inv"));
    cb.enforce(l!(), c.clone(), c_inv, switch.switch.clone());
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - three.clone(),
        c.clone(),
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two,
        unused_pc_namespace,
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        unused_pc,
        zero.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - three);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_cond_jump_c_0<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace: _,
        pc: _,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_cond_jump_c_0"));
    cb.assert_offset(WITNESS_OFFSET_COND_JUMP_C_0);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::CondJump as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    let three = cb.lit(3);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - three.clone(),
        zero.clone(),
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two,
        new_pc_namespace,
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one,
        new_pc,
        zero,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - three);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
}

fn visit_jump<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace: _,
        pc: _,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_jump"));
    cb.assert_offset(WITNESS_OFFSET_JUMP);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Jump as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two.clone(),
        new_pc_namespace,
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        new_pc,
        zero,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
}

fn visit_read<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_read"));
    cb.assert_offset(WITNESS_OFFSET_READ);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Read as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let address = cb.alloc(l!("address"));
    let value = cb.alloc(l!("value"));
    let one = cb.one();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace.clone(),
        address.clone(),
        value.clone(),
        value.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        address.clone(),
        value.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace,
        new_pc_namespace,
    );
}

fn visit_write<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_write"));
    cb.assert_offset(WITNESS_OFFSET_WRITE);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Write as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let address = cb.alloc(l!("address"));
    let old = cb.alloc(l!("old"));
    let new = cb.alloc(l!("new"));
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace.clone(),
        address.clone(),
        old.clone(),
        new.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        new.clone(),
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two.clone(),
        address.clone(),
        zero.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace,
        new_pc_namespace,
    );
}

fn visit_alloc<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: count,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_alloc"));
    cb.assert_offset(WITNESS_OFFSET_ALLOC);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Alloc as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + count);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc,
        pc + one.clone() + one,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_select_c_not_0<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_select_c_not_0"));
    cb.assert_offset(WITNESS_OFFSET_SELECT_C_NOT_0);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Select as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    let c = cb.alloc(l!("c"));
    let c_inv = cb.alloc(l!("c_inv"));
    let y = cb.alloc(l!("y"));
    cb.enforce(l!(), c.clone(), c_inv, switch.switch.clone());
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two.clone(),
        y,
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        c,
        zero.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_select_c_0<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_select_c_0"));
    cb.assert_offset(WITNESS_OFFSET_SELECT_C_0);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Select as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    let three = cb.lit(3);
    let x = cb.alloc(l!("x"));
    let y = cb.alloc(l!("y"));
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - three.clone(),
        x.clone(),
        y.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two.clone(),
        y,
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        zero.clone(),
        zero,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc_namespace,
        pc_namespace,
    );
}

fn visit_empty_step<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    WASMGlobal {
        opcode: _,
        param: _,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_empty_step"));
    // NB: We must allocate no new variables in visit_empty_step
    cb.assert_offset(WITNESS_OFFSET_EMPTY_STEP);
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc_namespace,
        pc_namespace,
    );
    // Verifies that no variables have been allocated
    cb.assert_offset(WITNESS_OFFSET_EMPTY_STEP);
}

impl Circuit<WASM_IO> for WASM_VM {
    fn run<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
        &self,
        mut cb: B,
        input: impl Fn(WASM_IO) -> Var,
        output: impl Fn(WASM_IO) -> Var,
    ) {
        let (mut switches, consume_switches) = Switches::new();
        let sp_namespace = input(WASM_IO::sp_namespace);
        let sp = input(WASM_IO::sp);
        let pc_namespace = input(WASM_IO::pc_namespace);
        let pc = input(WASM_IO::pc);
        let new_sp_namespace = output(WASM_IO::sp_namespace);
        let new_sp = output(WASM_IO::sp);
        let new_pc_namespace = output(WASM_IO::pc_namespace);
        let new_pc = output(WASM_IO::pc);
        let opcode = cb.alloc(l!("opcode"));
        let param = cb.alloc(l!("param"));
        let global = WASMGlobal {
            opcode,
            param,
            sp_namespace,
            sp,
            pc_namespace,
            pc,
            new_sp_namespace,
            new_sp,
            new_pc_namespace,
            new_pc,
        };
        cb.assert_offset(N_GLOBAL_WITNESSES);
        visit_nop(cb.nest(l!()), &mut switches, global.clone());
        visit_drop(cb.nest(l!()), &mut switches, global.clone());
        visit_const(cb.nest(l!()), &mut switches, global.clone());
        visit_add(cb.nest(l!()), &mut switches, global.clone());
        visit_get(cb.nest(l!()), &mut switches, global.clone());
        visit_set(cb.nest(l!()), &mut switches, global.clone());
        visit_swap(cb.nest(l!()), &mut switches, global.clone());
        visit_cond_jump_c_not_0(cb.nest(l!()), &mut switches, global.clone());
        visit_cond_jump_c_0(cb.nest(l!()), &mut switches, global.clone());
        visit_jump(cb.nest(l!()), &mut switches, global.clone());
        visit_read(cb.nest(l!()), &mut switches, global.clone());
        visit_write(cb.nest(l!()), &mut switches, global.clone());
        visit_alloc(cb.nest(l!()), &mut switches, global.clone());
        visit_select_c_not_0(cb.nest(l!()), &mut switches, global.clone());
        visit_select_c_0(cb.nest(l!()), &mut switches, global.clone());
        let is_empty_step = switches.consume(consume_switches, cb.nest(l!()));
        visit_empty_step(cb.nest(l!()), is_empty_step.clone(), global.clone());
        {
            let mut cb = cb.nest(l!("opcode lookup"));
            cb.assert_offset(WITNESS_OFFSET_OPCODE_LOOKUP);
            let one = cb.one();
            let real_opcode_idx = cb.alloc(l!("real_opcode_idx"));
            let real_param_idx = cb.alloc(l!("real_param_idx"));
            let switch = Switch {
                switch: one.clone() - is_empty_step.switch.clone(),
            };
            cb.enforce(
                l!(),
                switch.switch.clone(),
                global.pc.clone(),
                real_opcode_idx.clone(),
            );
            cb.enforce(
                l!(),
                switch.switch.clone(),
                global.pc.clone() + one.clone(),
                real_param_idx.clone(),
            );
            cb.lookup(
                global.pc_namespace.clone(),
                real_opcode_idx,
                global.opcode.clone(),
            );
            cb.lookup(
                global.pc_namespace.clone(),
                real_param_idx,
                global.param.clone(),
            );
            cb.assert_offset(N_TOTAL_WITNESSES);
        }
    }
}
