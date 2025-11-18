use crate::interface::{BranchedCircuit, CircuitBuilder, CircuitBuilderVar, Location};
use crate::l;

pub enum MemoryTable {
    Stack,
    Memory,
    HelperStack,
}

pub enum LookupTable {
    Code,
    HostInteractions,
}

#[allow(non_camel_case_types)]
#[derive(Clone)]
pub enum WASM_IO {
    /// This is the stack pointer. It starts at 0, and thus,
    /// it points to the first new element, and the value is
    /// also equal to the size of the stack.
    sp,
    /// Points to the next instruction. Many instructions will
    /// also carry a constant at the cell one after the instruction.
    pc,
    /// Stack pointer to top of helper stack.
    helper_sp,
    /// Temporary register
    reg,
    /// "coordination counter"
    cc,
}

trait Builder<Var: CircuitBuilderVar>: CircuitBuilder<Var, WASM_IO, LookupTable, MemoryTable> {}
impl<Var: CircuitBuilderVar, B: CircuitBuilder<Var, WASM_IO, LookupTable, MemoryTable>> Builder<Var>
    for B
{
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
    /// not provable, hence it has no corresponding code
    Unreachable = 0,
    /// {} -> {}
    Nop = 1,
    /// x -> {}
    Drop = 2,
    /// {} -> code[pc+1]
    Const = 3,
    /// x y -> x+y
    Add = 4,
    /// {} -> stack[sp - code[pc+1]]
    /// NB: code[pc+1] must be greater than 0
    Get = 5,
    /// x -> {}; stack[sp - code[pc+1]] := x
    /// NB: code[pc+1] must be greater than 1
    Set = 6,
    /// x -> stack[sp - code[pc+1]]; stack[sp - code[pc+1]] := x
    /// NB: code[pc+1] must be greater than 0
    Swap = 7,
    /// c new_pc -> {}; pc := new_pc if c == 0, else if c != 0 then pc := pc + 1
    CondJump = 8,
    /// new_pc -> {}; pc := new_pc
    Jump = 9,
    /// ptr -> memory[ptr]
    Read = 10,
    /// ptr data -> {}; memory[ptr] = data
    Write = 11,
    /// {} -> code[pc+1] zeroes
    Alloc = 12,
    /// x y c -> y if c == 0
    /// x y c -> x if c != 0
    Select = 13,
    /// x -> {}; reg := x
    SetReg = 14,
    /// {} -> reg
    GetReg = 15,
    /// x -> {}; push_helper x
    ToHelper = 16,
    /// {} -> y; y = pop_helper
    FromHelper = 17,
    InitHostCall = 18,
    ToHost = 19,
    FromHost = 20,
    Mul = 21,
    Neg = 22,
    Not = 23,
    // not an instruction, signifies end of enum
    _End = 24,
}

/*
macro_rules! define_offsets {
    (@acc $prev:expr; ) => {}; // Base case: no more pairs.

    (@acc $prev:expr; $name:ident = $n:expr $(, $($rest:tt)*)? ) => {
        pub const $name: (usize, usize) = ($prev.0 + $prev.1, $n);
        define_offsets!(@acc $name; $($($rest)*)?);
    };

    // Entry point: start with prev=0 and the list.
    ( $($offsets:tt)* ) => {
        define_offsets!(@acc (0, 0); $($offsets)* );
    };
}
*/

#[derive(Clone)]
struct WASMGlobal<Var> {
    // The old state of the VM
    sp: Var,
    pc: Var,
    helper_sp: Var,
    reg: Var,
    cc: Var,
    // The new state of the VM
    new_sp: Var,
    new_pc: Var,
    new_helper_sp: Var,
    new_reg: Var,
    new_cc: Var,
}

fn globals<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, opcode: Instr) -> WASMGlobal<Var> {
    let sp = cb.input(WASM_IO::sp);
    let pc = cb.input(WASM_IO::pc);
    let helper_sp = cb.input(WASM_IO::helper_sp);
    let reg = cb.input(WASM_IO::reg);
    let cc = cb.input(WASM_IO::cc);
    let new_sp = cb.output(WASM_IO::sp);
    let new_pc = cb.output(WASM_IO::pc);
    let new_helper_sp = cb.output(WASM_IO::helper_sp);
    let new_reg = cb.output(WASM_IO::reg);
    let new_cc = cb.output(WASM_IO::cc);
    let opcode = cb.lit(opcode as i128);
    cb.lookup(LookupTable::Code, pc.clone(), opcode);
    WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }
}
fn param<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, pc: Var) -> Var {
    let one = cb.one();
    let param = cb.alloc(l!("param"));
    cb.lookup(LookupTable::Code, pc + one, param.clone());
    param
}

fn assert<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, x: Var, y: Var) {
    let one = cb.one();
    cb.enforce(l!(), one, x, y);
}

fn visit_nop<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_nop"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Nop);
    let one = cb.one();
    assert(cb.nest(l!()), new_sp, sp);
    assert(cb.nest(l!()), new_pc, pc + one);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_drop<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_drop"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Drop);
    let prev = cb.alloc(l!("prev"));
    let zero = cb.zero();
    let one = cb.one();
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        prev,
        zero.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp - one.clone());
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_const<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_const"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Const);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    let constant = param(cb.nest(l!()), pc.clone());
    cb.memory(MemoryTable::Stack, sp.clone(), zero.clone(), constant);
    assert(cb.nest(l!()), new_sp, sp + one);
    assert(cb.nest(l!()), new_pc, pc + two);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_get<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_get"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Get);
    let index = param(cb.nest(l!()), pc.clone());
    let value = cb.alloc(l!("value"));
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - index,
        value.clone(),
        value.clone(),
    );
    cb.memory(MemoryTable::Stack, sp.clone(), zero.clone(), value);
    assert(cb.nest(l!()), new_sp, sp + one);
    assert(cb.nest(l!()), new_pc, pc + two);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_set<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_set"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Set);
    let index = param(cb.nest(l!()), pc.clone());
    let old = cb.alloc(l!("old"));
    let new = cb.alloc(l!("new"));
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        new.clone(),
        zero.clone(),
    );
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - index,
        old.clone(),
        new.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp - one);
    assert(cb.nest(l!()), new_pc, pc + two);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_swap<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_swap"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Swap);
    let index = param(cb.nest(l!()), pc.clone());
    let x = cb.alloc(l!("x"));
    let y = cb.alloc(l!("y"));
    let one = cb.one();
    let two = cb.lit(2);
    cb.memory(MemoryTable::Stack, sp.clone() - one, x.clone(), y.clone());
    cb.memory(MemoryTable::Stack, sp.clone() - index, y.clone(), x.clone());
    assert(cb.nest(l!()), new_sp, sp);
    assert(cb.nest(l!()), new_pc, pc + two);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn assert_non_zero<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, x: Var) {
    let x_inv = cb.alloc(l!("x_inv"));
    let one = cb.one();
    cb.enforce(l!(), x, x_inv, one);
}

fn visit_cond_jump_c_not_0<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_cond_jump_c_not_0"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::CondJump);
    let one = cb.one();
    let unused_pc = cb.alloc(l!("unused_pc"));
    let two = cb.lit(2);
    let c = cb.alloc(l!("c"));
    assert_non_zero(cb.nest(l!()), c.clone());
    let zero = cb.zero();
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - two.clone(),
        c.clone(),
        zero.clone(),
    );
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        unused_pc,
        zero.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp - two);
    assert(cb.nest(l!()), new_pc, pc + one);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_cond_jump_c_0<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_cond_jump_c_0"));
    let WASMGlobal {
        sp,
        pc: _,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::CondJump);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - two.clone(),
        zero.clone(),
        zero.clone(),
    );
    cb.memory(MemoryTable::Stack, sp.clone() - one, new_pc, zero.clone());
    assert(cb.nest(l!()), new_sp, sp - two);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_jump<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_jump"));
    let WASMGlobal {
        sp,
        pc: _,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Jump);
    let zero = cb.zero();
    let one = cb.one();
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        new_pc,
        zero.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp - one);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_read<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_read"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Read);
    let address = cb.alloc(l!("address"));
    let value = cb.alloc(l!("value"));
    let one = cb.one();
    cb.memory(
        MemoryTable::Memory,
        address.clone(),
        value.clone(),
        value.clone(),
    );
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        address.clone(),
        value.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp);
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_write<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_write"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Write);
    let address = cb.alloc(l!("address"));
    let old = cb.alloc(l!("old"));
    let new = cb.alloc(l!("new"));
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    cb.memory(
        MemoryTable::Memory,
        address.clone(),
        old.clone(),
        new.clone(),
    );
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        new.clone(),
        zero.clone(),
    );
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - two.clone(),
        address.clone(),
        zero.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp - two);
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_alloc<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_alloc"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Alloc);
    let count = param(cb.nest(l!()), pc.clone());
    let two = cb.lit(2);
    assert(cb.nest(l!()), new_sp, sp + count);
    assert(cb.nest(l!()), new_pc, pc + two);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_select_c_not_0<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_select_c_not_0"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Select);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    let c = cb.alloc(l!("c"));
    let y = cb.alloc(l!("y"));
    assert_non_zero(cb.nest(l!()), c.clone());
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - two.clone(),
        y,
        zero.clone(),
    );
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        c,
        zero.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp - two);
    assert(cb.nest(l!()), new_pc, pc + one);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_select_c_0<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_select_c_0"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::Select);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    let three = cb.lit(3);
    let x = cb.alloc(l!("x"));
    let y = cb.alloc(l!("y"));
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - three.clone(),
        x.clone(),
        y.clone(),
    );
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - two.clone(),
        y,
        zero.clone(),
    );
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        zero.clone(),
        zero.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp - two);
    assert(cb.nest(l!()), new_pc, pc + one);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_set_reg<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_set_reg"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg: _,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::SetReg);
    let zero = cb.zero();
    let one = cb.one();
    let val = cb.alloc(l!("val"));
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        val.clone(),
        zero.clone(),
    );
    assert(cb.nest(l!()), new_sp, sp - one.clone());
    assert(cb.nest(l!()), new_pc, pc + one);
    assert(cb.nest(l!()), new_reg, val);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_get_reg<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_get_reg"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::GetReg);
    let zero = cb.zero();
    let one = cb.one();
    let val = cb.alloc(l!("val"));
    cb.memory(MemoryTable::Stack, sp.clone(), zero.clone(), val.clone());
    assert(cb.nest(l!()), new_sp, sp + one.clone());
    assert(cb.nest(l!()), new_pc, pc + one);
    assert(cb.nest(l!()), reg.clone(), val);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_to_helper<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_to_helper"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::ToHelper);
    let val = cb.alloc(l!("val"));
    let zero = cb.zero();
    let one = cb.one();
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        val.clone(),
        zero.clone(),
    );
    cb.memory(
        MemoryTable::HelperStack,
        helper_sp.clone(),
        zero.clone(),
        val,
    );
    assert(cb.nest(l!()), new_sp, sp - one.clone());
    assert(cb.nest(l!()), new_helper_sp, helper_sp + one.clone());
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_from_helper<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_from_helper"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::FromHelper);
    let val = cb.alloc(l!("val"));
    let zero = cb.zero();
    let one = cb.one();
    cb.memory(
        MemoryTable::HelperStack,
        helper_sp.clone() - one.clone(),
        val.clone(),
        zero.clone(),
    );
    cb.memory(MemoryTable::Stack, sp.clone(), zero.clone(), val);
    assert(cb.nest(l!()), new_sp, sp + one.clone());
    assert(cb.nest(l!()), new_helper_sp, helper_sp - one.clone());
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_init_host_call<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_init_host_call"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::InitHostCall);
    let host_call = param(cb.nest(l!()), pc.clone());
    cb.lookup(LookupTable::HostInteractions, cc.clone(), host_call);
    let one = cb.one();
    let two = cb.lit(2);
    assert(cb.nest(l!()), new_sp, sp);
    assert(cb.nest(l!()), new_helper_sp, helper_sp);
    assert(cb.nest(l!()), new_pc, pc + two);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), new_cc, cc + one);
}

fn visit_to_host<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_to_host"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::ToHost);
    let val = cb.alloc(l!("val"));
    let zero = cb.zero();
    let one = cb.one();
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        val.clone(),
        zero.clone(),
    );
    cb.lookup(LookupTable::HostInteractions, cc.clone(), val);
    assert(cb.nest(l!()), new_sp, sp - one.clone());
    assert(cb.nest(l!()), new_helper_sp, helper_sp);
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), new_cc, cc + one);
}

fn visit_from_host<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_from_host"));
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), Instr::FromHost);
    let val = cb.alloc(l!("val"));
    let zero = cb.zero();
    let one = cb.one();
    cb.memory(MemoryTable::Stack, sp.clone(), zero.clone(), val.clone());
    cb.lookup(LookupTable::HostInteractions, cc.clone(), val);
    assert(cb.nest(l!()), new_sp, sp + one.clone());
    assert(cb.nest(l!()), new_helper_sp, helper_sp);
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), new_cc, cc + one);
}

trait BinOp {
    fn loc(&self) -> Location;
    fn instr(&self) -> Instr;
    fn perform<Var: CircuitBuilderVar, B: Builder<Var>>(
        &self,
        cb: B,
        first: Var,
        second: Var,
    ) -> Var;
}

trait UnOp {
    fn loc(&self) -> Location;
    fn instr(&self) -> Instr;
    fn perform<Var: CircuitBuilderVar, B: Builder<Var>>(&self, cb: B, arg: Var) -> Var;
}

fn alloc_int<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, n_bits: u32) -> Var {
    let zero = cb.zero();
    let one = cb.one();
    // TODO: implement this with lookups instead
    (0..n_bits)
        .map(|i| {
            let bit = cb.alloc(l!("bit"));
            cb.enforce(l!(), bit.clone(), one.clone() - bit.clone(), zero.clone());
            bit * 2i128.pow(i)
        })
        .fold(zero.clone(), |acc, power| acc + power)
}

fn visit_bin_op<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, bin_op: impl BinOp) {
    let mut cb = cb.nest(l!("visit_bin_op"));
    let mut cb = cb.nest(bin_op.loc());
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), bin_op.instr());
    let first = cb.alloc(l!("first"));
    let second = cb.alloc(l!("second"));
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    let result = bin_op.perform(cb.nest(l!()), first.clone(), second.clone());
    cb.memory(
        MemoryTable::Stack,
        sp.clone() - one.clone(),
        second.clone(),
        zero.clone(),
    );
    cb.memory(MemoryTable::Stack, sp.clone() - two, first.clone(), result);
    assert(cb.nest(l!()), new_sp, sp - one.clone());
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

fn visit_un_op<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, un_op: impl UnOp) {
    let mut cb = cb.nest(l!("visit_un_op"));
    let mut cb = cb.nest(un_op.loc());
    let WASMGlobal {
        sp,
        pc,
        helper_sp,
        reg,
        cc,
        new_sp,
        new_pc,
        new_helper_sp,
        new_reg,
        new_cc,
    }: WASMGlobal<Var> = globals(cb.nest(l!()), un_op.instr());
    let arg = cb.alloc(l!("arg"));
    let one = cb.one();
    let two = cb.lit(2);
    let result = un_op.perform(cb.nest(l!()), arg.clone());
    cb.memory(MemoryTable::Stack, sp.clone() - two, arg.clone(), result);
    assert(cb.nest(l!()), new_sp, sp - one.clone());
    assert(cb.nest(l!()), new_pc, pc + one.clone());
    assert(cb.nest(l!()), helper_sp, new_helper_sp);
    assert(cb.nest(l!()), reg, new_reg);
    assert(cb.nest(l!()), cc, new_cc);
}

struct OpAdd;
impl BinOp for OpAdd {
    fn loc(&self) -> Location {
        l!("OpAdd")
    }
    fn instr(&self) -> Instr {
        Instr::Add
    }
    fn perform<Var: CircuitBuilderVar, B: Builder<Var>>(
        &self,
        mut cb: B,
        first: Var,
        second: Var,
    ) -> Var {
        let bit_rep = alloc_int(cb.nest(l!()), 32);
        let carry = alloc_int(cb.nest(l!()), 1);
        let whole_result = bit_rep.clone() + carry * 2i128.pow(32);
        let one = cb.one();
        cb.enforce(l!(), one, first + second, whole_result);
        bit_rep
    }
}

struct OpMul;
impl BinOp for OpMul {
    fn loc(&self) -> Location {
        l!("OpMul")
    }
    fn instr(&self) -> Instr {
        Instr::Mul
    }
    fn perform<Var: CircuitBuilderVar, B: Builder<Var>>(
        &self,
        mut cb: B,
        first: Var,
        second: Var,
    ) -> Var {
        let lower_half = alloc_int(cb.nest(l!()), 32);
        let upper_half = alloc_int(cb.nest(l!()), 32);
        let whole_result = lower_half.clone() + upper_half * 2i128.pow(32);
        cb.enforce(l!(), first, second, whole_result);
        lower_half
    }
}

struct OpNegNonZero;
impl UnOp for OpNegNonZero {
    fn loc(&self) -> Location {
        l!("OpNegNonZero")
    }
    fn instr(&self) -> Instr {
        Instr::Neg
    }
    fn perform<Var: CircuitBuilderVar, B: Builder<Var>>(&self, mut cb: B, arg: Var) -> Var {
        assert_non_zero(cb.nest(l!()), arg.clone());
        cb.lit(1i128.pow(32)) - arg
    }
}

struct OpNegZero;
impl UnOp for OpNegZero {
    fn loc(&self) -> Location {
        l!("OpNegZero")
    }
    fn instr(&self) -> Instr {
        Instr::Neg
    }
    fn perform<Var: CircuitBuilderVar, B: Builder<Var>>(&self, mut cb: B, arg: Var) -> Var {
        let zero = cb.zero();
        let one = cb.one();
        cb.enforce(l!(), arg, one, zero.clone());
        zero
    }
}

struct OpNotNonZero;
impl UnOp for OpNotNonZero {
    fn loc(&self) -> Location {
        l!("OpNotNonZero")
    }
    fn instr(&self) -> Instr {
        Instr::Not
    }
    fn perform<Var: CircuitBuilderVar, B: Builder<Var>>(&self, mut cb: B, arg: Var) -> Var {
        assert_non_zero(cb.nest(l!()), arg.clone());
        cb.zero()
    }
}

struct OpNotZero;
impl UnOp for OpNotZero {
    fn loc(&self) -> Location {
        l!("OpNotZero")
    }
    fn instr(&self) -> Instr {
        Instr::Not
    }
    fn perform<Var: CircuitBuilderVar, B: Builder<Var>>(&self, mut cb: B, arg: Var) -> Var {
        let zero = cb.zero();
        let one = cb.one();
        cb.enforce(l!(), arg, one.clone(), zero.clone());
        one
    }
}

#[derive(PartialEq)]
pub enum Branch {
    Nop,
    Drop,
    Const,
    OpAdd,
    OpMul,
    OpNegNonZero,
    OpNegZero,
    OpNotNonZero,
    OpNotZero,
    Get,
    Set,
    Swap,
    CondJumpCNot0,
    CondJumpC0,
    Jump,
    Read,
    Write,
    Alloc,
    SelectCNot0,
    SelectC0,
    GetReg,
    SetReg,
    ToHelper,
    FromHelper,
    InitHostCall,
    ToHost,
    FromHost,
}

impl BranchedCircuit<Branch, WASM_IO, LookupTable, MemoryTable> for WASM_VM {
    fn branches(&self) -> impl Iterator<Item = Branch> {
        [
            Branch::Nop,
            Branch::Drop,
            Branch::Const,
            Branch::OpAdd,
            Branch::OpMul,
            Branch::OpNegNonZero,
            Branch::OpNegZero,
            Branch::OpNotNonZero,
            Branch::OpNotZero,
            Branch::Get,
            Branch::Set,
            Branch::Swap,
            Branch::CondJumpCNot0,
            Branch::CondJumpC0,
            Branch::Jump,
            Branch::Read,
            Branch::Write,
            Branch::Alloc,
            Branch::SelectCNot0,
            Branch::SelectC0,
            Branch::GetReg,
            Branch::SetReg,
            Branch::ToHelper,
            Branch::FromHelper,
            Branch::InitHostCall,
            Branch::ToHost,
            Branch::FromHost,
        ]
        .into_iter()
    }
    fn io(&self) -> impl Iterator<Item = WASM_IO> {
        [
            WASM_IO::sp,
            WASM_IO::pc,
            WASM_IO::helper_sp,
            WASM_IO::reg,
            WASM_IO::cc,
        ]
        .into_iter()
    }
    // rustc bug
    #[allow(private_bounds)]
    fn run<Var: CircuitBuilderVar, B: Builder<Var>>(&self, branch: Branch, mut cb: B) {
        match branch {
            Branch::Nop => visit_nop(cb.nest(l!())),
            Branch::Drop => visit_drop(cb.nest(l!())),
            Branch::Const => visit_const(cb.nest(l!())),
            Branch::OpAdd => visit_bin_op(cb.nest(l!()), OpAdd),
            Branch::OpMul => visit_bin_op(cb.nest(l!()), OpMul),
            Branch::OpNegNonZero => visit_un_op(cb.nest(l!()), OpNegNonZero),
            Branch::OpNegZero => visit_un_op(cb.nest(l!()), OpNegZero),
            Branch::OpNotNonZero => visit_un_op(cb.nest(l!()), OpNotNonZero),
            Branch::OpNotZero => visit_un_op(cb.nest(l!()), OpNotZero),
            Branch::Get => visit_get(cb.nest(l!())),
            Branch::Set => visit_set(cb.nest(l!())),
            Branch::Swap => visit_swap(cb.nest(l!())),
            Branch::CondJumpCNot0 => visit_cond_jump_c_not_0(cb.nest(l!())),
            Branch::CondJumpC0 => visit_cond_jump_c_0(cb.nest(l!())),
            Branch::Jump => visit_jump(cb.nest(l!())),
            Branch::Read => visit_read(cb.nest(l!())),
            Branch::Write => visit_write(cb.nest(l!())),
            Branch::Alloc => visit_alloc(cb.nest(l!())),
            Branch::SelectCNot0 => visit_select_c_not_0(cb.nest(l!())),
            Branch::SelectC0 => visit_select_c_0(cb.nest(l!())),
            Branch::SetReg => visit_set_reg(cb.nest(l!())),
            Branch::GetReg => visit_get_reg(cb.nest(l!())),
            Branch::ToHelper => visit_to_helper(cb.nest(l!())),
            Branch::FromHelper => visit_from_helper(cb.nest(l!())),
            Branch::InitHostCall => visit_init_host_call(cb.nest(l!())),
            Branch::ToHost => visit_to_host(cb.nest(l!())),
            Branch::FromHost => visit_from_host(cb.nest(l!())),
        }
    }
}
