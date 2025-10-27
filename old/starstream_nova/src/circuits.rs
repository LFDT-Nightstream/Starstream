use crate::interface::{Circuit, CircuitBuilder, CircuitBuilderVar};
use crate::l;

pub enum Memories {
    Stack,
    Memory,
    HelperStack,
}

pub enum LookupTables {
    Code,
    HostInteractions,
}

#[allow(non_camel_case_types)]
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

trait Builder<Var: CircuitBuilderVar>: CircuitBuilder<Var, LookupTables, Memories> {}
impl<Var: CircuitBuilderVar, B: CircuitBuilder<Var, LookupTables, Memories>> Builder<Var> for B {}

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
    fn alloc(&mut self, mut cb: impl Builder<Var>) -> Switch<Var> {
        let switch = cb.alloc(l!("switch"));
        self.0.push(switch.clone());
        Switch { switch }
    }
    #[must_use]
    fn consume(self, _marker: ConsumeSwitches, mut cb: impl Builder<Var>) -> Switch<Var> {
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

fn cond_assert<Var: CircuitBuilderVar, B: Builder<Var>>(
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
// TODO: maybe add a built-in way in interfaces to "zero" a variable when switch is off
fn cond_memory<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    namespace: Memories,
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

fn cond_memory_to_zero<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    namespace: Memories,
    addr: Var,
    old: Var,
) {
    let real_addr = cb.alloc(l!("real_addr"));
    let real_old = cb.alloc(l!("real_old"));
    cb.enforce(l!(), switch.switch.clone(), addr.clone(), real_addr.clone());
    cb.enforce(l!(), switch.switch.clone(), old.clone(), real_old.clone());
    let zero = cb.zero();
    cb.memory(namespace, real_addr, real_old, zero);
}

// FIXME: return real_new
fn cond_memory_zero_to<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    namespace: Memories,
    addr: Var,
    new: Var,
) {
    let real_addr = cb.alloc(l!("real_addr"));
    let real_new = cb.alloc(l!("real_new"));
    cb.enforce(l!(), switch.switch.clone(), addr.clone(), real_addr.clone());
    cb.enforce(l!(), switch.switch.clone(), new.clone(), real_new.clone());
    let zero = cb.zero();
    cb.memory(namespace, real_addr, zero, real_new);
}

fn cond_memory_zero<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    namespace: Memories,
    addr: Var,
) {
    let real_addr = cb.alloc(l!("real_addr"));
    cb.enforce(l!(), switch.switch.clone(), addr.clone(), real_addr.clone());
    let zero = cb.zero();
    cb.memory(namespace, real_addr, zero.clone(), zero);
}

fn cond_lookup<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    namespace: LookupTables,
    addr: Var,
    val: Var,
) {
    let real_addr = cb.alloc(l!("real_addr"));
    let real_val = cb.alloc(l!("real_val"));
    cb.enforce(l!(), switch.switch.clone(), addr.clone(), real_addr.clone());
    cb.enforce(l!(), switch.switch.clone(), val.clone(), real_val.clone());
    cb.lookup(namespace, real_addr, real_val);
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
    /// <TODO>
    InitHostCall = 18,
    /// <TODO>
    ToHost = 19,
    /// <TODO>
    FromHost = 20,
    // not an instruction, signifies end of enum
    _End = 21,
}

pub const N_GLOBAL_WITNESSES: usize = 2;
pub const WITNESS_OFFSET_NOP: usize = N_GLOBAL_WITNESSES;
pub const WITNESS_OFFSET_DROP: usize = WITNESS_OFFSET_NOP + 1;
pub const WITNESS_OFFSET_CONST: usize = WITNESS_OFFSET_DROP + 4;
pub const WITNESS_OFFSET_ADD: usize = WITNESS_OFFSET_CONST + 3;
pub const WITNESS_OFFSET_GET: usize = WITNESS_OFFSET_ADD + 41;
pub const WITNESS_OFFSET_SET: usize = WITNESS_OFFSET_GET + 7;
pub const WITNESS_OFFSET_SWAP: usize = WITNESS_OFFSET_SET + 8;
pub const WITNESS_OFFSET_COND_JUMP_C_NOT_0: usize = WITNESS_OFFSET_SWAP + 9;
pub const WITNESS_OFFSET_COND_JUMP_C_0: usize = WITNESS_OFFSET_COND_JUMP_C_NOT_0 + 8;
pub const WITNESS_OFFSET_JUMP: usize = WITNESS_OFFSET_COND_JUMP_C_0 + 4;
pub const WITNESS_OFFSET_READ: usize = WITNESS_OFFSET_JUMP + 3;
pub const WITNESS_OFFSET_WRITE: usize = WITNESS_OFFSET_READ + 9;
pub const WITNESS_OFFSET_ALLOC: usize = WITNESS_OFFSET_WRITE + 11;
pub const WITNESS_OFFSET_SELECT_C_NOT_0: usize = WITNESS_OFFSET_ALLOC + 1;
pub const WITNESS_OFFSET_SELECT_C_0: usize = WITNESS_OFFSET_SELECT_C_NOT_0 + 8;
pub const WITNESS_OFFSET_SET_REG: usize = WITNESS_OFFSET_SELECT_C_0 + 9;
pub const WITNESS_OFFSET_GET_REG: usize = WITNESS_OFFSET_SET_REG + 4;
pub const WITNESS_OFFSET_TO_HELPER: usize = WITNESS_OFFSET_GET_REG + 4;
pub const WITNESS_OFFSET_FROM_HELPER: usize = WITNESS_OFFSET_TO_HELPER + 6;
pub const WITNESS_OFFSET_INIT_HOST_CALL: usize = WITNESS_OFFSET_FROM_HELPER + 6;
pub const WITNESS_OFFSET_TO_HOST: usize = WITNESS_OFFSET_INIT_HOST_CALL + 3;
pub const WITNESS_OFFSET_FROM_HOST: usize = WITNESS_OFFSET_TO_HOST + 6;
pub const WITNESS_OFFSET_EMPTY_STEP: usize = WITNESS_OFFSET_FROM_HOST + 6;
pub const WITNESS_OFFSET_OPCODE_LOOKUP: usize = WITNESS_OFFSET_EMPTY_STEP;
pub const N_TOTAL_WITNESSES: usize = WITNESS_OFFSET_OPCODE_LOOKUP + 2;

#[derive(Clone)]
struct WASMGlobal<Var> {
    // The variable containing the opcode, shared for convenience.
    opcode: Var,
    // The parameter to the instruction, or the opcode of the next instruction.
    // Only used in the former case.
    param: Var,
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

fn visit_nop<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_nop"));
    cb.assert_offset(WITNESS_OFFSET_NOP);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Nop as i128);
    let one = cb.one();
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_drop<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_drop"));
    cb.assert_offset(WITNESS_OFFSET_DROP);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Drop as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let prev = cb.alloc(l!("prev"));
    let one = cb.one();
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        prev,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_const<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: constant,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_const"));
    cb.assert_offset(WITNESS_OFFSET_CONST);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Const as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    let two = cb.lit(2);
    cond_memory_zero_to(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone(),
        constant,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + two);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_add<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
        cb.enforce(l!(), last_bit.clone(), one.clone() - last_bit.clone(), zero);
        result_mod_2_32.clone() + last_bit * 2i128.pow(32)
    };
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        first.clone() + second.clone(),
        result.clone(),
    );
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        first.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - two,
        second.clone(),
        result_mod_2_32,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_get<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: index,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_get"));
    cb.assert_offset(WITNESS_OFFSET_GET);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Get as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let value = cb.alloc(l!("value"));
    let one = cb.one();
    let two = cb.lit(2);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - index,
        value.clone(),
        value.clone(),
    );
    cond_memory_zero_to(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone(),
        value,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + two);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_set<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: index,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_set"));
    cb.assert_offset(WITNESS_OFFSET_SET);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Set as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let old = cb.alloc(l!("old"));
    let new = cb.alloc(l!("new"));
    let one = cb.one();
    let two = cb.lit(2);
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        new.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - index,
        old.clone(),
        new.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + two);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_swap<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: index,
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
    let two = cb.lit(2);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one,
        x.clone(),
        y.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - index,
        y.clone(),
        x.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + two);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_cond_jump_c_not_0<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_cond_jump_c_not_0"));
    cb.assert_offset(WITNESS_OFFSET_COND_JUMP_C_NOT_0);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::CondJump as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    let unused_pc = cb.alloc(l!("unused_pc"));
    let two = cb.lit(2);
    let c = cb.alloc(l!("c"));
    let c_inv = cb.alloc(l!("c_inv"));
    cb.enforce(l!(), c.clone(), c_inv, switch.switch.clone());
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - two.clone(),
        c.clone(),
    );
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        unused_pc,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_cond_jump_c_0<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_cond_jump_c_0"));
    cb.assert_offset(WITNESS_OFFSET_COND_JUMP_C_0);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::CondJump as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    let two = cb.lit(2);
    cond_memory_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - two.clone(),
    );
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one,
        new_pc,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_jump<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_jump"));
    cb.assert_offset(WITNESS_OFFSET_JUMP);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Jump as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        new_pc,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_read<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
        Memories::Memory,
        address.clone(),
        value.clone(),
        value.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        address.clone(),
        value.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_write<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    let one = cb.one();
    let two = cb.lit(2);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        Memories::Memory,
        address.clone(),
        old.clone(),
        new.clone(),
    );
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        new.clone(),
    );
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - two.clone(),
        address.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_alloc<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: count,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_alloc"));
    cb.assert_offset(WITNESS_OFFSET_ALLOC);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Alloc as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let two = cb.lit(2);
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + count);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + two);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_select_c_not_0<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_select_c_not_0"));
    cb.assert_offset(WITNESS_OFFSET_SELECT_C_NOT_0);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Select as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    let two = cb.lit(2);
    let c = cb.alloc(l!("c"));
    let c_inv = cb.alloc(l!("c_inv"));
    let y = cb.alloc(l!("y"));
    cb.enforce(l!(), c.clone(), c_inv, switch.switch.clone());
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - two.clone(),
        y,
    );
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        c,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_select_c_0<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_select_c_0"));
    cb.assert_offset(WITNESS_OFFSET_SELECT_C_0);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Select as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    let two = cb.lit(2);
    let three = cb.lit(3);
    let x = cb.alloc(l!("x"));
    let y = cb.alloc(l!("y"));
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - three.clone(),
        x.clone(),
        y.clone(),
    );
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - two.clone(),
        y,
    );
    cond_memory_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_set_reg<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_set_reg"));
    cb.assert_offset(WITNESS_OFFSET_SET_REG);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::SetReg as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    let val = cb.alloc(l!("val"));
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        val.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(cb.nest(l!()), switch.clone(), new_reg, val);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_get_reg<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_get_reg"));
    cb.assert_offset(WITNESS_OFFSET_GET_REG);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::GetReg as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let one = cb.one();
    let val = cb.alloc(l!("val"));
    cond_memory_zero_to(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone(),
        val.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(cb.nest(l!()), switch.clone(), reg.clone(), val);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), helper_sp, new_helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_to_helper<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_to_helper"));
    cb.assert_offset(WITNESS_OFFSET_TO_HELPER);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::ToHelper as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let val = cb.alloc(l!("val"));
    let one = cb.one();
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        val.clone(),
    );
    cond_memory_zero_to(
        cb.nest(l!()),
        switch.clone(),
        Memories::HelperStack,
        helper_sp.clone(),
        val,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_helper_sp,
        helper_sp + one.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_from_helper<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_from_helper"));
    cb.assert_offset(WITNESS_OFFSET_FROM_HELPER);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::FromHelper as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let val = cb.alloc(l!("val"));
    let one = cb.one();
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::HelperStack,
        helper_sp.clone() - one.clone(),
        val.clone(),
    );
    cond_memory_zero_to(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone(),
        val,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_helper_sp,
        helper_sp - one.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), cc, new_cc);
}

fn visit_init_host_call<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: host_call,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_init_host_call"));
    cb.assert_offset(WITNESS_OFFSET_INIT_HOST_CALL);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::InitHostCall as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    cond_lookup(
        cb.nest(l!()),
        switch.clone(),
        LookupTables::HostInteractions,
        cc.clone(),
        host_call,
    );
    let one = cb.one();
    let two = cb.lit(2);
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_helper_sp, helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + two);
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), new_cc, cc + one);
}

fn visit_to_host<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_to_host"));
    cb.assert_offset(WITNESS_OFFSET_TO_HOST);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::ToHost as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let val = cb.alloc(l!("val"));
    let one = cb.one();
    cond_memory_to_zero(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone() - one.clone(),
        val.clone(),
    );
    cond_lookup(
        cb.nest(l!()),
        switch.clone(),
        LookupTables::HostInteractions,
        cc.clone(),
        val,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_helper_sp, helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), new_cc, cc + one);
}

fn visit_from_host<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_from_host"));
    cb.assert_offset(WITNESS_OFFSET_FROM_HOST);
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::FromHost as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let val = cb.alloc(l!("val"));
    let one = cb.one();
    cond_memory_zero_to(
        cb.nest(l!()),
        switch.clone(),
        Memories::Stack,
        sp.clone(),
        val.clone(),
    );
    cond_lookup(
        cb.nest(l!()),
        switch.clone(),
        LookupTables::HostInteractions,
        cc.clone(),
        val,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_helper_sp, helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), reg, new_reg);
    cond_assert(cb.nest(l!()), switch.clone(), new_cc, cc + one);
}

fn visit_empty_step<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    WASMGlobal {
        opcode: _,
        param: _,
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
    }: WASMGlobal<Var>,
) {
    let mut cb = cb.nest(l!("visit_empty_step"));
    // NB: We must allocate no new variables in visit_empty_step
    cb.assert_offset(WITNESS_OFFSET_EMPTY_STEP);
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc);
    cond_assert(cb.nest(l!()), switch.clone(), new_reg, reg);
    cond_assert(cb.nest(l!()), switch.clone(), new_helper_sp, helper_sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_cc, cc);
    // Verifies that no variables have been allocated
    cb.assert_offset(WITNESS_OFFSET_EMPTY_STEP);
}

impl Circuit<WASM_IO, LookupTables, Memories> for WASM_VM {
    fn run<Var: CircuitBuilderVar, B: Builder<Var>>(
        &self,
        mut cb: B,
        input: impl Fn(WASM_IO) -> Var,
        output: impl Fn(WASM_IO) -> Var,
    ) {
        let (mut switches, consume_switches) = Switches::new();
        let sp = input(WASM_IO::sp);
        let pc = input(WASM_IO::pc);
        let helper_sp = input(WASM_IO::helper_sp);
        let reg = input(WASM_IO::reg);
        let cc = input(WASM_IO::cc);
        let new_sp = output(WASM_IO::sp);
        let new_pc = output(WASM_IO::pc);
        let new_helper_sp = output(WASM_IO::helper_sp);
        let new_reg = output(WASM_IO::reg);
        let new_cc = output(WASM_IO::cc);
        let opcode = cb.alloc(l!("opcode"));
        let param = cb.alloc(l!("param"));
        let global = WASMGlobal {
            opcode,
            param,
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
        visit_set_reg(cb.nest(l!()), &mut switches, global.clone());
        visit_get_reg(cb.nest(l!()), &mut switches, global.clone());
        visit_to_helper(cb.nest(l!()), &mut switches, global.clone());
        visit_from_helper(cb.nest(l!()), &mut switches, global.clone());
        visit_init_host_call(cb.nest(l!()), &mut switches, global.clone());
        visit_to_host(cb.nest(l!()), &mut switches, global.clone());
        visit_from_host(cb.nest(l!()), &mut switches, global.clone());
        let is_empty_step = switches.consume(consume_switches, cb.nest(l!()));
        visit_empty_step(cb.nest(l!()), is_empty_step.clone(), global.clone());
        {
            let mut cb = cb.nest(l!("opcode_lookup"));
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
            cb.lookup(LookupTables::Code, real_opcode_idx, global.opcode.clone());
            cb.lookup(LookupTables::Code, real_param_idx, global.param.clone());
            cb.assert_offset(N_TOTAL_WITNESSES);
        }
    }
}
