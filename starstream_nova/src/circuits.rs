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
    fn consume(self, _marker: ConsumeSwitches, mut cb: impl CircuitBuilder<Var>) {
        let zero = cb.zero();
        let one = cb.one();
        cb.enforce(
            l!("only one switch must be on"),
            self.0
                .iter()
                .fold(zero.clone(), |acc, switch| acc + switch.clone()),
            one.clone(),
            one.clone(),
        );

        for switch in &self.0 {
            cb.enforce(
                l!("switch must be binary"),
                switch.clone(),
                one.clone() - switch.clone(),
                zero.clone(),
            );
        }

        std::mem::forget(self)
    }
}

fn cond_assert<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    a: Var,
    b: Var,
) {
    let maybe_b = cb.alloc(l!("maybe_b"));
    cb.enforce(l!(), switch.switch.clone(), b, maybe_b.clone());
    cb.enforce(l!(), switch.switch, a, maybe_b);
}

// NB: this requires that the 0th index of the memory is always zero,
// i.e. that *nullptr = 0
// FIXME: this could be done more efficiently probably
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
    cb.memory(namespace, addr, old, new);
}

/*
fn if_switch<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(mut cb: B, switch: Var, n: Var) -> Var {
    let r = cb.alloc(l!("if_result"));
    cb.enforce(l!("if_switch"), switch, n, r.clone());
    r
}

fn either_switch<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switch: Switch<Var>,
    case_true: Var,
    case_false: Var,
) -> Var {
    let zero = cb.zero();
    let one = cb.one();
    let r_true = cb.alloc(l!("either_switch_true"));
    cb.enforce(
        l!("either_switch.case_true"),
        switch.switch.clone(),
        case_true.clone(),
        r_true.clone(),
    );
    let r_false = cb.alloc(l!("either_switch_false"));
    cb.enforce(
        l!("either_switch.case_true"),
        one.clone() - switch.switch.clone(),
        case_false.clone(),
        r_false.clone(),
    );
    cb.enforce(
        l!("either_switch.combine"),
        r_true.clone(),
        r_false.clone(),
        zero.clone(),
    );
    let r = cb.alloc(l!("either_switch_r"));
    cb.enforce(
        l!("either_switch.either"),
        r_true.clone() + r_false.clone(),
        one.clone(),
        r.clone(),
    );
    r
}
*/

/*
// FIXME: we shouldn't use hashing
// FIXME: this just adds together all elements
fn hash<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(mut cb: B, switch: Var, input: Vec<Var>) -> Var {
    let mut cb = cb.nest(l!("hash"));
    let out = cb.alloc(l!("out"));
    // FIXME: poseidon_hash_allocated doesn't take a switch,
    // so all its constraints cost even if they aren't used.
    let mut input: Vec<AllocatedNum<B::F>> = input
        .into_iter()
        .map(|var| cb.to_allocated_num(var))
        .collect();
    // FIXME: HORRIBLE!
    // poseidon_hash_allocated is buggy I assume,
    // because it can't handle larger inputs,
    // or it expects us to pass in a `U` that fits
    // exactly, but that seems unlikely, and not really possible
    // to abstract over because of the private trait constraint.
    input.truncate(2);
    let constants = PoseidonConstants::<B::F, U2>::new();
    let hash: AllocatedNum<B::F> =
        poseidon_hash_allocated(cb.nest(l!()).inner(), input, &constants)
            .expect("poseidon_hash_allocated failed");
    let hash = cb.from_allocated_num(hash);
    if_switch(cb.nest(l!("switch_hash")), switch, hash)
}

// adds H(a, v, t) to the multiset
fn memory<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switch: Var,
    multiset: Var,
    a: Var,
    v: Var,
    t: Var,
) -> Var {
    let preimage = vec![a, v, t];
    let hash = hash(cb.nest(l!()), switch, preimage);
    multiset + hash
}

fn push<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switch: Var,
    rs: &mut Var,
    ws: &mut Var,
    idx: Var,
    data: &[Var],
) {
    let prev = cb.alloc(l!("prev"));
    let timestamp = cb.alloc(l!("timestamp"));
    let new_timestamp = cb.alloc(l!("new_timestamp"));
    cb.enforce(
        l!(),
        new_timestamp.clone() - timestamp.clone(),
        switch.clone(),
        switch.clone(),
    );
    let zero = cb.zero();
    let one = cb.one();
    cb.enforce(l!(), zero.clone(), one, zero.clone());
    let preimage = {
        let mut v = Vec::from(data);
        v.push(prev.clone());
        v
    };
    let updated = hash(cb.nest(l!()), switch.clone(), preimage);
    *rs = memory(
        cb.nest(l!()),
        switch.clone(),
        rs.clone(),
        idx.clone(),
        prev,
        timestamp,
    );
    *ws = memory(
        cb.nest(l!()),
        switch,
        ws.clone(),
        idx,
        updated,
        new_timestamp,
    );
}

struct StarstreamIO<Var> {
    rs: Var,
    ws: Var,
    rs_coord: Var,
    ws_coord: Var,
    coord_idx: Var,
    coord_stack: Var,
}

impl<Var> StarstreamIO<Var> {
    fn of(fields: Vec<Var>) -> StarstreamIO<Var> {
        let Ok([rs, ws, rs_coord, ws_coord, coord_idx, coord_stack]): Result<[Var; 6], _> =
            fields.try_into()
        else {
            unreachable!("wrong number of elements in io");
        };
        StarstreamIO {
            rs,
            ws,
            rs_coord,
            ws_coord,
            coord_idx,
            coord_stack,
        }
    }
    fn to(self) -> Vec<Var> {
        let StarstreamIO {
            rs,
            ws,
            rs_coord,
            ws_coord,
            coord_idx,
            coord_stack,
        } = self;
        vec![rs, ws, rs_coord, ws_coord, coord_idx, coord_stack]
    }
}

fn visit_enter<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    io: &mut StarstreamIO<Var>,
) {
    let switch = switches.alloc(cb.nest(l!()));
    let utxo_idx = cb.alloc(l!("utxo_idx"));
    let input = cb.alloc(l!("input"));
    push(
        cb.nest(l!()),
        switch.switch.clone(),
        &mut io.rs,
        &mut io.ws,
        utxo_idx.clone(),
        &[input.clone()],
    );
    let zero = cb.zero();
    push(
        cb.nest(l!()),
        switch.switch,
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        &[zero, utxo_idx, input],
    );
}

fn visit_push_coord<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    io: &mut StarstreamIO<Var>,
) {
    let switch = switches.alloc(cb.nest(l!()));
    let new_coord_idx = cb.alloc(l!("new_coord_idx"));
    let input = cb.alloc(l!("input"));
    let one = cb.one();
    push(
        cb.nest(l!()),
        switch.switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        // FIXME: use hash instead of idx
        &[one, new_coord_idx.clone(), input.clone()],
    );
    let two = cb.lit(2);
    push(
        cb.nest(l!()),
        switch.switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        new_coord_idx.clone(),
        // FIXME: use hash instead of idx
        &[two, io.coord_idx.clone(), input],
    );
    let preimage = vec![io.coord_idx.clone(), io.coord_stack.clone()];
    let new_coord_stack = hash(cb.nest(l!()), switch.switch.clone(), preimage);
    io.coord_idx = either_switch(
        cb.nest(l!()),
        switch.clone(),
        new_coord_idx,
        io.coord_idx.clone(),
    );
    io.coord_stack = either_switch(
        cb.nest(l!()),
        switch,
        new_coord_stack,
        io.coord_stack.clone(),
    );
}

fn visit_pop_coord<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    io: &mut StarstreamIO<Var>,
) {
    let switch = switches.alloc(cb.nest(l!()));
    let old_coord_idx = cb.alloc(l!("old_coord_idx"));
    let old_coord_stack = cb.alloc(l!("old_coord_stack"));
    let three = cb.lit(3);
    push(
        cb.nest(l!()),
        switch.switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        &[three],
    );
    let preimage = vec![old_coord_idx.clone(), old_coord_stack.clone()];
    let should_be = hash(cb.nest(l!()), switch.switch.clone(), preimage);
    cb.enforce(
        l!(),
        should_be.clone(),
        switch.switch.clone(),
        io.coord_stack.clone(),
    );
    io.coord_idx = either_switch(
        cb.nest(l!()),
        switch.clone(),
        old_coord_idx,
        io.coord_idx.clone(),
    );
    io.coord_stack = either_switch(
        cb.nest(l!()),
        switch,
        old_coord_stack,
        io.coord_stack.clone(),
    );
}

fn visit_nop<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(mut cb: B, switches: &mut Switches<Var>) {
    let _switch = switches.alloc(cb.nest(l!()));
}

pub struct StarstreamCircuit;

impl Circuit for StarstreamCircuit {
    fn run<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(&self, mut cb: B, io: Vec<Var>) -> Vec<Var> {
        let (mut switches, consume_switches) = Switches::new();

        let mut io = StarstreamIO::of(io);

        visit_enter(cb.nest(l!()), &mut switches, &mut io);
        visit_push_coord(cb.nest(l!()), &mut switches, &mut io);
        visit_pop_coord(cb.nest(l!()), &mut switches, &mut io);
        visit_nop(cb.nest(l!()), &mut switches);

        switches.consume(consume_switches, cb.nest(l!()));

        io.to()
    }
}

// NB: this is a really dumb "vm", which is just R1CS in R1CS basically.
// Even if we were to adopt this approach, it would probably be more
// efficient to use Spartan to reduce the amount of work necessary
// rather than naively embedding R1CS in R1CS.
#[allow(non_camel_case_types)]
pub struct R1CS_DUMMY_VM;

const R1CS_DUMMY_WITNESS_COUNT: usize = 254;
const R1CS_DUMMY_CONSTRAINT_COUNT: usize = 256;

fn mul<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(mut cb: B, x: Var, y: Var) -> Var {
    let r = cb.alloc(l!("mul"));
    cb.enforce(l!("mul"), x, y, r.clone());
    r
}

impl Circuit for R1CS_DUMMY_VM {
    fn run<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(&self, mut cb: B, io: Vec<Var>) -> Vec<Var> {
        let [scripts, ios] = io.try_into().expect("arity wrong");
        let io = cb.alloc(l!("io"));
        let witnesses: Vec<_> = (0..R1CS_DUMMY_WITNESS_COUNT)
            .map(|_| cb.alloc(l!("witness")))
            .collect();
        let constraints_a: Vec<_> = (0..(R1CS_DUMMY_CONSTRAINT_COUNT
            * (R1CS_DUMMY_WITNESS_COUNT + 2)))
            .map(|_| cb.alloc(l!("factor_a")))
            .collect();
        let constraints_b: Vec<_> = (0..(R1CS_DUMMY_CONSTRAINT_COUNT
            * (R1CS_DUMMY_WITNESS_COUNT + 2)))
            .map(|_| cb.alloc(l!("factor_b")))
            .collect();
        let constraints_c: Vec<_> = (0..(R1CS_DUMMY_CONSTRAINT_COUNT
            * (R1CS_DUMMY_WITNESS_COUNT + 2)))
            .map(|_| cb.alloc(l!("factor_c")))
            .collect();
        let one = cb.one();
        for i in 0..R1CS_DUMMY_CONSTRAINT_COUNT {
            let a = constraints_a[i].clone();
            let a = a + mul(
                cb.nest(l!("io_a")),
                io.clone(),
                constraints_a[i + 1].clone(),
            );
            let a = (0..R1CS_DUMMY_WITNESS_COUNT).fold(a, |a, j| {
                a + mul(
                    cb.nest(l!("witness_factor_a_mul")),
                    witnesses[j].clone(),
                    constraints_a[i + j + 2].clone(),
                )
            });
            let b = constraints_b[i].clone();
            let b = b + mul(
                cb.nest(l!("io_b")),
                io.clone(),
                constraints_b[i + 1].clone(),
            );
            let b = (0..R1CS_DUMMY_WITNESS_COUNT).fold(b, |b, j| {
                b + mul(
                    cb.nest(l!("witness_factor_b_mul")),
                    witnesses[j].clone(),
                    constraints_b[i + j + 2].clone(),
                )
            });
            let c = constraints_c[i].clone();
            let c = c + mul(
                cb.nest(l!("io_c")),
                io.clone(),
                constraints_c[i + 1].clone(),
            );
            let c = (0..R1CS_DUMMY_WITNESS_COUNT).fold(c, |c, j| {
                c + mul(
                    cb.nest(l!("witness_factor_c_mul")),
                    witnesses[j].clone(),
                    constraints_c[i + j + 2].clone(),
                )
            });
            cb.enforce(l!(), a, b, c);
        }
        let script_hash = hash(
            cb.nest(l!()),
            one.clone(),
            constraints_a
                .into_iter()
                .chain(constraints_b)
                .chain(constraints_c)
                .collect(),
        );
        let scripts = hash(cb.nest(l!()), one, vec![script_hash, scripts]);
        vec![scripts, ios]
    }
}
*/

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
 *
 * TODO: use struct to carry around "global" vars (opcode, sp, pc, new_sp, new_pc)
 *
 * FIXME: incorrect handling of sp
 */
#[allow(non_camel_case_types)]
pub struct WASM_VM;

#[repr(u32)]
pub enum Instr {
    // not provable
    Unreachable = 0,
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
    // x -> stack[sp - code[pc+1]] := x
    // NB: code[pc+1] must be greater than 1
    Set,
    // b new_pc_namespace new_pc -> {}; pc := new_pc, pc_namespace := new_pc_namespace if b == 1, else if b == 0 then pc := pc + 1
    CondJump,
    // new_pc_namespace new_pc -> {}; pc := new_pc, pc_namespace := new_pc_namespace
    // FIXME: implement
    Jump,
    // ptr -> memory[ptr]
    Read,
    // ptr data -> {}; memory[ptr] = data
    Write,
    // {} -> code[pc+1] zeroes
    Alloc,
}

#[allow(non_camel_case_types)]
struct WASM_IO<Var> {
    sp_namespace: Var,
    sp: Var,
    pc_namespace: Var,
    pc: Var,
}

#[derive(Clone)]
struct WASMGlobal<Var> {
    opcode: Var,
    sp_namespace: Var,
    sp: Var,
    pc_namespace: Var,
    pc: Var,
    new_sp_namespace: Var,
    new_sp: Var,
    new_pc_namespace: Var,
    new_pc: Var,
}

impl<Var> WASM_IO<Var> {
    fn of(fields: Vec<Var>) -> WASM_IO<Var> {
        let Ok([sp_namespace, sp, pc_namespace, pc]): Result<[Var; 4], _> = fields.try_into()
        else {
            unreachable!("wrong number of elements in io");
        };
        WASM_IO {
            sp_namespace,
            sp,
            pc_namespace,
            pc,
        }
    }
    fn to(self) -> Vec<Var> {
        let WASM_IO {
            sp_namespace,
            sp,
            pc_namespace,
            pc,
        } = self;
        vec![sp_namespace, sp, pc_namespace, pc]
    }
}

fn visit_drop<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
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
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Drop as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let prev = cb.alloc(l!("prev"));
    let zero = cb.zero();
    let one = cb.one();
    // FIXME: wrong
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
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Const as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let constant = cb.alloc(l!("constant"));
    let zero = cb.zero();
    let one = cb.one();
    cb.lookup(
        pc_namespace.clone(),
        pc.clone() + one.clone(),
        constant.clone(),
    );
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
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Add as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let first = cb.alloc(l!("first"));
    let second = cb.alloc(l!("second"));
    let one = cb.one();
    let zero = cb.zero();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace.clone(),
        sp.clone(),
        first.clone(),
        zero,
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        second.clone(),
        first + second,
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
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Get as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let index = cb.alloc(l!("index"));
    let value = cb.alloc(l!("value"));
    let zero = cb.zero();
    let one = cb.one();
    cb.lookup(
        pc_namespace.clone(),
        pc.clone() + one.clone(),
        index.clone(),
    );
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
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::Set as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let index = cb.alloc(l!("index"));
    let old = cb.alloc(l!("old"));
    let new = cb.alloc(l!("new"));
    let zero = cb.zero();
    let one = cb.one();
    cb.lookup(
        pc_namespace.clone(),
        pc.clone() + one.clone(),
        index.clone(),
    );
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

fn visit_jump_b_0<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
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
    let mut cb = cb.nest(l!("visit_jump_b_1"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(Instr::CondJump as i128);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    let unused_pc = cb.alloc(l!("unused_pc"));
    let unused_pc_namespace = cb.alloc(l!("unused_pc_namespace"));
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

fn visit_jump_b_1<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
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
    let mut cb = cb.nest(l!("visit_jump_b_0"));
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
        one.clone(),
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
        sp.clone() - one.clone(),
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

fn visit_read<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
    mut cb: B,
    switches: &mut Switches<Var>,
    WASMGlobal {
        opcode,
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

impl Circuit for WASM_VM {
    fn run<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
        &self,
        mut cb: B,
        io: Vec<Var>,
    ) -> Vec<Var> {
        let (mut switches, consume_switches) = Switches::new();
        let WASM_IO {
            sp_namespace,
            sp,
            pc_namespace,
            pc,
        } = WASM_IO::of(io);
        let opcode = cb.alloc(l!("opcode"));
        cb.lookup(pc_namespace.clone(), pc.clone(), opcode.clone());
        let new_sp_namespace = cb.alloc(l!("new_sp_namespace"));
        let new_sp = cb.alloc(l!("new_sp"));
        let new_pc_namespace = cb.alloc(l!("new_pc_namespace"));
        let new_pc = cb.alloc(l!("new_pc"));
        let global = WASMGlobal {
            opcode,
            sp_namespace,
            sp,
            pc_namespace,
            pc,
            new_sp_namespace,
            new_sp,
            new_pc_namespace,
            new_pc,
        };
        visit_drop(cb.nest(l!()), &mut switches, global.clone());
        visit_const(cb.nest(l!()), &mut switches, global.clone());
        visit_add(cb.nest(l!()), &mut switches, global.clone());
        visit_get(cb.nest(l!()), &mut switches, global.clone());
        visit_set(cb.nest(l!()), &mut switches, global.clone());
        visit_jump_b_0(cb.nest(l!()), &mut switches, global.clone());
        visit_jump_b_1(cb.nest(l!()), &mut switches, global.clone());
        visit_read(cb.nest(l!()), &mut switches, global.clone());
        visit_write(cb.nest(l!()), &mut switches, global.clone());
        switches.consume(consume_switches, cb.nest(l!()));
        let WASMGlobal {
            opcode: _,
            sp_namespace: _,
            sp: _,
            pc_namespace: _,
            pc: _,
            new_sp_namespace: sp_namespace,
            new_sp: sp,
            new_pc_namespace: pc_namespace,
            new_pc: pc,
        } = global;
        WASM_IO {
            sp_namespace,
            sp,
            pc_namespace,
            pc,
        }
        .to()
    }
}
