use crate::interface::{BranchedCircuit, CircuitBuilder, CircuitBuilderVar};
use crate::l;

macro_rules! alloc {
    ($cb:expr, [$($name:ident),* $(,)?]) => {
        $(
            let $name = $cb.alloc(
                crate::interface::Location {
                    label: stringify!($name),
                    tag: const { crate::interface::const_hash_str(stringify!($name)) },
                    file: file!(),
                    line: line!(),
                    column: column!(),
                }
            );
        )*
    };
}

#[repr(u32)]
pub enum LookupTable {
    Code,
    EntryPoints,
    SegmentedReferencedEntryPoints,
    RangeCheck32,
    RangeCheck8,
}

#[repr(u32)]
pub enum MemoryTable {
    SegmentedLinearMemory = 0,
    SegmentedStack,
    SegmentedLocals,
    AuxStack,
    SegmentedOwnedChannels,
    NextPCs,
    NextSPs,
    NextLPs,
    UTXOStates,
    UTXOStateParamOne,
    UTXOStateParamTwo,
    UnownedChannels,
    SegmentedUTXOInputsOutputs,
    UTXOInputLength,
    UTXOSuspensionCounter,
}

pub enum SegmentedMemoryTable {
    LinearMemory,
    Stack,
    Locals,
    OwnedChannels,
    UTXOInputsOutputs,
}

fn segmented_memory<Var: CircuitBuilderVar, B: Builder<Var>>(
    cb: B,
    namespace: SegmentedMemoryTable,
    address: Var,
    old: Var,
    new: Var,
) {
    segmented_memory_(cb, namespace, address, old, new, 0)
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
pub enum StarstreamIO {
    n_utxos,
    this_utxo_plus_one,
    next_free_channel,
    pc,
    sp,
    lp,
    ap,
}

trait Builder<Var: CircuitBuilderVar>:
    CircuitBuilder<Var, StarstreamIO, LookupTable, MemoryTable>
{
}
impl<Var: CircuitBuilderVar, B: CircuitBuilder<Var, StarstreamIO, LookupTable, MemoryTable>>
    Builder<Var> for B
{
}

#[allow(non_camel_case_types)]
pub struct StarstreamVM;

// param = code[pc + 1]
// if param is used, pc += 2
#[repr(u32)]
pub enum Instr {
    /// not provable, hence it has no corresponding code
    Unreachable = 0,
    /// {} -> {}
    Nop,
    /// x -> {}
    Drop,
    /// {} -> param
    Const,
    /// {} -> locals[lp - param]
    GetLocal,
    /// x -> {}; locals[lp - param] := x
    SetLocal,
    /// c new_pc -> {}; pc := new_pc if c == 0, else if c != 0 then pc := pc + 1
    CondJump,
    /// new_pc -> {}; pc := new_pc, lp += param
    Jump,
    /// ptr -> memory[ptr + param]
    Read8,
    /// ptr data -> {}; memory[ptr + param] := data
    Write8,
    /// ptr -> memory[ptr + param] + memory[ptr + param + 1] * 2^8
    Read16,
    /// ptr data -> {}; memory[ptr + param] + memory[ptr + param + 1] * 2^8 := data
    Write16,
    /// ptr -> memory[ptr + param] + memory[ptr + param + 1] * 2^8 + memory[ptr + param + 2] * 2^16 + memory[ptr + param + 3] * 2^24
    Read32,
    /// ptr data -> {}; memory[ptr + param] + memory[ptr + param + 1] * 2^8 + memory[ptr + param + 2] * 2^16 + memory[ptr + param + 3] * 2^24 := data
    Write32,
    /// x y c -> y if c == 0
    /// x y c -> x if c != 0
    Select,
    /// x -> {}; aux := x :: aux
    PushAux,
    /// {} -> y; (y :: aux) := aux
    PopAux,
    /// x y -> x+y
    Add,
    /// x y -> x*y
    Mul,
    /// x -> -x
    Neg,
    StarstreamReadInput,
    StarstreamWriteOutput,
    StarstreamGetInputLength,
    StarstreamSuspend,
    StarstreamConnect,
    StarstreamSync,
    StarstreamSwap,
    StarstreamHandshakeAny,
    StarstreamHandshakeSelf,
    StarstreamHandshake,
    StarstreamPing,
    StarstreamPong,
    StarstreamConnectOwn,
    StarstreamSwapOwn,
    StarstreamClose,
    StarstreamRelease,
    StarstreamFail,
    Witness,
    // not an instruction, signifies end of enum
    _End,
}

#[repr(u32)]
pub enum UTXOState {
    Running,
    Suspended,
    Paused,
    Connecting,
    Syncing,
    Swapping,
    HandshakingAny,
    Handshaking,
    Pinging,
    Ponging,
}

const POW_2_32: i128 = 256 * 256 * 256 * 256;

fn segmented_memory__<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    this_utxo: Var,
    namespace: SegmentedMemoryTable,
    address: Var,
    old: Var,
    new: Var,
    manual_offset: i128,
) {
    let mut cb = cb.nest(l!("segmented_memory"));
    let n_utxos = cb.input(StarstreamIO::n_utxos);
    let segmented_namespace = match namespace {
        SegmentedMemoryTable::LinearMemory => MemoryTable::SegmentedLinearMemory,
        SegmentedMemoryTable::Stack => MemoryTable::SegmentedStack,
        SegmentedMemoryTable::Locals => MemoryTable::SegmentedLocals,
        SegmentedMemoryTable::OwnedChannels => MemoryTable::SegmentedOwnedChannels,
        SegmentedMemoryTable::UTXOInputsOutputs => MemoryTable::SegmentedUTXOInputsOutputs,
    };
    // when we suspend, we reset our memory, stack, locals, input/output, but not the owned channels
    let is_offset = match namespace {
        SegmentedMemoryTable::LinearMemory => true,
        SegmentedMemoryTable::Stack => true,
        SegmentedMemoryTable::Locals => true,
        SegmentedMemoryTable::OwnedChannels => false,
        SegmentedMemoryTable::UTXOInputsOutputs => true,
    };
    let offset = if is_offset {
        alloc!(cb, [suspension_counter, offset]);
        cb.memory(
            MemoryTable::UTXOSuspensionCounter,
            this_utxo.clone(),
            suspension_counter.clone(),
            suspension_counter.clone(),
        );
        cb.enforce(l!(), suspension_counter, n_utxos, offset.clone());
        offset
    } else {
        Var::from(0)
    };
    let segmented_address = address + (this_utxo + offset + manual_offset) * POW_2_32;
    cb.memory(segmented_namespace, segmented_address, old, new);
}

fn segmented_memory_<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    namespace: SegmentedMemoryTable,
    address: Var,
    old: Var,
    new: Var,
    manual_offset: i128,
) {
    let this_utxo = cb.input(StarstreamIO::this_utxo_plus_one) - 1;
    segmented_memory__(cb, this_utxo, namespace, address, old, new, manual_offset)
}

fn static_<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, io: StarstreamIO) -> Var {
    let mut cb = cb.nest(l!("static_"));
    let input = cb.input(io);
    let output = cb.output(io);
    cb.enforce(l!(), 1.into(), input.clone(), output);
    input
}

fn static_utxo<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) -> Lp<Var> {
    let mut cb = cb.nest(l!("static_utxo"));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    let lp = static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    Lp(lp)
}

fn inc_pc<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, instr: Instr) {
    let mut cb = cb.nest(l!("inc_pc"));
    let input = cb.input(StarstreamIO::pc);
    let output = cb.output(StarstreamIO::pc);
    cb.enforce(l!(), 1.into(), input.clone() + 1, output);
    let instr = Var::from(instr as i128);
    cb.lookup(LookupTable::Code, input, instr);
}

fn double_inc_pc<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, instr: Instr, param: Var) {
    let mut cb = cb.nest(l!("double_inc_pc"));
    let input = cb.input(StarstreamIO::pc);
    let output = cb.output(StarstreamIO::pc);
    cb.enforce(l!(), 1.into(), input.clone() + 2, output);
    let instr = Var::from(instr as u32 as i128);
    cb.lookup(LookupTable::Code, input.clone(), instr);
    cb.lookup(LookupTable::Code, input + 1, param);
}

struct Lp<A>(A);

fn do_stack<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, inputs: &[Var], outputs: &[Var]) {
    let mut cb = cb.nest(l!("do_stack"));
    let sp = cb.input(StarstreamIO::sp);
    let new_sp = cb.output(StarstreamIO::sp);
    let sp_diff = Var::from(inputs.len() as i128 - outputs.len() as i128);
    cb.enforce(l!(), 1.into(), sp.clone() + sp_diff, new_sp);
    for (i, (input, output)) in ((inputs.iter()).zip(outputs)).enumerate() {
        let index = inputs.len() as i128 - i as i128;
        segmented_memory(
            cb.nest(l!()),
            SegmentedMemoryTable::Stack,
            sp.clone() - index,
            input.clone(),
            output.clone(),
        );
    }
    for (i, input) in (inputs.iter().enumerate()).skip(outputs.len()) {
        let index = inputs.len() as i128 - i as i128;
        segmented_memory(
            cb.nest(l!()),
            SegmentedMemoryTable::Stack,
            sp.clone() - index,
            input.clone(),
            0.into(),
        );
    }
    for (i, output) in outputs.iter().skip(inputs.len()).enumerate() {
        segmented_memory(
            cb.nest(l!()),
            SegmentedMemoryTable::Stack,
            sp.clone() + i as i128,
            0.into(),
            output.clone(),
        );
    }
}

// FIXME: make sure either this or assert_not_in_utxo is called in every branch
fn assert_in_utxo<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let this_utxo_plus_one = cb.input(StarstreamIO::this_utxo_plus_one);
    assert_non_zero(cb.nest(l!()), this_utxo_plus_one);
}

fn assert_not_in_utxo<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let this_utxo_plus_one = cb.input(StarstreamIO::this_utxo_plus_one);
    cb.enforce(l!(), this_utxo_plus_one, 1.into(), 0.into());
}

fn instr<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    instr: Instr,
    param: Option<Var>,
    inputs: &[Var],
    outputs: &[Var],
) -> Lp<Var> {
    let mut cb = cb.nest(l!("instr"));
    if let Some(param) = param {
        double_inc_pc(cb.nest(l!()), instr, param);
    } else {
        inc_pc(cb.nest(l!()), instr);
    }
    do_stack(cb.nest(l!()), inputs, outputs);
    assert_in_utxo(cb.nest(l!()));
    static_utxo(cb.nest(l!()))
}

fn starstream_instr<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    instr: Instr,
    new_state: UTXOState,
    params: Option<(Var, Option<Var>)>,
    inputs: &[Var],
) {
    let mut cb = cb.nest(l!("starstream_instr"));
    alloc!(cb, [prev_next_pc, prev_next_sp, prev_next_lp,]);
    assert_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    let this_utxo = cb.input(StarstreamIO::this_utxo_plus_one) - 1;
    let new_this_utxo_plus_one = cb.output(StarstreamIO::this_utxo_plus_one);
    cb.enforce(l!(), new_this_utxo_plus_one, 1.into(), 0.into());
    let pc = cb.input(StarstreamIO::pc);
    let instr = Var::from(instr as i128);
    cb.lookup(LookupTable::Code, pc.clone(), instr);
    do_stack(cb.nest(l!()), inputs, &[]);
    let new_pc = cb.output(StarstreamIO::pc);
    cb.enforce(l!(), new_pc, 1.into(), 0.into());
    let sp = cb.input(StarstreamIO::sp);
    let new_sp = cb.output(StarstreamIO::sp);
    cb.enforce(l!(), new_sp, 1.into(), 0.into());
    let lp = cb.input(StarstreamIO::lp);
    let new_lp = cb.output(StarstreamIO::lp);
    cb.enforce(l!(), new_lp, 1.into(), 0.into());
    cb.memory(
        MemoryTable::NextPCs,
        this_utxo.clone(),
        prev_next_pc,
        pc + 1,
    );
    cb.memory(MemoryTable::NextSPs, this_utxo.clone(), prev_next_sp, sp);
    cb.memory(MemoryTable::NextLPs, this_utxo.clone(), prev_next_lp, lp);
    cb.memory(
        MemoryTable::UTXOStates,
        this_utxo.clone(),
        Var::from(UTXOState::Running as i128),
        Var::from(new_state as i128),
    );
    if let Some((param_one, param_two)) = params {
        cb.memory(
            MemoryTable::UTXOStateParamOne,
            this_utxo.clone(),
            0.into(),
            param_one,
        );
        if let Some(param_two) = param_two {
            cb.memory(
                MemoryTable::UTXOStateParamTwo,
                this_utxo.clone(),
                0.into(),
                param_two,
            );
        };
    };
}

fn visit_nop<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_nop"));
    assert_in_utxo(cb.nest(l!()));
    inc_pc(cb.nest(l!()), Instr::Nop);
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::sp);
    static_(cb.nest(l!()), StarstreamIO::lp);
}

fn visit_drop<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_drop"));
    let Lp(_) = instr(cb.nest(l!()), Instr::Drop, None, &[], &[]);
}

fn visit_const<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_const"));
    let constant = cb.alloc(l!("constant"));
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Const,
        Some(constant.clone()),
        &[],
        &[constant],
    );
}

fn visit_get_local<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_get_local"));
    let idx = cb.alloc(l!("index"));
    let val = cb.alloc(l!("value"));
    let Lp(lp) = instr(
        cb.nest(l!()),
        Instr::GetLocal,
        Some(idx.clone()),
        &[],
        &[val.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::Locals,
        lp.clone() - idx,
        val.clone(),
        val,
    );
}

fn visit_set_local<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_set_local"));
    alloc!(cb, [idx, old_val, new_val]);
    let Lp(lp) = instr(
        cb.nest(l!()),
        Instr::SetLocal,
        Some(idx.clone()),
        &[new_val.clone()],
        &[],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::Locals,
        lp.clone() - idx,
        old_val,
        new_val,
    );
}

fn assert_non_zero<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, x: Var) {
    let mut cb = cb.nest(l!("assert_non_zero"));
    let inv = cb.alloc(l!("inv"));
    cb.enforce(l!(), x, inv, 1.into());
}

fn assert_bit<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, x: Var) {
    let mut cb = cb.nest(l!("assert_bit"));
    cb.enforce(l!(), x.clone(), Var::from(1) - x, 0.into());
}

fn visit_cond_jump_c_not_0<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_cond_jump_c_not_0"));
    alloc!(cb, [c, new_pc]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::CondJump,
        None,
        &[c.clone(), new_pc],
        &[],
    );
    assert_non_zero(cb.nest(l!()), c);
}

fn visit_cond_jump_c_0<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_cond_jump_c_0"));
    let new_pc = cb.output(StarstreamIO::pc);
    do_stack(cb.nest(l!()), &[0.into(), new_pc], &[]);
    static_utxo(cb.nest(l!()));
    let pc = cb.input(StarstreamIO::pc);
    let instr = Var::from(Instr::CondJump as i128);
    cb.lookup(LookupTable::Code, pc, instr);
}

fn visit_jump<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_jump"));
    assert_in_utxo(cb.nest(l!()));
    let new_pc = cb.output(StarstreamIO::pc);
    do_stack(cb.nest(l!()), &[new_pc], &[]);
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    alloc!(cb, [lp_diff]);
    let lp = cb.input(StarstreamIO::lp);
    let new_lp = cb.output(StarstreamIO::lp);
    cb.enforce(l!(), 1.into(), lp + lp_diff.clone(), new_lp);
    let pc = cb.input(StarstreamIO::pc);
    let instr = Var::from(Instr::Jump as i128);
    cb.lookup(LookupTable::Code, pc.clone(), instr);
    cb.lookup(LookupTable::Code, pc.clone() + 1, lp_diff);
}

fn visit_read8<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_read8"));
    alloc!(cb, [offset, ptr, byte]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Read8,
        Some(offset.clone()),
        &[ptr.clone()],
        &[byte.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr + offset,
        byte.clone(),
        byte,
    );
}

fn visit_write8<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_write8"));
    alloc!(cb, [offset, ptr, old_byte, byte]);
    cb.lookup(LookupTable::RangeCheck8, byte.clone(), 1.into());
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Write8,
        Some(offset.clone()),
        &[ptr.clone(), byte.clone()],
        &[],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr + offset,
        old_byte,
        byte,
    );
}

fn visit_read16<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_read16"));
    alloc!(cb, [offset, ptr, lower_byte, upper_byte]);
    let n = lower_byte.clone() + upper_byte.clone() * 256;
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Read16,
        Some(offset.clone()),
        &[ptr.clone()],
        &[n.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr.clone() + offset.clone(),
        lower_byte.clone(),
        lower_byte,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr + offset + 1,
        upper_byte.clone(),
        upper_byte,
    );
}

fn visit_write16<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_write16"));
    alloc!(
        cb,
        [
            offset,
            ptr,
            old_lower_byte,
            old_upper_byte,
            lower_byte,
            upper_byte
        ]
    );
    cb.lookup(LookupTable::RangeCheck8, lower_byte.clone(), 1.into());
    cb.lookup(LookupTable::RangeCheck8, upper_byte.clone(), 1.into());
    let n = lower_byte.clone() + upper_byte.clone() * 256;
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Write16,
        Some(offset.clone()),
        &[ptr.clone(), n.clone()],
        &[],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr.clone() + offset.clone(),
        old_lower_byte,
        lower_byte,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr + offset + 1,
        old_upper_byte,
        upper_byte,
    );
}

fn visit_read32<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_read32"));
    alloc!(cb, [offset, ptr, first, second, third, fourth]);
    let n = first.clone()
        + second.clone() * 256
        + third.clone() * 256 * 256
        + fourth.clone() * 256 * 256 * 256;
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Read32,
        Some(offset.clone()),
        &[ptr.clone()],
        &[n.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr.clone() + offset.clone(),
        first.clone(),
        first,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr.clone() + offset.clone() + 1,
        second.clone(),
        second,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr.clone() + offset.clone() + 2,
        third.clone(),
        third,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr + offset + 3,
        fourth.clone(),
        fourth,
    );
}

fn visit_write32<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_write32"));
    alloc!(
        cb,
        [
            offset, ptr, old_first, old_second, old_third, old_fourth, first, second, third, fourth
        ]
    );
    cb.lookup(LookupTable::RangeCheck8, first.clone(), 1.into());
    cb.lookup(LookupTable::RangeCheck8, second.clone(), 1.into());
    cb.lookup(LookupTable::RangeCheck8, third.clone(), 1.into());
    cb.lookup(LookupTable::RangeCheck8, fourth.clone(), 1.into());
    let n = first.clone()
        + second.clone() * 256
        + third.clone() * 256 * 256
        + fourth.clone() * 256 * 256 * 256;
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Write32,
        Some(offset.clone()),
        &[ptr.clone(), n.clone()],
        &[],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr.clone() + offset.clone(),
        old_first,
        first,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr.clone() + offset.clone() + 1,
        old_second,
        second,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr.clone() + offset.clone() + 2,
        old_third,
        third,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::LinearMemory,
        ptr + offset + 3,
        old_fourth,
        fourth,
    );
}

fn visit_select_c_not_0<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_select_c_not_0"));
    alloc!(cb, [x, y, c]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Select,
        None,
        &[x.clone(), y, c.clone()],
        &[x],
    );
    assert_non_zero(cb.nest(l!()), c.clone());
}

fn visit_select_c_0<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_select_c_0"));
    alloc!(cb, [x, y]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Select,
        None,
        &[x, y.clone(), 0.into()],
        &[y],
    );
}

fn visit_push_aux<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_push_aux"));
    assert_in_utxo(cb.nest(l!()));
    alloc!(cb, [x]);
    do_stack(cb.nest(l!()), &[x.clone()], &[]);
    inc_pc(cb.nest(l!()), Instr::PushAux);
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::lp);
    let ap = cb.input(StarstreamIO::ap);
    let new_ap = cb.output(StarstreamIO::ap);
    cb.enforce(l!(), 1.into(), ap.clone() + 1, new_ap);
    cb.memory(MemoryTable::AuxStack, ap, 0.into(), x);
}

fn visit_pop_aux<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_pop_aux"));
    assert_in_utxo(cb.nest(l!()));
    alloc!(cb, [x]);
    do_stack(cb.nest(l!()), &[], &[x.clone()]);
    inc_pc(cb.nest(l!()), Instr::PopAux);
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::lp);
    let ap = cb.input(StarstreamIO::ap);
    let new_ap = cb.output(StarstreamIO::ap);
    cb.enforce(l!(), 1.into(), ap.clone() - 1, new_ap);
    cb.memory(MemoryTable::AuxStack, ap - 1, x, 0.into());
}

fn visit_add<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_add"));
    alloc!(cb, [x, y, r, carry]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Add,
        None,
        &[x.clone(), y.clone()],
        &[r.clone()],
    );
    assert_bit(cb.nest(l!()), carry.clone());
    cb.lookup(LookupTable::RangeCheck32, r.clone(), 1.into());
    cb.enforce(l!(), 1.into(), r + carry * POW_2_32, x + y);
}

// FIXME: assumes field has 64 bits
fn visit_mul<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_mul"));
    alloc!(cb, [x, y, lower_r, upper_r]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Mul,
        None,
        &[x.clone(), y.clone()],
        &[lower_r.clone()],
    );
    cb.lookup(LookupTable::RangeCheck32, lower_r.clone(), 1.into());
    cb.lookup(LookupTable::RangeCheck32, upper_r.clone(), 1.into());
    cb.enforce(l!(), x, y, lower_r + upper_r * POW_2_32);
}

fn visit_neg<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_neg"));
    alloc!(cb, [x]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::Mul,
        None,
        &[x.clone()],
        &[Var::from(POW_2_32) - x],
    );
}

fn visit_starstream_read_input<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_read_input"));
    alloc!(cb, [idx, byte, input_length]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::StarstreamReadInput,
        None,
        &[idx.clone()],
        &[byte.clone()],
    );
    let this_utxo = cb.input(StarstreamIO::this_utxo_plus_one) - 1;
    cb.memory(
        MemoryTable::UTXOInputLength,
        this_utxo,
        input_length.clone(),
        input_length.clone(),
    );
    // FIXME: check that index is within bounds
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::UTXOInputsOutputs,
        idx,
        byte.clone(),
        byte,
    );
}

fn visit_starstream_write_output<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_write_output"));
    alloc!(cb, [idx, old_byte, byte]);
    cb.lookup(LookupTable::RangeCheck8, byte.clone(), 1.into());
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::StarstreamWriteOutput,
        None,
        &[idx.clone(), byte.clone()],
        &[],
    );
    segmented_memory_(
        cb.nest(l!()),
        SegmentedMemoryTable::UTXOInputsOutputs,
        idx,
        old_byte,
        byte,
        1,
    );
}

fn visit_starstream_get_input_length<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_get_input_length"));
    alloc!(cb, [input_length]);
    let Lp(_) = instr(
        cb.nest(l!()),
        Instr::StarstreamGetInputLength,
        None,
        &[],
        &[input_length.clone()],
    );
    let this_utxo = cb.input(StarstreamIO::this_utxo_plus_one) - 1;
    cb.memory(
        MemoryTable::UTXOInputLength,
        this_utxo,
        input_length.clone(),
        input_length.clone(),
    );
}

fn visit_starstream_suspend<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_suspend"));
    alloc!(
        cb,
        [
            input_length,
            output_length,
            prev_next_pc,
            prev_next_sp,
            prev_next_lp,
            entry_point,
            suspension_counter,
        ]
    );
    assert_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    do_stack(cb.nest(l!()), &[output_length.clone()], &[]);
    let this_utxo = cb.input(StarstreamIO::this_utxo_plus_one) - 1;
    let new_this_utxo_plus_one = cb.output(StarstreamIO::this_utxo_plus_one);
    cb.enforce(l!(), new_this_utxo_plus_one, 1.into(), 0.into());
    cb.memory(
        MemoryTable::UTXOInputLength,
        this_utxo.clone(),
        input_length,
        output_length.clone(),
    );
    cb.lookup(
        LookupTable::EntryPoints,
        this_utxo.clone(),
        entry_point.clone(),
    );
    let new_sp = cb.output(StarstreamIO::sp);
    cb.enforce(l!(), new_sp, 1.into(), 0.into());
    let new_pc = cb.output(StarstreamIO::pc);
    cb.enforce(l!(), new_pc, 1.into(), 0.into());
    let new_lp = cb.output(StarstreamIO::lp);
    cb.enforce(l!(), new_lp, 1.into(), 0.into());
    cb.memory(
        MemoryTable::NextPCs,
        this_utxo.clone(),
        prev_next_pc,
        entry_point,
    );
    cb.memory(
        MemoryTable::NextSPs,
        this_utxo.clone(),
        prev_next_sp,
        0.into(),
    );
    cb.memory(
        MemoryTable::NextLPs,
        this_utxo.clone(),
        prev_next_lp,
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOSuspensionCounter,
        this_utxo.clone(),
        suspension_counter.clone(),
        suspension_counter + 1,
    );
    cb.memory(
        MemoryTable::UTXOStates,
        this_utxo,
        Var::from(UTXOState::Running as i128),
        Var::from(UTXOState::Suspended as i128),
    );
}

fn visit_starstream_connect<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_connect"));
    alloc!(cb, [at]);
    starstream_instr(
        cb.nest(l!()),
        Instr::StarstreamConnect,
        UTXOState::Connecting,
        Some((at.clone(), None)),
        &[at],
    );
}

fn visit_starstream_sync<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_sync"));
    alloc!(cb, [at, value, channel]);
    starstream_instr(
        cb.nest(l!()),
        Instr::StarstreamSync,
        UTXOState::Syncing,
        Some((channel.clone(), Some(value.clone()))),
        &[at.clone(), value],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        at,
        channel.clone(),
        channel,
    );
}

fn visit_starstream_swap<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_swap"));
    alloc!(cb, [at, this, channel]);
    starstream_instr(
        cb.nest(l!()),
        Instr::StarstreamSync,
        UTXOState::Swapping,
        Some((channel.clone(), Some(this.clone()))),
        &[at.clone(), this],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        at,
        channel.clone(),
        channel,
    );
}

fn visit_starstream_handshake_any<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_handshake_any"));
    alloc!(cb, [at, channel]);
    starstream_instr(
        cb.nest(l!()),
        Instr::StarstreamHandshakeAny,
        UTXOState::HandshakingAny,
        Some((channel.clone(), None)),
        &[at.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        at,
        channel.clone(),
        channel,
    );
}

fn visit_starstream_handshake_self<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_handshake_self"));
    alloc!(cb, [at, entry_point, channel]);
    let this_utxo = cb.input(StarstreamIO::this_utxo_plus_one) - 1;
    cb.lookup(LookupTable::EntryPoints, this_utxo, entry_point.clone());
    starstream_instr(
        cb.nest(l!()),
        Instr::StarstreamHandshakeSelf,
        UTXOState::Handshaking,
        Some((channel.clone(), Some(entry_point))),
        &[at.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        at,
        channel.clone(),
        channel,
    );
}

fn visit_starstream_handshake<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_handshake"));
    alloc!(cb, [at, program_index, entry_point, channel]);
    let this_utxo = cb.input(StarstreamIO::this_utxo_plus_one) - 1;
    cb.lookup(
        LookupTable::SegmentedReferencedEntryPoints,
        this_utxo * POW_2_32 + program_index,
        entry_point.clone(),
    );
    starstream_instr(
        cb.nest(l!()),
        Instr::StarstreamHandshake,
        UTXOState::Handshaking,
        Some((channel.clone(), Some(entry_point))),
        &[at.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        at,
        channel.clone(),
        channel,
    );
}

fn visit_starstream_ping<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_ping"));
    alloc!(cb, [at, channel]);
    starstream_instr(
        cb.nest(l!()),
        Instr::StarstreamPing,
        UTXOState::Pinging,
        Some((channel.clone(), None)),
        &[at.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        at,
        channel.clone(),
        channel,
    );
}

fn visit_starstream_pong<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_pong"));
    alloc!(cb, [at, channel]);
    starstream_instr(
        cb.nest(l!()),
        Instr::StarstreamPong,
        UTXOState::Ponging,
        Some((channel.clone(), None)),
        &[at.clone()],
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        at,
        channel.clone(),
        channel,
    );
}

fn visit_starstream_connect_own<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_connect_own"));
    alloc!(cb, [x, y, prev_at_x, prev_at_y]);
    assert_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    do_stack(cb.nest(l!()), &[x.clone(), y.clone()], &[]);
    inc_pc(cb.nest(l!()), Instr::StarstreamConnectOwn);
    let next_free_channel = cb.input(StarstreamIO::next_free_channel);
    let new_next_free_channel = cb.output(StarstreamIO::next_free_channel);
    cb.enforce(
        l!(),
        1.into(),
        next_free_channel.clone() + 1,
        new_next_free_channel,
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        x,
        prev_at_x,
        next_free_channel.clone(),
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        y,
        prev_at_y,
        next_free_channel,
    );
}

fn visit_starstream_swap_own<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_swap_own"));
    alloc!(cb, [x, y, at_x, at_y]);
    assert_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    do_stack(cb.nest(l!()), &[x.clone(), y.clone()], &[]);
    inc_pc(cb.nest(l!()), Instr::StarstreamSwapOwn);
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        x,
        at_x.clone(),
        at_y.clone(),
    );
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        y,
        at_y,
        at_x,
    );
}

fn visit_starstream_close<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_starstream_close"));
    alloc!(cb, [x, at_x]);
    assert_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    do_stack(cb.nest(l!()), &[x.clone()], &[]);
    inc_pc(cb.nest(l!()), Instr::StarstreamClose);
    segmented_memory(
        cb.nest(l!()),
        SegmentedMemoryTable::OwnedChannels,
        x,
        at_x.clone(),
        0.into(),
    );
}

fn visit_witness<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_witness"));
    alloc!(cb, [w]);
    let Lp(_) = instr(cb.nest(l!()), Instr::Witness, None, &[], &[w]);
}

#[derive(PartialEq)]
pub enum PausedOrSuspended {
    Paused,
    Suspended,
}

fn visit_starstream_enter<Var: CircuitBuilderVar, B: Builder<Var>>(
    mut cb: B,
    paused_or_suspended: PausedOrSuspended,
) {
    let mut cb = cb.nest(l!("visit_starstream_enter"));
    alloc!(cb, [this_utxo,]);
    assert_not_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    let new_this_utxo_plus_one = cb.output(StarstreamIO::this_utxo_plus_one);
    cb.enforce(
        l!(),
        1.into(),
        new_this_utxo_plus_one,
        this_utxo.clone() + 1,
    );
    let new_sp = cb.output(StarstreamIO::sp);
    cb.memory(MemoryTable::NextSPs, this_utxo.clone(), new_sp, 0.into());
    let new_pc = cb.output(StarstreamIO::pc);
    cb.memory(MemoryTable::NextPCs, this_utxo.clone(), new_pc, 0.into());
    let new_lp = cb.output(StarstreamIO::lp);
    cb.memory(MemoryTable::NextLPs, this_utxo.clone(), new_lp, 0.into());
    cb.memory(
        MemoryTable::UTXOStates,
        this_utxo,
        Var::from(match paused_or_suspended {
            PausedOrSuspended::Paused => UTXOState::Paused,
            PausedOrSuspended::Suspended => UTXOState::Suspended,
        } as i128),
        Var::from(UTXOState::Running as i128),
    );
}

fn visit_pair_connecting<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_pair_connecting"));
    alloc!(cb, [x, y, x_index, y_index, x_prev, y_prev]);
    assert_not_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::pc);
    static_(cb.nest(l!()), StarstreamIO::sp);
    static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    let next_free_channel = cb.input(StarstreamIO::next_free_channel);
    let new_next_free_channel = cb.output(StarstreamIO::next_free_channel);
    cb.enforce(
        l!(),
        1.into(),
        next_free_channel.clone() + 1,
        new_next_free_channel,
    );
    cb.memory(
        MemoryTable::UTXOStates,
        x.clone(),
        Var::from(UTXOState::Connecting as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        x.clone(),
        x_index.clone(),
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOStates,
        y.clone(),
        Var::from(UTXOState::Connecting as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        y.clone(),
        y_index.clone(),
        0.into(),
    );
    segmented_memory__(
        cb.nest(l!()),
        x,
        SegmentedMemoryTable::OwnedChannels,
        x_index,
        x_prev,
        next_free_channel.clone(),
        0,
    );
    segmented_memory__(
        cb.nest(l!()),
        y,
        SegmentedMemoryTable::OwnedChannels,
        y_index,
        y_prev,
        next_free_channel.clone(),
        0,
    );
}

fn visit_pair_syncing<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_pair_syncing"));
    alloc!(cb, [x, y, channel, value]);
    assert_not_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::pc);
    static_(cb.nest(l!()), StarstreamIO::sp);
    static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    cb.memory(
        MemoryTable::UTXOStates,
        x.clone(),
        Var::from(UTXOState::Syncing as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        x.clone(),
        channel.clone(),
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOStateParamTwo,
        x.clone(),
        value.clone(),
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOStates,
        y.clone(),
        Var::from(UTXOState::Syncing as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        y.clone(),
        channel.clone(),
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOStateParamTwo,
        y.clone(),
        value.clone(),
        0.into(),
    );
}

fn visit_pair_swapping<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_pair_swapping"));
    alloc!(cb, [x, y, x_index, y_index, x_channel, y_channel]);
    assert_not_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::pc);
    static_(cb.nest(l!()), StarstreamIO::sp);
    static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    cb.memory(
        MemoryTable::UTXOStates,
        x.clone(),
        Var::from(UTXOState::Swapping as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        x.clone(),
        x_index.clone(),
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOStateParamTwo,
        x.clone(),
        x_channel.clone(),
        0.into(),
    );
    segmented_memory__(
        cb.nest(l!()),
        x,
        SegmentedMemoryTable::OwnedChannels,
        x_index,
        x_channel.clone(),
        y_channel.clone(),
        0,
    );
    cb.memory(
        MemoryTable::UTXOStates,
        y.clone(),
        Var::from(UTXOState::Swapping as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        y.clone(),
        y_index.clone(),
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOStateParamTwo,
        y.clone(),
        y_channel.clone(),
        0.into(),
    );
    segmented_memory__(
        cb.nest(l!()),
        y,
        SegmentedMemoryTable::OwnedChannels,
        y_index,
        y_channel.clone(),
        x_channel.clone(),
        0,
    );
}

// TODO: should two HandshakingAny be valid?
fn visit_pair_handshaking<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B, left_is_any: bool) {
    let mut cb = cb.nest(l!("visit_pair_handshaking"));
    alloc!(cb, [x, y, channel, x_entrypoint, y_entrypoint]);
    assert_not_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::pc);
    static_(cb.nest(l!()), StarstreamIO::sp);
    static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    cb.memory(
        MemoryTable::UTXOStates,
        x.clone(),
        Var::from(if left_is_any {
            UTXOState::HandshakingAny
        } else {
            UTXOState::Handshaking
        } as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        x.clone(),
        channel.clone(),
        0.into(),
    );
    if !left_is_any {
        cb.memory(
            MemoryTable::UTXOStateParamTwo,
            x.clone(),
            y_entrypoint.clone(),
            0.into(),
        );
    }
    cb.memory(
        MemoryTable::UTXOStates,
        y.clone(),
        Var::from(UTXOState::Handshaking as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        y.clone(),
        channel.clone(),
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOStateParamTwo,
        y.clone(),
        x_entrypoint.clone(),
        0.into(),
    );
    cb.lookup(LookupTable::EntryPoints, x, x_entrypoint);
    // FIXME: make y_entrypoint variable not allocated when left_is_any
    if !left_is_any {
        cb.lookup(LookupTable::EntryPoints, y, y_entrypoint);
    }
}

fn visit_pair_ping_pong<Var: CircuitBuilderVar, B: Builder<Var>>(mut cb: B) {
    let mut cb = cb.nest(l!("visit_pair_ping_pong"));
    alloc!(cb, [x, y, channel]);
    assert_not_in_utxo(cb.nest(l!()));
    static_(cb.nest(l!()), StarstreamIO::n_utxos);
    static_(cb.nest(l!()), StarstreamIO::this_utxo_plus_one);
    static_(cb.nest(l!()), StarstreamIO::next_free_channel);
    static_(cb.nest(l!()), StarstreamIO::pc);
    static_(cb.nest(l!()), StarstreamIO::sp);
    static_(cb.nest(l!()), StarstreamIO::lp);
    static_(cb.nest(l!()), StarstreamIO::ap);
    cb.memory(
        MemoryTable::UTXOStates,
        x.clone(),
        Var::from(UTXOState::Pinging as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        x.clone(),
        channel.clone(),
        0.into(),
    );
    cb.memory(
        MemoryTable::UTXOStates,
        y.clone(),
        Var::from(UTXOState::Ponging as i128),
        Var::from(UTXOState::Paused as i128),
    );
    cb.memory(
        MemoryTable::UTXOStateParamOne,
        y.clone(),
        channel.clone(),
        0.into(),
    );
}

#[derive(PartialEq)]
#[allow(non_camel_case_types)]
pub enum Branch {
    visit_nop,
    visit_drop,
    visit_const,
    visit_get_local,
    visit_set_local,
    visit_cond_jump_c_not_0,
    visit_cond_jump_c_0,
    visit_jump,
    visit_read8,
    visit_write8,
    visit_read16,
    visit_write16,
    visit_read32,
    visit_write32,
    visit_select_c_not_0,
    visit_select_c_0,
    visit_push_aux,
    visit_pop_aux,
    visit_add,
    visit_mul,
    visit_neg,
    visit_starstream_read_input,
    visit_starstream_write_output,
    visit_starstream_get_input_length,
    visit_starstream_suspend,
    visit_starstream_connect,
    visit_starstream_sync,
    visit_starstream_swap,
    visit_starstream_handshake_any,
    visit_starstream_handshake_self,
    visit_starstream_handshake,
    visit_starstream_ping,
    visit_starstream_pong,
    visit_starstream_connect_own,
    visit_starstream_swap_own,
    visit_starstream_close,
    visit_witness,
    visit_starstream_enter(PausedOrSuspended),
    visit_pair_connecting,
    visit_pair_syncing,
    visit_pair_swapping,
    visit_pair_handshaking { left_is_any: bool },
    visit_pair_ping_pong,
}

impl BranchedCircuit<Branch, StarstreamIO, LookupTable, MemoryTable> for StarstreamVM {
    fn branches(&self) -> impl Iterator<Item = Branch> {
        [
            Branch::visit_nop,
            Branch::visit_drop,
            Branch::visit_const,
            Branch::visit_get_local,
            Branch::visit_set_local,
            Branch::visit_cond_jump_c_not_0,
            Branch::visit_cond_jump_c_0,
            Branch::visit_jump,
            Branch::visit_read8,
            Branch::visit_write8,
            Branch::visit_read16,
            Branch::visit_write16,
            Branch::visit_read32,
            Branch::visit_write32,
            Branch::visit_select_c_not_0,
            Branch::visit_select_c_0,
            Branch::visit_push_aux,
            Branch::visit_pop_aux,
            Branch::visit_add,
            Branch::visit_mul,
            Branch::visit_neg,
            Branch::visit_starstream_read_input,
            Branch::visit_starstream_write_output,
            Branch::visit_starstream_get_input_length,
            Branch::visit_starstream_suspend,
            Branch::visit_starstream_connect,
            Branch::visit_starstream_sync,
            Branch::visit_starstream_swap,
            Branch::visit_starstream_handshake_any,
            Branch::visit_starstream_handshake_self,
            Branch::visit_starstream_handshake,
            Branch::visit_starstream_ping,
            Branch::visit_starstream_pong,
            Branch::visit_starstream_connect_own,
            Branch::visit_starstream_swap_own,
            Branch::visit_starstream_close,
            Branch::visit_witness,
            Branch::visit_starstream_enter(PausedOrSuspended::Paused),
            Branch::visit_starstream_enter(PausedOrSuspended::Suspended),
            Branch::visit_pair_connecting,
            Branch::visit_pair_syncing,
            Branch::visit_pair_swapping,
            Branch::visit_pair_handshaking { left_is_any: false },
            Branch::visit_pair_handshaking { left_is_any: true },
            Branch::visit_pair_ping_pong,
        ]
        .into_iter()
    }
    fn io(&self) -> impl Iterator<Item = StarstreamIO> {
        [
            StarstreamIO::n_utxos,
            StarstreamIO::this_utxo_plus_one,
            StarstreamIO::next_free_channel,
            StarstreamIO::pc,
            StarstreamIO::sp,
            StarstreamIO::lp,
            StarstreamIO::ap,
        ]
        .into_iter()
    }
    // rustc bug
    #[allow(private_bounds)]
    fn run<Var: CircuitBuilderVar, B: Builder<Var>>(&self, branch: Branch, mut cb: B) {
        match branch {
            Branch::visit_nop => visit_nop(cb.nest(l!())),
            Branch::visit_drop => visit_drop(cb.nest(l!())),
            Branch::visit_const => visit_const(cb.nest(l!())),
            Branch::visit_get_local => visit_get_local(cb.nest(l!())),
            Branch::visit_set_local => visit_set_local(cb.nest(l!())),
            Branch::visit_cond_jump_c_not_0 => visit_cond_jump_c_not_0(cb.nest(l!())),
            Branch::visit_cond_jump_c_0 => visit_cond_jump_c_0(cb.nest(l!())),
            Branch::visit_jump => visit_jump(cb.nest(l!())),
            Branch::visit_read8 => visit_read8(cb.nest(l!())),
            Branch::visit_write8 => visit_write8(cb.nest(l!())),
            Branch::visit_read16 => visit_read16(cb.nest(l!())),
            Branch::visit_write16 => visit_write16(cb.nest(l!())),
            Branch::visit_read32 => visit_read32(cb.nest(l!())),
            Branch::visit_write32 => visit_write32(cb.nest(l!())),
            Branch::visit_select_c_not_0 => visit_select_c_not_0(cb.nest(l!())),
            Branch::visit_select_c_0 => visit_select_c_0(cb.nest(l!())),
            Branch::visit_push_aux => visit_push_aux(cb.nest(l!())),
            Branch::visit_pop_aux => visit_pop_aux(cb.nest(l!())),
            Branch::visit_add => visit_add(cb.nest(l!())),
            Branch::visit_mul => visit_mul(cb.nest(l!())),
            Branch::visit_neg => visit_neg(cb.nest(l!())),
            Branch::visit_starstream_read_input => visit_starstream_read_input(cb.nest(l!())),
            Branch::visit_starstream_write_output => visit_starstream_write_output(cb.nest(l!())),
            Branch::visit_starstream_get_input_length => {
                visit_starstream_get_input_length(cb.nest(l!()))
            }
            Branch::visit_starstream_suspend => visit_starstream_suspend(cb.nest(l!())),
            Branch::visit_starstream_connect => visit_starstream_connect(cb.nest(l!())),
            Branch::visit_starstream_sync => visit_starstream_sync(cb.nest(l!())),
            Branch::visit_starstream_swap => visit_starstream_swap(cb.nest(l!())),
            Branch::visit_starstream_handshake_any => visit_starstream_handshake_any(cb.nest(l!())),
            Branch::visit_starstream_handshake_self => {
                visit_starstream_handshake_self(cb.nest(l!()))
            }
            Branch::visit_starstream_handshake => visit_starstream_handshake(cb.nest(l!())),
            Branch::visit_starstream_ping => visit_starstream_ping(cb.nest(l!())),
            Branch::visit_starstream_pong => visit_starstream_pong(cb.nest(l!())),
            Branch::visit_starstream_connect_own => visit_starstream_connect_own(cb.nest(l!())),
            Branch::visit_starstream_swap_own => visit_starstream_swap_own(cb.nest(l!())),
            Branch::visit_starstream_close => visit_starstream_close(cb.nest(l!())),
            Branch::visit_witness => visit_witness(cb.nest(l!())),
            Branch::visit_starstream_enter(paused_or_suspended) => {
                visit_starstream_enter(cb.nest(l!()), paused_or_suspended)
            }
            Branch::visit_pair_connecting => visit_pair_connecting(cb.nest(l!())),
            Branch::visit_pair_syncing => visit_pair_syncing(cb.nest(l!())),
            Branch::visit_pair_swapping => visit_pair_swapping(cb.nest(l!())),
            Branch::visit_pair_handshaking { left_is_any } => {
                visit_pair_handshaking(cb.nest(l!()), left_is_any)
            }
            Branch::visit_pair_ping_pong => visit_pair_ping_pong(cb.nest(l!())),
        }
    }
}
