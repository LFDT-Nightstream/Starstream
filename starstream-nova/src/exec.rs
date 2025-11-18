// take a list of WASM modules, hash each to give them names,
// then the entry point,
// execute that to generate a transaction and a corresponding dummy proof

use std::ops::{BitAnd, Shr};

use crate::{
    circuits::{Branch, Instr},
    interface::const_hash_str,
};

pub struct State {
    pub memory: Vec<i32>,
    pub stack: Vec<i32>,
    pub helper_stack: Vec<i32>,
    pub pc: i32,
    pub cc: i32,
    pub reg: i32,
    // FIXME: use enum, and shouldn't be pub
    pub host_call: i32,
    pub host_args: Vec<i32>,
    pub getting_from_host: bool,
}

const fn tag(s: &str) -> u64 {
    const_hash_str(s)
}

const fn cons_tag(head: &str, tail: u64) -> u64 {
    const C: u64 = 6872849445662101858;
    let head = tag(head);
    tail.wrapping_mul(C).wrapping_add(head)
}

pub struct Witnesses {
    pub v: Vec<(u64, i32, i32)>,
    pub branch: Branch,
}

/// Execute one step of the VM and generate the witnesses for the
/// part of the switchboard executed. The usize returned indicates
/// the offset into the array of witnesses. All other witnesses except
/// the global witnesses are to be 0. The global witnesses can be
/// calculated trivially by the caller given the old and the new state.
pub fn step(
    code: &[i32],
    state: &mut State,
    mut host: impl FnMut(i32, Vec<i32>) -> Vec<i32>,
) -> Witnesses {
    let opcode = code[state.pc as usize];
    match opcode {
        o if o == Instr::Unreachable as i32 => panic!("code jumped to unreachable area"),
        o if o == Instr::Nop as i32 => {
            eprintln!("executing Nop");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_nop"))
            }
            state.pc += 1;
            let v = vec![(t("switch"), 1, 1)];
            Witnesses {
                v,
                branch: Branch::Nop,
            }
        }
        o if o == Instr::Drop as i32 => {
            eprintln!("executing Drop");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_drop"))
            }
            state.pc += 1;
            let popped = state.stack.pop().expect("empty stack");
            let v = vec![
                (t("switch"), 1, 1),
                (t("prev"), popped, 1),
                (t("real_addr"), state.stack.len() as i32, 1),
                (t("real_old"), popped, 1),
            ];
            Witnesses {
                v,
                branch: Branch::Drop,
            }
        }
        o if o == Instr::Const as i32 => {
            eprintln!("executing Const");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_const"))
            }
            let pushed = code[state.pc as usize + 1];
            let sp = state.stack.len();
            state.stack.push(pushed);
            state.pc += 2;
            let v = vec![
                (t("switch"), 1, 1),
                (t("real_addr"), sp as i32, 1),
                (t("real_new"), pushed, 1),
            ];
            Witnesses {
                v,
                branch: Branch::Const,
            }
        }
        o if o == Instr::Add as i32 => {
            eprintln!("executing Add");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_add"))
            }
            let sp = state.stack.len() as i32;
            let first = state.stack.pop().expect("empty stack");
            let second = state.stack.pop().expect("empty stack");
            state.stack.push(first + second);
            state.pc += 1;
            let r = (first + second) as u32;
            let v: Vec<_> = [
                (t("switch"), 1, 1),
                (t("first"), first, 1),
                (t("second"), second, 1),
            ]
            .into_iter()
            .chain((0..32).map(|i: u32| (t("bit"), r.shr(i).bitand(1) as i32, 1)))
            .chain(
                [
                    (
                        t("last_bit"),
                        if r < first as u32 || r < second as u32 {
                            1
                        } else {
                            0
                        },
                        1,
                    ),
                    (t("real_addr"), sp - 1, 1),
                    (t("real_old"), first, 1),
                    (t("real_addr"), sp - 2, 1),
                    (t("real_old"), second, 1),
                    (t("real_new"), first + second, 1),
                ]
                .into_iter(),
            )
            .collect();
            Witnesses {
                v,
                branch: Branch::OpAdd,
            }
        }
        o if o == Instr::Get as i32 => {
            eprintln!("executing Get");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_get"))
            }
            let idx = code[state.pc as usize + 1];
            let sp = state.stack.len() as i32;
            let val = state.stack[(sp - idx) as usize];
            state.stack.push(val);
            state.pc += 2;
            let v = vec![
                (t("switch"), 1, 1),
                (t("value"), val, 1),
                (t("real_addr"), sp - idx, 1),
                (t("real_old"), val, 1),
                (t("real_new"), val, 1),
                (t("real_addr"), sp, 1),
                (t("real_new"), val, 1),
            ];
            Witnesses {
                v,
                branch: Branch::Get,
            }
        }
        o if o == Instr::Set as i32 => {
            eprintln!("executing Set");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_set"))
            }
            let idx = code[state.pc as usize + 1];
            let sp = state.stack.len() as i32;
            let old = state.stack[(sp - idx) as usize];
            let new = state.stack.pop().expect("empty stack");
            state.stack[(sp - idx) as usize] = new;
            state.pc += 2;
            let v = vec![
                (t("switch"), 1, 1),
                (t("old"), old, 1),
                (t("new"), new, 1),
                (t("real_addr"), sp - 1, 1),
                (t("real_old"), new, 1),
                (t("real_addr"), sp - idx, 1),
                (t("real_old"), old, 1),
                (t("real_new"), new, 1),
            ];
            Witnesses {
                v,
                branch: Branch::Set,
            }
        }
        o if o == Instr::Swap as i32 => {
            eprintln!("executing Swap");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_swap"))
            }
            let idx = code[state.pc as usize + 1];
            let sp = state.stack.len() as i32;
            let x = state.stack.pop().expect("empty stack");
            let y = state.stack[sp as usize - idx as usize];
            state.stack[sp as usize - idx as usize] = x;
            state.stack.push(y);
            state.pc += 2;
            let v = vec![
                (t("switch"), 1, 1),
                (t("x"), x, 1),
                (t("y"), y, 1),
                (t("real_addr"), sp - 1, 1),
                (t("real_old"), x, 1),
                (t("real_new"), y, 1),
                (t("real_addr"), sp - idx, 1),
                (t("real_old"), y, 1),
                (t("real_new"), x, 1),
            ];
            Witnesses {
                v,
                branch: Branch::Swap,
            }
        }
        o if o == Instr::CondJump as i32 => {
            eprintln!("executing CondJump");
            let sp = state.stack.len() as i32;
            let new_pc = state.stack.pop().expect("empty stack");
            let c = state.stack.pop().expect("empty stack");
            if c == 0 {
                const fn t(l: &str) -> u64 {
                    cons_tag(l, tag("visit_cond_jump_c_0"))
                }
                let v = vec![
                    (t("switch"), 1, 1),
                    (t("real_addr"), sp - 2, 1),
                    (t("real_addr"), sp - 1, 1),
                    (t("real_old"), new_pc, 1),
                ];
                state.pc = new_pc;
                Witnesses {
                    v,
                    branch: Branch::CondJumpC0,
                }
            } else {
                const fn t(l: &str) -> u64 {
                    cons_tag(l, tag("visit_cond_jump_c_not_0"))
                }
                let v = vec![
                    (t("switch"), 1, 1),
                    (t("unused_pc"), new_pc, 1),
                    (t("c"), c, 1),
                    (t("c_inv"), 1, c),
                    (t("real_addr"), sp - 2, 1),
                    (t("real_old"), c, 1),
                    (t("real_addr"), sp - 1, 1),
                    (t("real_old"), new_pc, 1),
                ];
                state.pc += 1;
                Witnesses {
                    v,
                    branch: Branch::CondJumpCNot0,
                }
            }
        }
        o if o == Instr::Jump as i32 => {
            eprintln!("executing Jump");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_jump"))
            }
            let sp = state.stack.len() as i32;
            let new_pc = state.stack.pop().expect("empty stack");
            let v = vec![
                (t("switch"), 1, 1),
                (t("real_addr"), sp - 1, 1),
                (t("real_old"), new_pc, 1),
            ];
            state.pc = new_pc;
            Witnesses {
                v,
                branch: Branch::Jump,
            }
        }
        o if o == Instr::Read as i32 => {
            eprintln!("executing Read");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_read"))
            }
            let sp = state.stack.len() as i32;
            let address = state.stack.pop().expect("empty stack");
            let value = *state.memory.get(address as usize).unwrap_or(&0);
            state.stack.push(value);
            let v = vec![
                (t("switch"), 1, 1),
                (t("address"), address, 1),
                (t("value"), value, 1),
                (t("real_addr"), address, 1),
                (t("real_old"), value, 1),
                (t("real_new"), value, 1),
                (t("real_addr"), sp - 1, 1),
                (t("real_old"), address, 1),
                (t("real_new"), value, 1),
            ];
            state.pc += 1;
            Witnesses {
                v,
                branch: Branch::Read,
            }
        }
        o if o == Instr::Write as i32 => {
            eprintln!("executing Write");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_write"))
            }
            let sp = state.stack.len() as i32;
            let new = state.stack.pop().expect("empty stack");
            let address = state.stack.pop().expect("empty stack");
            let old = *state.memory.get(address as usize).unwrap_or(&0);
            let v = vec![
                (t("switch"), 1, 1),
                (t("address"), address, 1),
                (t("old"), old, 1),
                (t("new"), new, 1),
                (t("real_addr"), address, 1),
                (t("real_old"), old, 1),
                (t("real_new"), new, 1),
                (t("real_addr"), sp - 1, 1),
                (t("real_old"), new, 1),
                (t("real_addr"), sp - 2, 1),
                (t("real_old"), address, 1),
            ];
            state.pc += 1;
            if state.memory.len() <= address as usize {
                state.memory.resize(address as usize + 1, 0);
            }
            state.memory[address as usize] = new;
            Witnesses {
                v,
                branch: Branch::Write,
            }
        }
        o if o == Instr::Alloc as i32 => {
            eprintln!("executing Alloc");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_alloc"))
            }
            let count = code[state.pc as usize + 1];
            state.stack.resize(state.stack.len() + count as usize, 0);
            state.pc += 2;
            let v = vec![(t("switch"), 1, 1)];
            Witnesses {
                v,
                branch: Branch::Alloc,
            }
        }
        o if o == Instr::Select as i32 => {
            eprintln!("executing Select");
            let sp = state.stack.len() as i32;
            let c = state.stack.pop().expect("empty stack");
            let y = state.stack.pop().expect("empty stack");
            let x = state.stack.pop().expect("empty stack");
            if c == 0 {
                const fn t(l: &str) -> u64 {
                    cons_tag(l, tag("visit_select_c_0"))
                }
                let v = vec![
                    (t("switch"), 1, 1),
                    (t("x"), x, 1),
                    (t("y"), y, 1),
                    (t("real_addr"), sp - 3, 1),
                    (t("real_old"), x, 1),
                    (t("real_new"), y, 1),
                    (t("real_addr"), sp - 2, 1),
                    (t("real_old"), y, 1),
                    (t("real_addr"), sp - 1, 1),
                ];
                state.stack.push(x);
                state.pc += 1;
                Witnesses {
                    v,
                    branch: Branch::SelectC0,
                }
            } else {
                const fn t(l: &str) -> u64 {
                    cons_tag(l, tag("visit_select_c_not_0"))
                }
                let v = vec![
                    (t("switch"), 1, 1),
                    (t("c"), c, 1),
                    (t("c_inv"), 1, c),
                    (t("y"), y, 1),
                    (t("real_addr"), sp - 2, 1),
                    (t("real_old"), y, 1),
                    (t("real_addr"), sp - 1, 1),
                    (t("real_old"), c, 1),
                ];
                state.stack.push(y);
                state.pc += 1;
                Witnesses {
                    v,
                    branch: Branch::SelectCNot0,
                }
            }
        }
        o if o == Instr::SetReg as i32 => {
            eprintln!("executing SetReg");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_set_reg"))
            }
            let sp = state.stack.len() as i32;
            let val = state.stack.pop().expect("empty stack");
            let v = vec![
                (t("switch"), 1, 1),
                (t("val"), val, 1),
                (t("real_addr"), sp - 1, 1),
                (t("real_old"), val, 1),
            ];
            state.reg = val;
            state.pc += 1;
            Witnesses {
                v,
                branch: Branch::SetReg,
            }
        }
        o if o == Instr::GetReg as i32 => {
            eprintln!("executing GetReg");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_get_reg"))
            }
            let sp = state.stack.len() as i32;
            state.stack.push(state.reg);
            let v = vec![
                (t("switch"), 1, 1),
                (t("val"), state.reg, 1),
                (t("real_addr"), sp, 1),
                (t("real_new"), state.reg, 1),
            ];
            state.pc += 1;
            Witnesses {
                v,
                branch: Branch::GetReg,
            }
        }
        o if o == Instr::ToHelper as i32 => {
            eprintln!("executing ToHelper");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_to_helper"))
            }
            let sp = state.stack.len() as i32;
            let helper_sp = state.helper_stack.len() as i32;
            let val = state.stack.pop().expect("empty stack");
            state.helper_stack.push(val);
            let v = vec![
                (t("switch"), 1, 1),
                (t("val"), val, 1),
                (t("real_addr"), sp - 1, 1),
                (t("real_old"), val, 1),
                (t("real_addr"), helper_sp, 1),
                (t("real_new"), val, 1),
            ];
            state.pc += 1;
            Witnesses {
                v,
                branch: Branch::ToHelper,
            }
        }
        o if o == Instr::FromHelper as i32 => {
            eprintln!("executing FromHelper");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_from_helper"))
            }
            let sp = state.stack.len() as i32;
            let helper_sp = state.helper_stack.len() as i32;
            let val = state.helper_stack.pop().expect("empty stack");
            state.stack.push(val);
            let v = vec![
                (t("switch"), 1, 1),
                (t("val"), val, 1),
                (t("real_addr"), helper_sp - 1, 1),
                (t("real_old"), val, 1),
                (t("real_addr"), sp, 1),
                (t("real_new"), val, 1),
            ];
            state.pc += 1;
            Witnesses {
                v,
                branch: Branch::FromHelper,
            }
        }
        o if o == Instr::InitHostCall as i32 => {
            eprintln!("executing InitHostCall");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_init_host_call"))
            }
            let host_call = code[state.pc as usize + 1];
            let v = vec![
                (t("switch"), 1, 1),
                (t("real_addr"), state.cc, 1),
                (t("real_val"), host_call, 1),
            ];
            state.cc += 1;
            state.pc += 2;
            state.host_call = host_call;
            state.host_args = Vec::new();
            state.getting_from_host = false;
            Witnesses {
                v,
                branch: Branch::InitHostCall,
            }
        }
        o if o == Instr::ToHost as i32 => {
            eprintln!("executing ToHost");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_to_host"))
            }
            let sp = state.stack.len() as i32;
            let val = state.stack.pop().expect("empty stack");
            let v = vec![
                (t("switch"), 1, 1),
                (t("val"), val, 1),
                (t("real_addr"), sp - 1, 1),
                (t("real_old"), val, 1),
                (t("real_addr"), state.cc, 1),
                (t("real_val"), val, 1),
            ];
            state.pc += 1;
            state.cc += 1;
            state.host_args.push(val);
            Witnesses {
                v,
                branch: Branch::ToHost,
            }
        }
        o if o == Instr::FromHost as i32 => {
            eprintln!("executing FromHost");
            const fn t(l: &str) -> u64 {
                cons_tag(l, tag("visit_from_host"))
            }
            let sp = state.stack.len() as i32;
            if !state.getting_from_host {
                state.getting_from_host = true;
                let host_args = std::mem::take(&mut state.host_args);
                state.host_args = host(state.host_call, host_args);
            }
            let val = state
                .host_args
                .pop()
                .expect("host call didn't return enough values");
            state.stack.push(val);
            let v = vec![
                (t("switch"), 1, 1),
                (t("val"), val, 1),
                (t("real_addr"), sp, 1),
                (t("real_new"), val, 1),
                (t("real_addr"), state.cc, 1),
                (t("real_val"), val, 1),
            ];
            state.pc += 1;
            state.cc += 1;
            Witnesses {
                v,
                branch: Branch::FromHost,
            }
        }
        o if o >= Instr::_End as i32 => {
            panic!("invalid instruction")
        }
        _ => unreachable!("rustc can't tell this won't happen"),
    }
}
