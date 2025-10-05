use crate::memory::{self, Address, IVCMemory};
use crate::{F, Instruction, UtxoChange, UtxoId, memory::IVCMemoryAllocated};
use ark_ff::AdditiveGroup as _;
use ark_r1cs_std::alloc::AllocationMode;
use ark_r1cs_std::{
    GR1CSVar as _, alloc::AllocVar as _, eq::EqGadget, fields::fp::FpVar, prelude::Boolean,
};
use ark_relations::{
    gr1cs::{ConstraintSystemRef, LinearCombination, SynthesisError, Variable},
    ns,
};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::marker::PhantomData;

/// The RAM part is an array of ProgramState
pub const RAM_SEGMENT: u64 = 9u64;
/// Utxos don't have contiguous ids, so we use these to map ids to contiguous
/// addresses.
pub const UTXO_INDEX_MAPPING_SEGMENT: u64 = 10u64;
/// The expected output for each utxo.
/// This is public, so the verifier can just set the ROM to the values it
/// expects.
pub const OUTPUT_CHECK_SEGMENT: u64 = 11u64;

pub const PROGRAM_STATE_SIZE: u64 = 4u64;
pub const UTXO_INDEX_MAPPING_SIZE: u64 = 1u64;
pub const OUTPUT_CHECK_SIZE: u64 = 2u64;

pub struct StepCircuitBuilder<M> {
    pub utxos: BTreeMap<UtxoId, UtxoChange>,
    pub ops: Vec<Instruction>,
    write_ops: Vec<(ProgramState, ProgramState)>,
    utxo_order_mapping: HashMap<F, usize>,

    mem: PhantomData<M>,
}

/// common circuit variables to all the opcodes
#[derive(Clone)]
pub struct Wires {
    // irw
    current_program: FpVar<F>,
    utxos_len: FpVar<F>,
    n_finalized: FpVar<F>,

    // switches
    utxo_yield_switch: Boolean<F>,
    yield_resume_switch: Boolean<F>,
    resume_switch: Boolean<F>,
    check_utxo_output_switch: Boolean<F>,
    drop_utxo_switch: Boolean<F>,

    utxo_id: FpVar<F>,
    input: FpVar<F>,
    output: FpVar<F>,

    utxo_read_wires: ProgramStateWires,
    coord_read_wires: ProgramStateWires,

    utxo_write_wires: ProgramStateWires,

    // TODO: for now there can only be a single coordination script, with the
    // address 1.
    //
    // this can be lifted, but it requires a bit of logic.
    coordination_script: FpVar<F>,

    // variables in the ROM part that has the expected 'output' or final state
    // for a utxo
    utxo_final_output: FpVar<F>,
    utxo_final_consumed: FpVar<F>,

    constant_false: Boolean<F>,
    constant_true: Boolean<F>,
    constant_one: FpVar<F>,
}

/// these are the mcc witnesses
#[derive(Clone)]
pub struct ProgramStateWires {
    consumed: FpVar<F>,
    finalized: FpVar<F>,
    input: FpVar<F>,
    output: FpVar<F>,
}

// helper so that we always allocate witnesses in the same order
pub struct PreWires {
    utxo_address: F,

    coord_address: F,

    utxo_id: F,
    input: F,
    output: F,

    // switches
    yield_start_switch: bool,
    yield_end_switch: bool,
    resume_switch: bool,
    check_utxo_output_switch: bool,
    nop_switch: bool,
    drop_utxo_switch: bool,

    irw: InterRoundWires,
}

#[derive(Clone)]
pub struct ProgramState {
    consumed: bool,
    finalized: bool,
    input: F,
    output: F,
}

/// IVC wires (state between steps)
///
/// these get input and output variables
#[derive(Clone)]
pub struct InterRoundWires {
    current_program: F,
    utxos_len: F,
    n_finalized: F,
}

impl ProgramStateWires {
    const CONSUMED: &str = "consumed";
    const FINALIZED: &str = "finalized";
    const INPUT: &str = "input";
    const OUTPUT: &str = "output";

    fn to_var_vec(&self) -> Vec<FpVar<F>> {
        vec![
            self.consumed.clone(),
            self.finalized.clone(),
            self.input.clone(),
            self.output.clone(),
        ]
    }

    fn conditionally_enforce_equal(
        &self,
        other: &Self,
        should_enforce: &Boolean<F>,
        except: HashSet<&'static str>,
    ) -> Result<(), SynthesisError> {
        if !except.contains(Self::CONSUMED) {
            // dbg!(&self.consumed.value().unwrap());
            // dbg!(&other.consumed.value().unwrap());
            self.consumed
                .conditional_enforce_equal(&other.consumed, should_enforce)?;
        }
        if !except.contains(Self::FINALIZED) {
            // dbg!(&self.finalized.value().unwrap());
            // dbg!(&other.finalized.value().unwrap());
            self.finalized
                .conditional_enforce_equal(&other.finalized, should_enforce)?;
        }
        if !except.contains(Self::INPUT) {
            // dbg!(&self.input.value().unwrap());
            // dbg!(&other.input.value().unwrap());
            self.input
                .conditional_enforce_equal(&other.input, should_enforce)?;
        }
        if !except.contains(Self::OUTPUT) {
            // dbg!(&self.output.value().unwrap());
            // dbg!(&other.output.value().unwrap());

            self.output
                .conditional_enforce_equal(&other.output, should_enforce)?;
        }
        Ok(())
    }

    fn from_vec(utxo_read_wires: Vec<FpVar<F>>) -> ProgramStateWires {
        ProgramStateWires {
            consumed: utxo_read_wires[0].clone(),
            finalized: utxo_read_wires[1].clone(),
            input: utxo_read_wires[2].clone(),
            output: utxo_read_wires[3].clone(),
        }
    }

    fn from_write_values(
        cs: ConstraintSystemRef<F>,
        utxo_write_values: &ProgramState,
    ) -> Result<ProgramStateWires, SynthesisError> {
        Ok(ProgramStateWires {
            consumed: FpVar::from(Boolean::new_witness(cs.clone(), || {
                Ok(utxo_write_values.consumed)
            })?),
            finalized: FpVar::from(Boolean::new_witness(cs.clone(), || {
                Ok(utxo_write_values.finalized)
            })?),
            input: FpVar::new_witness(cs.clone(), || Ok(utxo_write_values.input))?,
            output: FpVar::new_witness(cs.clone(), || Ok(utxo_write_values.output))?,
        })
    }
}

impl Wires {
    pub fn from_irw<M: IVCMemoryAllocated<F>>(
        vals: &PreWires,
        rm: &mut M,
        utxo_write_values: &ProgramState,
        coord_write_values: &ProgramState,
    ) -> Result<Wires, SynthesisError> {
        let cs = rm.get_cs();

        // io vars
        let current_program = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.irw.current_program))?;
        let utxos_len = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.irw.utxos_len))?;
        let n_finalized = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.irw.n_finalized))?;

        // switches
        let switches = [
            vals.resume_switch,
            vals.yield_end_switch,
            vals.yield_start_switch,
            vals.check_utxo_output_switch,
            vals.nop_switch,
            vals.drop_utxo_switch,
        ];

        let allocated_switches: Vec<_> = switches
            .iter()
            .map(|val| Boolean::new_witness(cs.clone(), || Ok(*val)).unwrap())
            .collect();

        let [
            resume_switch,
            yield_resume_switch,
            utxo_yield_switch,
            check_utxo_output_switch,
            nop_switch,
            drop_utxo_switch,
        ] = allocated_switches.as_slice()
        else {
            unreachable!()
        };

        // TODO: figure out how to write this with the proper dsl
        // but we only need r1cs anyway.
        cs.enforce_r1cs_constraint(
            || {
                allocated_switches
                    .iter()
                    .fold(LinearCombination::new(), |acc, switch| acc + switch.lc())
                    .clone()
            },
            || LinearCombination::new() + Variable::one(),
            || LinearCombination::new() + Variable::one(),
        )
        .unwrap();

        let utxo_id = FpVar::<F>::new_witness(ns!(cs.clone(), "utxo_id"), || Ok(vals.utxo_id))?;
        let input = FpVar::<F>::new_witness(ns!(cs.clone(), "input"), || Ok(vals.input))?;
        let output = FpVar::<F>::new_witness(ns!(cs.clone(), "output"), || Ok(vals.output))?;

        let utxo_address = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.utxo_address))?;
        let coord_address = FpVar::<F>::new_witness(cs.clone(), || Ok(vals.coord_address))?;

        let coord_read_wires = rm.conditional_read(
            &(yield_resume_switch | utxo_yield_switch),
            &Address {
                addr: coord_address.clone(),
                tag: RAM_SEGMENT,
            },
        )?;

        let coord_read_wires = ProgramStateWires::from_vec(coord_read_wires);

        let utxo_read_wires = rm.conditional_read(
            check_utxo_output_switch,
            &Address {
                addr: utxo_address.clone(),
                tag: RAM_SEGMENT,
            },
        )?;

        let utxo_read_wires = ProgramStateWires::from_vec(utxo_read_wires);

        let utxo_write_wires = ProgramStateWires::from_write_values(cs.clone(), utxo_write_values)?;
        let coord_write_wires =
            ProgramStateWires::from_write_values(cs.clone(), coord_write_values)?;

        let coord_conditional_write_switch = &resume_switch;

        rm.conditional_write(
            coord_conditional_write_switch,
            &Address {
                addr: coord_address.clone(),
                tag: RAM_SEGMENT,
            },
            &coord_write_wires.to_var_vec(),
        )?;

        let utxo_conditional_write_switch =
            utxo_yield_switch | resume_switch | yield_resume_switch | check_utxo_output_switch;

        rm.conditional_write(
            &utxo_conditional_write_switch,
            &Address {
                addr: utxo_address.clone(),
                tag: RAM_SEGMENT,
            },
            &utxo_write_wires.to_var_vec(),
        )?;

        let coordination_script = FpVar::<F>::new_constant(cs.clone(), F::from(1))?;

        let rom_read_wires = rm.conditional_read(
            &!nop_switch,
            &Address {
                addr: (&utxo_address + &utxos_len),
                tag: UTXO_INDEX_MAPPING_SEGMENT,
            },
        )?;

        rom_read_wires[0].enforce_equal(&utxo_id)?;

        let utxo_output_address = &utxo_address + &utxos_len + &utxos_len;

        let utxo_rom_output_read = rm.conditional_read(
            check_utxo_output_switch,
            &Address {
                addr: utxo_output_address,
                tag: OUTPUT_CHECK_SEGMENT,
            },
        )?;

        Ok(Wires {
            current_program,
            utxos_len,
            n_finalized,

            utxo_yield_switch: utxo_yield_switch.clone(),
            yield_resume_switch: yield_resume_switch.clone(),
            resume_switch: resume_switch.clone(),
            check_utxo_output_switch: check_utxo_output_switch.clone(),
            drop_utxo_switch: drop_utxo_switch.clone(),

            utxo_id,
            input,
            output,
            utxo_read_wires,
            coord_read_wires,
            coordination_script,

            utxo_write_wires,

            utxo_final_output: utxo_rom_output_read[0].clone(),
            utxo_final_consumed: utxo_rom_output_read[1].clone(),

            constant_false: Boolean::new_constant(cs.clone(), false)?,
            constant_true: Boolean::new_constant(cs.clone(), true)?,
            constant_one: FpVar::new_constant(cs.clone(), F::from(1))?,
        })
    }
}

impl InterRoundWires {
    pub fn new(rom_offset: F) -> Self {
        InterRoundWires {
            current_program: F::from(1),
            utxos_len: rom_offset,
            n_finalized: F::from(0),
        }
    }

    pub fn update(&mut self, res: Wires) {
        self.current_program = res.current_program.value().unwrap();
        self.utxos_len = res.utxos_len.value().unwrap();
        self.n_finalized = res.n_finalized.value().unwrap();
    }
}

impl Instruction {
    pub fn write_values(
        &self,
        coord_read: Vec<F>,
        utxo_read: Vec<F>,
    ) -> (ProgramState, ProgramState) {
        match &self {
            Instruction::Nop {} => (ProgramState::dummy(), ProgramState::dummy()),
            Instruction::Resume {
                utxo_id: _,
                input,
                output,
            } => {
                let coord = ProgramState {
                    consumed: coord_read[0] == F::from(1),
                    finalized: coord_read[1] == F::from(1),
                    input: *input,
                    output: *output,
                };

                let utxo = ProgramState {
                    consumed: true,
                    finalized: utxo_read[1] == F::from(1),
                    input: utxo_read[2],
                    output: utxo_read[3],
                };

                (coord, utxo)
            }
            Instruction::YieldResume {
                utxo_id: _,
                output: _,
            } => {
                let coord = ProgramState::dummy();

                let utxo = ProgramState {
                    consumed: utxo_read[0] == F::from(1),
                    finalized: utxo_read[1] == F::from(1),
                    input: utxo_read[2],
                    output: utxo_read[3],
                };

                (coord, utxo)
            }
            Instruction::Yield { utxo_id: _, input } => {
                let coord = ProgramState::dummy();

                let utxo = ProgramState {
                    consumed: false,
                    finalized: utxo_read[1] == F::from(1),
                    input: F::from(0),
                    output: *input,
                };

                (coord, utxo)
            }
            Instruction::CheckUtxoOutput { utxo_id: _ } => {
                let coord = ProgramState::dummy();

                let utxo = ProgramState {
                    consumed: utxo_read[0] == F::from(1),
                    finalized: true,
                    input: utxo_read[2],
                    output: utxo_read[3],
                };

                (coord, utxo)
            }
            Instruction::DropUtxo { utxo_id: _ } => {
                let coord = ProgramState::dummy();
                let utxo = ProgramState::dummy();

                (coord, utxo)
            }
        }
    }
}

impl<M: IVCMemory<F>> StepCircuitBuilder<M> {
    pub fn new(utxos: BTreeMap<F, UtxoChange>, ops: Vec<Instruction>) -> Self {
        Self {
            utxos,
            ops,
            write_ops: vec![],
            utxo_order_mapping: Default::default(),

            mem: PhantomData,
        }
    }

    // pub fn dummy(utxos: BTreeMap<F, UtxoChange>) -> Self {
    //     Self {
    //         utxos,
    //         ops: vec![Instruction::Nop {}],
    //         write_ops: vec![],
    //         utxo_order_mapping: Default::default(),

    //         mem: PhantomData,
    //     }
    // }

    pub fn make_step_circuit(
        &self,
        i: usize,
        rm: &mut M::Allocator,
        cs: ConstraintSystemRef<F>,
        mut irw: InterRoundWires,
    ) -> Result<InterRoundWires, SynthesisError> {
        rm.start_step(cs.clone()).unwrap();

        let wires_in = self.allocate_vars(i, rm, &irw)?;
        let next_wires = wires_in.clone();

        // per opcode constraints
        let next_wires = self.visit_utxo_yield(next_wires)?;
        let next_wires = self.visit_utxo_yield_resume(next_wires)?;
        let next_wires = self.visit_utxo_resume(next_wires)?;
        let next_wires = self.visit_utxo_output_check(next_wires)?;
        let next_wires = self.visit_utxo_drop(next_wires)?;

        rm.finish_step(i == self.ops.len() - 1)?;

        // input <-> output mappings are done by modifying next_wires
        ivcify_wires(&cs, &wires_in, &next_wires)?;

        irw.update(next_wires);

        Ok(irw)
    }

    pub fn trace_memory_ops(&mut self, params: <M as memory::IVCMemory<F>>::Params) -> M {
        let utxos_len = self.utxos.len() as u64;
        let (mut mb, utxo_order_mapping) = {
            let mut mb = M::new(params);

            mb.register_mem(RAM_SEGMENT, PROGRAM_STATE_SIZE);
            mb.register_mem(UTXO_INDEX_MAPPING_SEGMENT, UTXO_INDEX_MAPPING_SIZE);
            mb.register_mem(OUTPUT_CHECK_SEGMENT, OUTPUT_CHECK_SIZE);

            let mut utxo_order_mapping: HashMap<F, usize> = Default::default();

            mb.init(
                Address {
                    addr: 1,
                    tag: RAM_SEGMENT,
                },
                ProgramState::dummy().to_field_vec(),
            );

            for (
                index,
                (
                    utxo_id,
                    UtxoChange {
                        output_before,
                        output_after,
                        consumed,
                    },
                ),
            ) in self.utxos.iter().enumerate()
            {
                mb.init(
                    // 0 is not a valid address
                    // 1 is the coordination script
                    // utxos start at 2
                    Address {
                        addr: index as u64 + 2,
                        tag: RAM_SEGMENT,
                    },
                    ProgramState {
                        consumed: false,
                        finalized: false,
                        input: F::from(0),
                        output: *output_before,
                    }
                    .to_field_vec(),
                );

                mb.init(
                    Address {
                        addr: index as u64 + 2 + utxos_len,
                        tag: UTXO_INDEX_MAPPING_SEGMENT,
                    },
                    vec![*utxo_id],
                );

                utxo_order_mapping.insert(*utxo_id, index + 2);

                mb.init(
                    Address {
                        addr: index as u64 + 2 + utxos_len * 2,
                        tag: OUTPUT_CHECK_SEGMENT,
                    },
                    vec![*output_after, F::from(if *consumed { 1 } else { 0 })],
                );
            }

            (mb, utxo_order_mapping)
        };

        let utxos_len = self.utxos.len() as u64;

        self.utxo_order_mapping = utxo_order_mapping;

        // out of circuit memory operations.
        // this is needed to commit to the memory operations before-hand.
        for instr in &self.ops {
            // per step we conditionally:
            //
            // 1. read the coordination script state
            // 2. read a single utxo state
            // 3. write the new coordination script state
            // 4. write the new utxo state (for the same utxo)
            //
            // Aditionally we read from the ROM
            //
            // 5. The expected utxo final state (if the check utxo switch is on).
            // 6. The utxo id mapping.
            //
            // All instructions need to the same number of reads and writes,
            // since these have to allocate witnesses.
            //
            // The witnesses are allocated in Wires::from_irw.
            //
            // Each read or write here needs a corresponding witness in that
            // function, with the same switchboard condition, and the same
            // address.
            let (utxo_id, coord_read_cond, utxo_read_cond, coord_write_cond, utxo_write_cond) =
                match instr {
                    Instruction::Resume { utxo_id, .. } => (*utxo_id, false, false, true, true),
                    Instruction::YieldResume { utxo_id, .. }
                    | Instruction::Yield { utxo_id, .. } => (*utxo_id, true, false, false, true),
                    Instruction::CheckUtxoOutput { utxo_id } => {
                        (*utxo_id, false, true, false, true)
                    }
                    Instruction::Nop {} => (F::from(0), false, false, false, false),
                    Instruction::DropUtxo { utxo_id } => (*utxo_id, false, false, false, false),
                };

            let utxo_addr = *self.utxo_order_mapping.get(&utxo_id).unwrap_or(&2);

            let coord_read = mb.conditional_read(
                coord_read_cond,
                Address {
                    addr: 1,
                    tag: RAM_SEGMENT,
                },
            );
            let utxo_read = mb.conditional_read(
                utxo_read_cond,
                Address {
                    addr: utxo_addr as u64,
                    tag: RAM_SEGMENT,
                },
            );

            let (coord_write, utxo_write) = instr.write_values(coord_read, utxo_read);

            self.write_ops
                .push((coord_write.clone(), utxo_write.clone()));

            mb.conditional_write(
                coord_write_cond,
                Address {
                    addr: 1,
                    tag: RAM_SEGMENT,
                },
                coord_write.to_field_vec(),
            );
            mb.conditional_write(
                utxo_write_cond,
                Address {
                    addr: utxo_addr as u64,
                    tag: RAM_SEGMENT,
                },
                utxo_write.to_field_vec(),
            );

            mb.conditional_read(
                !matches!(instr, Instruction::Nop {}),
                Address {
                    addr: utxo_addr as u64 + utxos_len,
                    tag: UTXO_INDEX_MAPPING_SEGMENT,
                },
            );

            mb.conditional_read(
                matches!(instr, Instruction::CheckUtxoOutput { .. }),
                Address {
                    addr: utxo_addr as u64 + utxos_len * 2,
                    tag: OUTPUT_CHECK_SEGMENT,
                },
            );
        }

        mb
    }

    fn allocate_vars(
        &self,
        i: usize,
        rm: &mut M::Allocator,
        irw: &InterRoundWires,
    ) -> Result<Wires, SynthesisError> {
        let instruction = &self.ops[i];
        let (coord_write, utxo_write) = &self.write_ops[i];

        match instruction {
            Instruction::Nop {} => {
                let irw = PreWires {
                    nop_switch: true,
                    irw: irw.clone(),

                    // the first utxo has address 2
                    //
                    // this doesn't matter since the read is unconditionally
                    // false, it's just for padding purposes
                    utxo_address: F::from(2_u64),

                    ..PreWires::new(irw.clone())
                };

                Wires::from_irw(&irw, rm, utxo_write, coord_write)
            }
            Instruction::Resume {
                utxo_id,
                input,
                output,
            } => {
                let utxo_addr = *self.utxo_order_mapping.get(utxo_id).unwrap();

                let irw = PreWires {
                    resume_switch: true,

                    utxo_id: *utxo_id,
                    input: *input,
                    output: *output,

                    utxo_address: F::from(utxo_addr as u64),

                    irw: irw.clone(),

                    ..PreWires::new(irw.clone())
                };

                Wires::from_irw(&irw, rm, utxo_write, coord_write)
            }
            Instruction::YieldResume { utxo_id, output } => {
                let utxo_addr = *self.utxo_order_mapping.get(utxo_id).unwrap();

                let irw = PreWires {
                    yield_end_switch: true,

                    utxo_id: *utxo_id,
                    output: *output,

                    utxo_address: F::from(utxo_addr as u64),

                    irw: irw.clone(),

                    ..PreWires::new(irw.clone())
                };

                Wires::from_irw(&irw, rm, utxo_write, coord_write)
            }
            Instruction::Yield { utxo_id, input } => {
                let utxo_addr = *self.utxo_order_mapping.get(utxo_id).unwrap();

                let irw = PreWires {
                    yield_start_switch: true,
                    utxo_id: *utxo_id,
                    input: *input,
                    utxo_address: F::from(utxo_addr as u64),
                    irw: irw.clone(),

                    ..PreWires::new(irw.clone())
                };

                Wires::from_irw(&irw, rm, utxo_write, coord_write)
            }
            Instruction::CheckUtxoOutput { utxo_id } => {
                let utxo_addr = *self.utxo_order_mapping.get(utxo_id).unwrap();

                let irw = PreWires {
                    check_utxo_output_switch: true,
                    utxo_id: *utxo_id,
                    utxo_address: F::from(utxo_addr as u64),
                    irw: irw.clone(),
                    ..PreWires::new(irw.clone())
                };

                Wires::from_irw(&irw, rm, utxo_write, coord_write)
            }
            Instruction::DropUtxo { utxo_id } => {
                let utxo_addr = *self.utxo_order_mapping.get(utxo_id).unwrap();

                let irw = PreWires {
                    drop_utxo_switch: true,
                    utxo_id: *utxo_id,
                    utxo_address: F::from(utxo_addr as u64),
                    irw: irw.clone(),
                    ..PreWires::new(irw.clone())
                };

                Wires::from_irw(&irw, rm, utxo_write, coord_write)
            }
        }
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_utxo_resume(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.resume_switch;

        wires.utxo_read_wires.conditionally_enforce_equal(
            &wires.utxo_write_wires,
            switch,
            [ProgramStateWires::CONSUMED].into_iter().collect(),
        )?;

        wires
            .current_program
            .conditional_enforce_equal(&wires.coordination_script, switch)?;

        wires
            .utxo_write_wires
            .consumed
            .conditional_enforce_equal(&FpVar::from(wires.constant_true.clone()), switch)?;

        wires.current_program = switch.select(&wires.utxo_id, &wires.current_program)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_utxo_drop(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.drop_utxo_switch;

        wires.utxo_read_wires.conditionally_enforce_equal(
            &wires.utxo_write_wires,
            switch,
            [].into_iter().collect(),
        )?;

        wires
            .current_program
            .conditional_enforce_equal(&wires.utxo_id, switch)?;

        wires.current_program =
            switch.select(&wires.coordination_script, &wires.current_program)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_utxo_yield_resume(&self, wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.yield_resume_switch;

        wires.utxo_read_wires.conditionally_enforce_equal(
            &wires.utxo_write_wires,
            switch,
            [].into_iter().collect(),
        )?;

        wires
            .coord_read_wires
            .input
            .conditional_enforce_equal(&wires.output, switch)?;

        wires
            .current_program
            .conditional_enforce_equal(&wires.utxo_id, switch)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_utxo_yield(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.utxo_yield_switch;

        wires.utxo_read_wires.conditionally_enforce_equal(
            &wires.utxo_write_wires,
            switch,
            [
                ProgramStateWires::CONSUMED,
                ProgramStateWires::OUTPUT,
                ProgramStateWires::INPUT,
            ]
            .into_iter()
            .collect(),
        )?;

        wires
            .utxo_write_wires
            .consumed
            .conditional_enforce_equal(&FpVar::from(wires.constant_false.clone()), switch)?;

        wires
            .coord_read_wires
            .output
            .conditional_enforce_equal(&wires.input, switch)?;

        wires
            .current_program
            .conditional_enforce_equal(&wires.utxo_id, switch)?;

        wires.current_program =
            switch.select(&wires.coordination_script, &wires.current_program)?;

        Ok(wires)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, wires))]
    fn visit_utxo_output_check(&self, mut wires: Wires) -> Result<Wires, SynthesisError> {
        let switch = &wires.check_utxo_output_switch;

        wires.utxo_read_wires.conditionally_enforce_equal(
            &wires.utxo_write_wires,
            switch,
            [ProgramStateWires::FINALIZED].into_iter().collect(),
        )?;

        wires
            .current_program
            .conditional_enforce_equal(&wires.coordination_script, switch)?;

        // utxo.output = expected.output
        wires
            .utxo_read_wires
            .output
            .conditional_enforce_equal(&wires.utxo_final_output, switch)?;

        // utxo.consumed = expected.consumed
        wires
            .utxo_read_wires
            .consumed
            .conditional_enforce_equal(&wires.utxo_final_consumed, switch)?;

        // utxo.finalized = true;
        wires
            .utxo_write_wires
            .finalized
            .enforce_equal(&FpVar::from(switch.clone()))?;

        // Check that we don't have duplicated entries. Otherwise the
        // finalization counter (n_finalized) will have the right value at the
        // end, but not all the utxo states will be verified.
        wires
            .utxo_read_wires
            .finalized
            .conditional_enforce_equal(&FpVar::from(wires.constant_false.clone()), switch)?;

        // n_finalized += 1;
        wires.n_finalized = switch.select(
            &(&wires.n_finalized + &wires.constant_one),
            &wires.n_finalized,
        )?;

        Ok(wires)
    }

    pub(crate) fn rom_offset(&self) -> F {
        F::from(self.utxos.len() as u64)
    }
}

fn ivcify_wires(
    cs: &ConstraintSystemRef<F>,
    wires_in: &Wires,
    wires_out: &Wires,
) -> Result<(), SynthesisError> {
    let (current_program_in, current_program_out) = {
        let f_in = || wires_in.current_program.value();
        let f_out = || wires_out.current_program.value();
        let alloc_in = FpVar::new_variable(cs.clone(), f_in, AllocationMode::Input)?;
        let alloc_out = FpVar::new_variable(cs.clone(), f_out, AllocationMode::Input)?;

        Ok((alloc_in, alloc_out))
    }?;

    wires_in
        .current_program
        .enforce_equal(&current_program_in)?;
    wires_out
        .current_program
        .enforce_equal(&current_program_out)?;

    let (current_rom_offset_in, current_rom_offset_out) = {
        let f_in = || wires_in.utxos_len.value();
        let f_out = || wires_out.utxos_len.value();
        let alloc_in = FpVar::new_variable(cs.clone(), f_in, AllocationMode::Input)?;
        let alloc_out = FpVar::new_variable(cs.clone(), f_out, AllocationMode::Input)?;

        Ok((alloc_in, alloc_out))
    }?;

    wires_in.utxos_len.enforce_equal(&current_rom_offset_in)?;
    wires_out.utxos_len.enforce_equal(&current_rom_offset_out)?;

    current_rom_offset_in.enforce_equal(&current_rom_offset_out)?;

    let (current_n_finalized_in, current_n_finalized_out) = {
        let cs = cs.clone();
        let f_in = || wires_in.n_finalized.value();
        let f_out = || wires_out.n_finalized.value();
        let alloc_in = FpVar::new_variable(cs.clone(), f_in, AllocationMode::Input)?;
        let alloc_out = FpVar::new_variable(cs.clone(), f_out, AllocationMode::Input)?;

        Ok((alloc_in, alloc_out))
    }?;

    wires_in
        .n_finalized
        .enforce_equal(&current_n_finalized_in)?;
    wires_out
        .n_finalized
        .enforce_equal(&current_n_finalized_out)?;

    Ok(())
}

impl PreWires {
    pub fn new(irw: InterRoundWires) -> Self {
        Self {
            utxo_address: F::ZERO,

            coord_address: F::from(1),

            // transcript vars
            utxo_id: F::ZERO,
            input: F::ZERO,
            output: F::ZERO,

            // switches
            yield_start_switch: false,
            yield_end_switch: false,
            resume_switch: false,
            check_utxo_output_switch: false,
            nop_switch: false,
            drop_utxo_switch: false,

            // io vars
            irw,
        }
    }
}

impl ProgramState {
    pub fn dummy() -> Self {
        Self {
            consumed: false,
            finalized: false,
            input: F::ZERO,
            output: F::ZERO,
        }
    }

    fn to_field_vec(&self) -> Vec<F> {
        vec![
            if self.consumed {
                F::from(1)
            } else {
                F::from(0)
            },
            if self.finalized {
                F::from(1)
            } else {
                F::from(0)
            },
            self.input,
            self.output,
        ]
    }
}
