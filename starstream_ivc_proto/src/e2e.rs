//* Dummy UTXO VM **just** for testing and to illustrate the data flow. It's not
//* trying to be a zkvm, nor a wasm-like vm.

use crate::{LedgerOperation, ProgramId, Transaction, UtxoChange, neo::ark_field_to_p3_goldilocks};
use ark_ff::PrimeField;
use neo_ajtai::{Commitment, PP, commit, decomp_b, setup};
use neo_ccs::crypto::poseidon2_goldilocks::poseidon2_hash;
use neo_math::ring::Rq as RqEl;
use p3_field::PrimeCharacteristicRing;
use p3_goldilocks::Goldilocks;
use rand::rng;
use std::sync::OnceLock;
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    rc::Rc,
};

// TODO: this should be a parameter
static AJTAI_PP: OnceLock<PP<RqEl>> = OnceLock::new();

fn get_ajtai_pp() -> &'static PP<RqEl> {
    AJTAI_PP.get_or_init(|| {
        let mut rng = rng();
        let d = neo_math::ring::D; // ring dimension
        let kappa = 128; // security parameter
        let m = 4; // vector length
        setup(&mut rng, d, kappa, m).expect("Failed to setup Ajtai commitment")
    })
}

fn incremental_commit(value1: [Goldilocks; 4], value2: [Goldilocks; 4]) -> [Goldilocks; 4] {
    let input = [value1, value2].concat();

    poseidon2_hash(&input)
}

// TODO: review this, there may be a more efficient conversion
// this is 864 hashes per step
// the important part is that it would have to be done in the circuit too, so review this
fn ajtai_commitment_to_goldilocks(commitment: &Commitment) -> [Goldilocks; 4] {
    let mut result = [Goldilocks::ZERO; 4];

    for chunk in commitment.data.chunks(4) {
        let input = [
            result[0], result[1], result[2], result[3], chunk[0], chunk[1], chunk[2], chunk[3],
        ];

        result = poseidon2_hash(&input);
    }

    result
}

fn block_commitment(
    op_tag: u64,
    utxo_id: crate::F,
    input: crate::F,
    output: crate::F,
) -> [Goldilocks; 4] {
    let z = vec![
        ark_field_to_p3_goldilocks(&crate::F::from(op_tag)),
        ark_field_to_p3_goldilocks(&utxo_id),
        ark_field_to_p3_goldilocks(&input),
        ark_field_to_p3_goldilocks(&output),
    ];

    let b = 2;
    let decomp_b = decomp_b(&z, b, neo_math::ring::D, neo_ajtai::DecompStyle::Balanced);

    let commitment = commit(get_ajtai_pp(), &decomp_b);

    ajtai_commitment_to_goldilocks(&commitment)
}

#[derive(Debug, Clone)]
pub struct IncrementalCommitment {
    commitment: [Goldilocks; 4],
}

impl IncrementalCommitment {
    pub fn new() -> Self {
        Self {
            commitment: [Goldilocks::ZERO; 4],
        }
    }

    pub fn add_operation(&mut self, op: &LedgerOperation<crate::F>) {
        let (tag, utxo_id, input, output) = match op {
            LedgerOperation::Resume {
                utxo_id,
                input,
                output,
            } => (1, *utxo_id, *input, *output),
            LedgerOperation::Yield { utxo_id, input } => (2, *utxo_id, *input, crate::F::from(0)),
            LedgerOperation::YieldResume { utxo_id, output } => {
                (3, *utxo_id, crate::F::from(0), *output)
            }
            LedgerOperation::DropUtxo { utxo_id } => {
                (4, *utxo_id, crate::F::from(0), crate::F::from(0))
            }
            // these are just auxiliary instructions for the proof, not real
            // ledger operations
            // they don't show up in the wasm execution trace
            LedgerOperation::Nop {} => return,
            LedgerOperation::CheckUtxoOutput { utxo_id: _ } => return,
        };

        let op_commitment = block_commitment(tag, utxo_id, input, output);

        self.commitment = incremental_commit(op_commitment, self.commitment);
    }

    pub fn as_field_elements(&self) -> [Goldilocks; 4] {
        self.commitment
    }
}

#[derive(Debug, Clone)]
pub struct ProgramTraceCommitments {
    commitments: HashMap<ProgramId, IncrementalCommitment>,
}

impl ProgramTraceCommitments {
    pub fn new() -> Self {
        Self {
            commitments: HashMap::new(),
        }
    }

    fn add_operation(&mut self, op: &LedgerOperation<crate::F>) {
        let program_id = match op {
            LedgerOperation::Resume {
                utxo_id,
                input: _,
                output: _,
            } => utxo_id,
            LedgerOperation::Yield { utxo_id, input: _ } => utxo_id,
            LedgerOperation::YieldResume { utxo_id, output: _ } => utxo_id,
            LedgerOperation::DropUtxo { utxo_id } => utxo_id,
            LedgerOperation::Nop {} => return,
            LedgerOperation::CheckUtxoOutput { utxo_id: _ } => return,
        };

        self.commitments
            .entry(*program_id)
            .or_insert_with(IncrementalCommitment::new)
            .add_operation(op);
    }

    fn get_all_commitments(&self) -> &HashMap<crate::F, IncrementalCommitment> {
        &self.commitments
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(usize);

type Value = crate::F;

trait BlackBox {
    fn run(&self, state: &mut HashMap<Variable, Value>) -> Option<usize>;
    fn box_clone(&self) -> Box<dyn BlackBox>;
}

impl Clone for Box<dyn BlackBox> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

impl<F> BlackBox for F
where
    F: Fn(&mut HashMap<Variable, Value>) -> Option<usize> + Clone + 'static,
{
    fn run(&self, state: &mut HashMap<Variable, Value>) -> Option<usize> {
        self(state)
    }

    fn box_clone(&self) -> Box<dyn BlackBox> {
        Box::new(self.clone())
    }
}

#[derive(Clone)]
enum Op {
    // pure compuation in the sense that it doesn't interact with the ledger
    // this represents the native operations of the wasm vm.
    Pure {
        f: Box<dyn BlackBox>,
    },
    New {
        initial_state: Value,
        output_var: Variable,
    },
    Yield {
        val: Variable,
    },
    Resume {
        utxo: Variable,
        val: Variable,
    },
    Burn {},
}

impl std::fmt::Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pure { .. } => f.debug_struct("Pure").finish(),
            Self::New {
                initial_state,
                output_var,
            } => f
                .debug_struct("New")
                .field("utxo", initial_state)
                .field("output_var", output_var)
                .finish(),
            Self::Yield { val } => f.debug_struct("Yield").field("val", val).finish(),
            Self::Resume { utxo, val } => f
                .debug_struct("Resume")
                .field("utxo", utxo)
                .field("val", val)
                .finish(),
            Op::Burn {} => f.debug_struct("Burn").finish(),
        }
    }
}

pub struct MockedProgram {
    code: Vec<Op>,
    state: MockedProgramState,
}

pub struct MockedProgramState {
    pc: usize,
    yield_skip: bool,
    thunk: Option<Thunk>,
    vars: HashMap<Variable, Value>,
    // output: Option<Value>,
    input: Option<Value>,
}

impl MockedProgram {
    fn new(code: Vec<Op>, yielded: bool) -> Rc<RefCell<MockedProgram>> {
        Rc::new(RefCell::new(MockedProgram {
            code,
            state: MockedProgramState {
                pc: 0,
                yield_skip: yielded,
                thunk: None,
                vars: HashMap::new(),
                // output: None,
                input: None,
            },
        }))
    }
}

#[derive(Clone, Debug)]
pub struct UtxoState {
    output: crate::F,
    #[allow(dead_code)]
    memory: crate::F,
}

pub struct MockedLedger {
    utxos: BTreeMap<ProgramId, UtxoState>,
}

#[derive(Clone, Debug)]
pub enum Thunk {
    Resolved(crate::F),
    Unresolved(Rc<RefCell<Option<crate::F>>>),
}

impl From<crate::F> for Thunk {
    fn from(f: crate::F) -> Self {
        Thunk::Resolved(f)
    }
}

impl From<&crate::F> for Thunk
where
    crate::F: Clone,
{
    fn from(f: &crate::F) -> Self {
        Thunk::Resolved(*f)
    }
}

impl From<Thunk> for crate::F {
    fn from(thunk: Thunk) -> Self {
        match thunk {
            Thunk::Resolved(f) => f,
            Thunk::Unresolved(maybe_f) => maybe_f.borrow().unwrap(),
        }
    }
}

impl Thunk {
    fn unresolved() -> Self {
        Self::Unresolved(Rc::new(RefCell::new(None)))
    }

    fn resolve(&self, f: crate::F) {
        match self {
            Thunk::Resolved(_fp) => unreachable!("already resolved value"),
            Thunk::Unresolved(unresolved) => (*unresolved.borrow_mut()) = Some(f),
        }
    }
}

impl MockedLedger {
    pub(crate) fn run_mocked_vm(
        &mut self,
        entry_point: Value,
        programs: HashMap<Value, Rc<RefCell<MockedProgram>>>,
    ) -> (
        Transaction<Vec<LedgerOperation<crate::F>>>,
        ProgramTraceCommitments,
    ) {
        let state_pre = self.utxos.clone();

        let mut instructions: Vec<LedgerOperation<Thunk>> = vec![];
        let mut commitments = ProgramTraceCommitments::new();

        let mut current_program = entry_point;
        let mut in_coord = true;
        let mut prev_program: Option<Value> = None;

        let mut consumed = HashSet::new();

        loop {
            let program_state = Rc::clone(programs.get(&current_program).unwrap());

            let opcode = program_state
                .borrow()
                .code
                .get(program_state.borrow().state.pc)
                .cloned();

            if let Some(opcode) = opcode {
                match opcode {
                    Op::Pure { f } => {
                        let new_pc = f.run(&mut program_state.borrow_mut().state.vars);

                        if let Some(new_pc) = new_pc {
                            program_state.borrow_mut().state.pc = new_pc;
                        } else {
                            program_state.borrow_mut().state.pc += 1;
                        }
                    }
                    Op::Yield { val } => {
                        let yield_to = *prev_program.as_ref().unwrap();

                        let yield_val = *program_state.borrow().state.vars.get(&val).unwrap();

                        self.utxos.entry(current_program).and_modify(|state| {
                            state.output = yield_val;
                        });

                        let yield_to_program = programs.get(&yield_to).unwrap();

                        let yield_resume_op = LedgerOperation::YieldResume {
                            utxo_id: current_program.into(),
                            output: yield_to_program.borrow().state.input.unwrap().into(),
                        };
                        let yield_op = LedgerOperation::Yield {
                            utxo_id: current_program.into(),
                            input: program_state.borrow().state.vars.get(&val).unwrap().into(),
                        };

                        instructions.push(yield_resume_op);
                        instructions.push(yield_op);

                        program_state.borrow_mut().state.pc += 1;
                        program_state.borrow_mut().state.yield_skip = false;

                        prev_program.replace(current_program);
                        current_program = yield_to;

                        in_coord = true;
                    }
                    Op::Resume { utxo, val } => {
                        let utxo_id = *(program_state.borrow().state.vars.get(&utxo).unwrap());
                        if !dbg!(program_state.borrow().state.yield_skip) {
                            let input = *(program_state.borrow().state.vars.get(&val).unwrap());

                            program_state.borrow_mut().state.input.replace(input);

                            prev_program.replace(current_program);

                            in_coord = false;
                            current_program = dbg!(utxo_id);

                            program_state.borrow_mut().state.yield_skip = true;

                            let output_thunk = Thunk::unresolved();
                            program_state.borrow_mut().state.thunk = Some(output_thunk.clone());

                            let resume_op = LedgerOperation::Resume {
                                utxo_id: program_state
                                    .borrow()
                                    .state
                                    .vars
                                    .get(&utxo)
                                    .unwrap()
                                    .into(),
                                input: program_state.borrow().state.vars.get(&val).unwrap().into(),
                                output: output_thunk,
                            };

                            instructions.push(resume_op);
                        } else {
                            let output_val = self.utxos.get(&utxo_id).unwrap().output;

                            program_state
                                .borrow()
                                .state
                                .thunk
                                .as_ref()
                                .unwrap()
                                .resolve(output_val);

                            program_state.borrow_mut().state.pc += 1;
                            program_state.borrow_mut().state.yield_skip = false;
                            in_coord = true;
                        }
                    }
                    Op::New {
                        initial_state,
                        output_var,
                    } => {
                        let utxo_id = 2 + self.utxos.len();

                        program_state
                            .borrow_mut()
                            .state
                            .vars
                            .insert(output_var, crate::F::from(utxo_id as u64));

                        self.utxos.insert(
                            ProgramId::from(utxo_id as u64),
                            UtxoState {
                                output: crate::F::from(0),
                                memory: initial_state,
                            },
                        );

                        program_state.borrow_mut().state.pc += 1;

                        assert!(in_coord);
                    }
                    Op::Burn {} => {
                        consumed.insert(current_program);
                        program_state.borrow_mut().state.pc += 1;

                        let drop_op = LedgerOperation::DropUtxo {
                            utxo_id: current_program.into(),
                        };

                        instructions.push(drop_op);

                        let yield_to = *prev_program.as_ref().unwrap();

                        prev_program.replace(current_program);

                        in_coord = true;
                        self.utxos.entry(current_program).and_modify(|state| {
                            state.output = crate::F::from(0);
                        });

                        current_program = yield_to;
                    }
                }
            } else {
                assert!(in_coord);
                break;
            }
        }

        let mut utxo_deltas: BTreeMap<ProgramId, UtxoChange> = Default::default();

        for (utxo_id, state_pos) in &self.utxos {
            let output_before = state_pre
                .get(utxo_id)
                .map(|st| st.output)
                .unwrap_or_default();

            utxo_deltas.insert(
                *utxo_id,
                UtxoChange {
                    output_before,
                    output_after: state_pos.output,
                    consumed: consumed.contains(utxo_id),
                },
            );
        }

        for utxo in consumed {
            self.utxos.remove(&utxo);
        }

        let resolved_instructions: Vec<LedgerOperation<crate::F>> = instructions
            .into_iter()
            .map(|instr| match instr {
                LedgerOperation::Resume {
                    utxo_id,
                    input,
                    output,
                } => LedgerOperation::Resume {
                    utxo_id: utxo_id.into(),
                    input: input.into(),
                    output: output.into(),
                },
                LedgerOperation::Yield { utxo_id, input } => LedgerOperation::Yield {
                    utxo_id: utxo_id.into(),
                    input: input.into(),
                },
                LedgerOperation::YieldResume { utxo_id, output } => LedgerOperation::YieldResume {
                    utxo_id: utxo_id.into(),
                    output: output.into(),
                },
                LedgerOperation::DropUtxo { utxo_id } => LedgerOperation::DropUtxo {
                    utxo_id: utxo_id.into(),
                },
                LedgerOperation::Nop {} => LedgerOperation::Nop {},
                LedgerOperation::CheckUtxoOutput { utxo_id } => LedgerOperation::CheckUtxoOutput {
                    utxo_id: utxo_id.into(),
                },
            })
            .collect();

        for op in resolved_instructions.iter() {
            commitments.add_operation(op);
        }

        (
            Transaction::new_unproven(utxo_deltas, resolved_instructions),
            commitments,
        )
    }
}

pub struct ProgramBuilder {
    ops: Vec<Op>,
    yielded: bool,
    next_var_id: usize,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            yielded: false,
            next_var_id: 0,
        }
    }

    pub fn alloc_var(&mut self) -> Variable {
        let var = Variable(self.next_var_id);
        self.next_var_id += 1;
        var
    }

    pub fn set_var(mut self, var: Variable, value: Value) -> Self {
        self.ops.push(Op::Pure {
            f: Box::new(move |state: &mut HashMap<Variable, Value>| {
                state.insert(var, value);
                None
            }),
        });
        self
    }

    pub fn increment_var(mut self, var: Variable, amount: Value) -> Self {
        self.ops.push(Op::Pure {
            f: Box::new(move |state: &mut HashMap<Variable, Value>| {
                state.entry(var).and_modify(|v| *v += amount);
                None
            }),
        });
        self
    }

    pub fn jump_to(mut self, pc: usize) -> Self {
        self.ops.push(Op::Pure {
            f: Box::new(move |_state: &mut HashMap<Variable, Value>| Some(pc)),
        });
        self
    }

    pub fn new_utxo(mut self, initial_state: Value, output_var: Variable) -> Self {
        self.ops.push(Op::New {
            initial_state,
            output_var,
        });
        self
    }

    pub fn yield_val(mut self, val: Variable) -> Self {
        self.ops.push(Op::Yield { val });
        self
    }

    pub fn resume(mut self, utxo: Variable, val: Variable) -> Self {
        self.ops.push(Op::Resume { utxo, val });
        self
    }

    pub fn burn(mut self) -> Self {
        self.ops.push(Op::Burn {});
        self
    }

    pub fn build(self) -> Rc<RefCell<MockedProgram>> {
        MockedProgram::new(self.ops, self.yielded)
    }
}

pub struct ProgramContext {
    programs: HashMap<Value, Rc<RefCell<MockedProgram>>>,
    next_id: u64,
}

impl ProgramContext {
    pub fn new() -> Self {
        Self {
            programs: HashMap::new(),
            next_id: 1,
        }
    }

    pub fn add_program_with_id(&mut self, id: Value, builder: ProgramBuilder) {
        self.programs.insert(id, builder.build());
        if id >= crate::F::from(self.next_id) {
            self.next_id = id.into_bigint().as_ref()[0] + 1;
        }
    }

    pub fn into_programs(self) -> HashMap<Value, Rc<RefCell<MockedProgram>>> {
        self.programs
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        F,
        e2e::{MockedLedger, ProgramBuilder, ProgramContext},
        test_utils::init_test_logging,
    };
    use std::collections::BTreeMap;

    #[test]
    fn test_trace_mocked_vm() {
        init_test_logging();
        let mut ctx = ProgramContext::new();

        let mut coord_builder = ProgramBuilder::new();
        let utxo1 = coord_builder.alloc_var();
        let val1 = coord_builder.alloc_var();
        let utxo2 = coord_builder.alloc_var();
        let val2 = coord_builder.alloc_var();

        let coordination_script = coord_builder
            .new_utxo(F::from(77), utxo1)
            .set_var(val1, F::from(42))
            .resume(utxo1, val1)
            .increment_var(val1, F::from(1))
            .resume(utxo1, val1)
            .new_utxo(F::from(120), utxo2)
            .set_var(val2, F::from(0))
            .resume(utxo2, val2)
            .resume(utxo2, val2);

        let mut utxo1_builder = ProgramBuilder::new();
        let utxo1_val = utxo1_builder.alloc_var();
        let utxo1 = utxo1_builder
            .set_var(utxo1_val, F::from(45))
            .yield_val(utxo1_val)
            .jump_to(1); // loop

        let mut utxo2_builder = ProgramBuilder::new();
        let utxo2_val = utxo2_builder.alloc_var();
        let utxo2 = utxo2_builder
            .set_var(utxo2_val, F::from(111))
            .yield_val(utxo2_val)
            .burn();

        ctx.add_program_with_id(F::from(1), coordination_script);
        ctx.add_program_with_id(F::from(2), utxo1);
        ctx.add_program_with_id(F::from(3), utxo2);

        let mut ledger = MockedLedger {
            utxos: BTreeMap::default(),
        };

        let (tx, commitments) = ledger.run_mocked_vm(F::from(1), ctx.into_programs());

        dbg!(&tx);
        for (program_id, commitment) in commitments.get_all_commitments() {
            let comm = commitment.as_field_elements();
            dbg!(program_id, comm[0], comm[1], comm[2], comm[3]);
        }

        tx.prove().unwrap();
    }
}
