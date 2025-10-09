//! Stack machine opcodes for the Starstream DSL
//! 
//! This module defines the opcodes that the compiler generates and the
//! interpreter executes.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[cfg(any(test, feature = "quickcheck"))]
use quickcheck::{Arbitrary, Gen};

/// A stack machine opcode
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Opcode {
    // Stack operations
    Push(i64),           // Push integer onto stack
    Pop,                 // Pop value from stack
    Dup,                 // Duplicate top of stack
    Swap,                // Swap top two stack elements
    
    // Arithmetic operations
    Add,                 // Pop two values, push their sum
    Subtract,            // Pop two values, push their difference
    Multiply,            // Pop two values, push their product
    Divide,              // Pop two values, push their quotient
    Modulo,              // Pop two values, push their remainder
    
    // Comparison operations
    Equal,               // Pop two values, push 1 if equal, 0 otherwise
    NotEqual,            // Pop two values, push 1 if not equal, 0 otherwise
    LessThan,            // Pop two values, push 1 if first < second, 0 otherwise
    LessEqual,           // Pop two values, push 1 if first <= second, 0 otherwise
    GreaterThan,         // Pop two values, push 1 if first > second, 0 otherwise
    GreaterEqual,        // Pop two values, push 1 if first >= second, 0 otherwise
    
    // Logical operations
    And,                 // Pop two values, push 1 if both non-zero, 0 otherwise
    Or,                  // Pop two values, push 1 if either non-zero, 0 otherwise
    Not,                 // Pop one value, push 1 if zero, 0 otherwise
    
    // Unary operations
    Negate,              // Pop one value, push its negation
    
    // Memory operations
    Load(String),        // Load variable value onto stack
    Store(String),       // Pop value and store in variable
    
    // Control flow
    Jump(usize),         // Jump to absolute address
    JumpIf(usize),       // Pop value, jump if non-zero
    JumpIfNot(usize),    // Pop value, jump if zero
    Call(usize),         // Call subroutine at address
    Return,              // Return from subroutine
    
    // Special operations
    Halt,                // Halt execution
    Nop,                 // No operation
}

/// A sequence of opcodes representing a compiled program
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OpcodeSequence {
    pub opcodes: Vec<Opcode>,
    pub labels: HashMap<String, usize>,
}

impl OpcodeSequence {
    pub fn new() -> Self {
        Self {
            opcodes: Vec::new(),
            labels: HashMap::new(),
        }
    }
    
    pub fn add_opcode(&mut self, opcode: Opcode) -> usize {
        let address = self.opcodes.len();
        self.opcodes.push(opcode);
        address
    }
    
    pub fn add_label(&mut self, name: String, address: usize) {
        self.labels.insert(name, address);
    }
    
    pub fn get_label_address(&self, name: &str) -> Option<usize> {
        self.labels.get(name).copied()
    }
    
    pub fn len(&self) -> usize {
        self.opcodes.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.opcodes.is_empty()
    }
}

#[cfg(any(test, feature = "quickcheck"))]
impl Arbitrary for Opcode {
    fn arbitrary(g: &mut Gen) -> Self {
        match g.choose(&std::array::from_fn::<usize, 100, _>(|i| i)).unwrap() {
            // Stack operations (0-3)
            0..=3 => Opcode::Push(Arbitrary::arbitrary(g)),
            4..=6 => Opcode::Pop,
            7..=9 => Opcode::Dup,
            10..=12 => Opcode::Swap,
            
            // Arithmetic operations (13-17)
            13..=17 => Opcode::Add,
            18..=22 => Opcode::Subtract,
            23..=27 => Opcode::Multiply,
            28..=32 => Opcode::Divide,
            33..=37 => Opcode::Modulo,
            
            // Comparison operations (38-43)
            38..=40 => Opcode::Equal,
            41..=43 => Opcode::NotEqual,
            44..=46 => Opcode::LessThan,
            47..=49 => Opcode::LessEqual,
            50..=52 => Opcode::GreaterThan,
            53..=55 => Opcode::GreaterEqual,
            
            // Logical operations (56-58)
            56..=58 => Opcode::And,
            59..=61 => Opcode::Or,
            62..=64 => Opcode::Not,
            
            // Unary operations (65-67)
            65..=67 => Opcode::Negate,
            
            // Memory operations (68-75)
            68..=75 => Opcode::Load(generate_variable_name(g)),
            76..=83 => Opcode::Store(generate_variable_name(g)),
            
            // Control flow (84-95)
            84..=87 => Opcode::Jump(<usize as Arbitrary>::arbitrary(g) % 1000), // Limit jump addresses
            88..=91 => Opcode::JumpIf(<usize as Arbitrary>::arbitrary(g) % 1000),
            92..=95 => Opcode::JumpIfNot(<usize as Arbitrary>::arbitrary(g) % 1000),
            96..=97 => Opcode::Call(<usize as Arbitrary>::arbitrary(g) % 1000),
            98..=99 => Opcode::Return,
            
            // Special operations (100+). No need to actually generate these
            _ => match g.choose(&[0, 1]).unwrap() {
                0 => Opcode::Halt,
                _ => Opcode::Nop,
            },
        }
    }
}

#[cfg(any(test, feature = "quickcheck"))]
fn generate_variable_name(g: &mut Gen) -> String {
    let prefixes = ["x", "y", "z", "a", "b", "c", "var", "temp", "result"];
    let prefix = g.choose(&prefixes).unwrap();
    let suffix = *g.choose(&std::array::from_fn::<usize, 10, _>(|i| i)).unwrap();
    format!("{}{}", prefix, suffix)
}
