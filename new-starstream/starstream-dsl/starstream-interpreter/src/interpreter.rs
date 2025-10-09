//! Streaming interpreter for the Starstream DSL
//! 
//! This module provides a streaming interpreter that executes opcodes one at a time
//! with a plugin architecture for handling different opcode types.

use starstream_types::*;
use std::collections::BTreeMap;

use crate::context::ExecutionEnvironment;

/// Trait for handling opcodes in the streaming interpreter
pub trait OpcodeHandler {
    fn handle_opcode(&mut self, opcode: &Opcode, environment: &mut ExecutionEnvironment) -> Result<()>;
}

/// Default opcode handler that implements basic stack machine operations
pub struct DefaultOpcodeHandler;

impl OpcodeHandler for DefaultOpcodeHandler {
    fn handle_opcode(&mut self, opcode: &Opcode, environment: &mut ExecutionEnvironment) -> Result<()> {
        match opcode {
            // Stack operations
            Opcode::Push(value) => {
                environment.local_memory_context.push(*value)?;
            }
            Opcode::Pop => {
                environment.local_memory_context.pop()?;
            }
            Opcode::Dup => {
                let value = environment.local_memory_context.peek()?;
                environment.local_memory_context.push(value)?;
            }
            Opcode::Swap => {
                let a = environment.local_memory_context.pop()?;
                let b = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(a)?;
                environment.local_memory_context.push(b)?;
            }

            // Arithmetic operations
            Opcode::Add => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(a + b)?;
            }
            Opcode::Subtract => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(a - b)?;
            }
            Opcode::Multiply => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(a * b)?;
            }
            Opcode::Divide => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                if b == 0 {
                    return Err(StarstreamError::RuntimeError("Division by zero".to_string()));
                }
                environment.local_memory_context.push(a / b)?;
            }
            Opcode::Modulo => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                if b == 0 {
                    return Err(StarstreamError::RuntimeError("Modulo by zero".to_string()));
                }
                environment.local_memory_context.push(a % b)?;
            }

            // Comparison operations
            Opcode::Equal => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a == b { 1 } else { 0 })?;
            }
            Opcode::NotEqual => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a != b { 1 } else { 0 })?;
            }
            Opcode::LessThan => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a < b { 1 } else { 0 })?;
            }
            Opcode::LessEqual => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a <= b { 1 } else { 0 })?;
            }
            Opcode::GreaterThan => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a > b { 1 } else { 0 })?;
            }
            Opcode::GreaterEqual => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a >= b { 1 } else { 0 })?;
            }

            // Logical operations
            Opcode::And => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a != 0 && b != 0 { 1 } else { 0 })?;
            }
            Opcode::Or => {
                let b = environment.local_memory_context.pop()?;
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a != 0 || b != 0 { 1 } else { 0 })?;
            }
            Opcode::Not => {
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(if a == 0 { 1 } else { 0 })?;
            }

            // Unary operations
            Opcode::Negate => {
                let a = environment.local_memory_context.pop()?;
                environment.local_memory_context.push(-a)?;
            }

            // Memory operations
            Opcode::Load(name) => {
                // First try local memory, then persistent memory
                let value = environment.local_memory_context.get_variable(name)
                    .or_else(|| environment.persistent_memory_context.get_global_variable(name))
                    .ok_or_else(|| StarstreamError::UndefinedVariable(name.clone()))?;
                environment.local_memory_context.push(value)?;
            }
            Opcode::Store(name) => {
                let value = environment.local_memory_context.pop()?;
                // Store in local memory for now (could be enhanced to distinguish local vs global)
                environment.local_memory_context.set_variable(name.clone(), value);
            }

            // Control flow
            Opcode::Jump(address) => {
                environment.instruction_context.jump_to(*address);
                return Ok(());
            }
            Opcode::JumpIf(address) => {
                let condition = environment.local_memory_context.pop()?;
                if condition != 0 {
                    environment.instruction_context.jump_to(*address);
                    return Ok(());
                }
            }
            Opcode::JumpIfNot(address) => {
                let condition = environment.local_memory_context.pop()?;
                if condition == 0 {
                    environment.instruction_context.jump_to(*address);
                    return Ok(());
                }
            }
            Opcode::Call(address) => {
                environment.instruction_context.call(*address);
                return Ok(());
            }
            Opcode::Return => {
                let return_address = environment.instruction_context.return_from_call()?;
                environment.instruction_context.jump_to(return_address);
            }

            // Special operations
            Opcode::Halt => {
                // Do nothing, execution will stop
            }
            Opcode::Nop => {
                // No operation
            }
        }
        Ok(())
    }
}

/// Streaming interpreter that executes opcodes one at a time
pub struct StreamingInterpreter<H: OpcodeHandler> {
    handler: H,
    environment: ExecutionEnvironment,
}

impl<H: OpcodeHandler> StreamingInterpreter<H> {
    pub fn new(handler: H, instructions: Vec<Opcode>) -> Self {
        Self {
            handler,
            environment: ExecutionEnvironment::new(instructions),
        }
    }

    /// Execute a single opcode
    pub fn execute_opcode(&mut self, opcode: &Opcode) -> Result<()> {
        self.handler.handle_opcode(opcode, &mut self.environment)?;
        Ok(())
    }

    /// Execute a sequence of opcodes
    pub fn execute_sequence(&mut self, opcodes: &[Opcode]) -> Result<()> {
        for opcode in opcodes {
            self.execute_opcode(opcode)?;
        }
        Ok(())
    }

    /// Execute the current instruction from the instruction context
    pub fn execute_current_instruction(&mut self) -> Result<bool> {
        let opcode = if let Some(opcode) = self.environment.instruction_context.current_opcode() {
            opcode.clone()
        } else {
            return Ok(false);
        };
        
        self.execute_opcode(&opcode)?;
        self.environment.instruction_context.program_counter += 1;
        Ok(true)
    }

    /// Run the interpreter until completion or error
    pub fn run(&mut self) -> Result<()> {
        while !self.environment.instruction_context.is_finished() {
            self.execute_current_instruction()?;
        }
        Ok(())
    }

    /// Get the current execution environment
    pub fn environment(&self) -> &ExecutionEnvironment {
        &self.environment
    }

    /// Get a mutable reference to the execution environment
    pub fn environment_mut(&mut self) -> &mut ExecutionEnvironment {
        &mut self.environment
    }

    /// Legacy method for backward compatibility
    pub fn context(&self) -> ExecutionContext {
        ExecutionContext {
            stack: self.environment.local_memory_context.stack.clone(),
            variables: self.environment.local_memory_context.variables.clone(),
            program_counter: self.environment.instruction_context.program_counter,
            call_stack: self.environment.instruction_context.call_stack.clone(),
        }
    }

    /// Legacy method for backward compatibility
    pub fn context_mut(&mut self) -> &mut ExecutionContext {
        // This is a bit tricky since we need to return a mutable reference
        // but we can't easily create one from the environment
        // For now, we'll create a temporary context that can be used for reading
        // In practice, users should migrate to using environment() and environment_mut()
        panic!("context_mut() is deprecated. Use environment_mut() instead.");
    }
}
