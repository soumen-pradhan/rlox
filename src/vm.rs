#![allow(dead_code)]

use crate::chunk::{disassemble_instruction, Chunk, OpCode};

pub struct VM<'a> {
    chunk: Option<&'a Chunk>,
    ip: usize, // instruction pointer
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        Self { chunk: None, ip: 0 }
    }

    pub fn interpret(&mut self, chunk: &'a Chunk) -> InterpretResult {
        self.chunk = Some(chunk);
        self.ip = 0;

        self.run();

        InterpretResult::CompileError
    }

    fn run(&mut self) -> Option<InterpretResult> {
        let chunk = self.chunk?;

        loop {
            #[cfg(any(test, debug_assertions))]
            disassemble_instruction(chunk, self.ip);
            
            let byte = chunk.get_byte(self.ip)?;
            self.ip += 1;

            if let Ok(op) = OpCode::try_from(*byte) {
                match op {
                    OpCode::Return => return Some(InterpretResult::Ok),

                    OpCode::Constant => {
                        let idx = chunk.get_byte(self.ip)?;
                        self.ip += 1;

                        let value = chunk.get_constant(*idx as usize)?;

                        println!("constant {value}");
                        break;
                    }

                    OpCode::ConstantLong => {
                        let idx0 = chunk.get_byte(self.ip)?;
                        self.ip += 1;

                        let idx1 = chunk.get_byte(self.ip)?;
                        self.ip += 1;

                        let index = (*idx1 as usize) << 8 | (*idx0 as usize);
                        let value = chunk.get_constant(index)?;

                        println!("constant {value}");
                        break;
                    }
                }
            }
        }

        Some(InterpretResult::Ok)
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;

    use super::*;

    fn helper() -> (Chunk, usize) {
        (Chunk::new(), 666)
    }

    #[test]
    fn it_works() {
        let (mut chunk, line) = helper();

        for i in 0..10 {
            chunk.add_constant(Value::Num(i as f64));
        }

        chunk.add_op(OpCode::Constant, line).add_byte(0x01, line);
        chunk.add_op(OpCode::Constant, line).add_byte(0x05, line);

        chunk.add_op(OpCode::Return, line);

        let mut vm = VM::new();
        vm.interpret(&chunk);
    }
}
