#![allow(dead_code)]

use owo_colors::OwoColorize;

use crate::{
    chunk::{debug::disassemble_instruction, Chunk, OpCode},
    utils::{debug::stack_trace, Stack},
    value::Value,
};

pub struct VM<'a> {
    chunk: Option<&'a Chunk>,
    ip: usize, // instruction pointer
    stack: Stack<Value>,
}

pub enum VMResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: Stack::new(),
        }
    }

    pub fn interpret(&mut self, chunk: &'a Chunk) -> VMResult {
        self.chunk = Some(chunk);
        self.ip = 0;

        let res = self.run();

        match res {
            Some(r) => r,
            None => {
                println!("{}", "VM Runtime Error".red());
                VMResult::RuntimeError
            }
        }
    }

    // TODO Add more context when None is returned
    fn run(&mut self) -> Option<VMResult> {
        let chunk = self.chunk?;

        loop {
            #[cfg(any(test, debug_assertions))]
            {
                stack_trace(&self.stack);
                disassemble_instruction(chunk, self.ip);
            }

            let byte = chunk.get_byte(self.ip)?;
            self.ip += 1;

            let mut binary_op = |op: OpCode| -> Option<()> {
                let Value::Num(op2) = self.stack.pop()?;
                let Value::Num(op1) = self.stack.pop()?;

                let res = match op {
                    OpCode::Add => op1 + op2,
                    OpCode::Subtract => op1 - op2,
                    OpCode::Multiply => op1 * op2,
                    OpCode::Divide => op1 / op2,
                    _ => return None,
                };

                self.stack.push(Value::Num(res));
                Some(())
            };

            if let Ok(op) = OpCode::try_from(*byte) {
                match op {
                    OpCode::Return => {
                        let val = self.stack.pop()?;
                        println!("{val}");
                        return Some(VMResult::Ok);
                    }

                    OpCode::Constant => {
                        let idx = chunk.get_byte(self.ip)?;
                        self.ip += 1;

                        let val = chunk.get_constant(*idx as usize)?;
                        self.stack.push(*val);
                    }

                    OpCode::ConstantLong => {
                        let idx0 = chunk.get_byte(self.ip)?;
                        self.ip += 1;

                        let idx1 = chunk.get_byte(self.ip)?;
                        self.ip += 1;

                        let index = (*idx1 as usize) << 8 | (*idx0 as usize);
                        let val = chunk.get_constant(index)?;
                        self.stack.push(*val);
                    }

                    OpCode::Negate => {
                        // let val = match self.stack.pop()? {
                        //     Value::Num(n) => Value::Num(-n),
                        // };

                        // self.stack.push(val);

                        match self.stack.top_mut()? {
                            Value::Num(n) => *n = -*n,
                        };
                    }

                    OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide => {
                        binary_op(op)?;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;

    use super::*;

    fn helper() -> (Chunk, usize) {
        (Chunk::new(), 200)
    }

    fn constant_add(chunk: &mut Chunk, val: Value, line: usize) {
        let (b1, b2) = chunk.add_constant(val).unwrap();

        if b2 == 0 {
            chunk.add_op(OpCode::Constant, line).add_byte(b1, line);
        } else {
            chunk
                .add_op(OpCode::ConstantLong, line)
                .add_byte(b1, line)
                .add_byte(b2, line);
        }
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

    #[test]
    fn negate() {
        let (mut chunk, line) = helper();

        constant_add(&mut chunk, Value::Num(3.14), line);

        chunk
            .add_op(OpCode::Negate, line)
            .add_op(OpCode::Return, line);

        let mut vm = VM::new();
        vm.interpret(&chunk);
    }

    fn check_binary_operator(op: OpCode) {
        let mut vm = VM::new();

        let (mut chunk, line) = helper();

        constant_add(&mut chunk, Value::Num(1.0), line);
        constant_add(&mut chunk, Value::Num(2.0), line);

        chunk.add_op(op, line).add_op(OpCode::Return, line);

        vm.interpret(&chunk);
        chunk.clear();
    }

    #[test]
    fn add() {
        check_binary_operator(OpCode::Add);
    }

    #[test]
    fn subtract() {
        check_binary_operator(OpCode::Subtract);
    }

    #[test]
    fn multiply() {
        check_binary_operator(OpCode::Multiply);
    }

    #[test]
    fn divide() {
        check_binary_operator(OpCode::Divide);
    }

    #[test]
    fn expr() {
        // (- (* (+ 1.2 3.4) 5.6))

        let (mut chunk, line) = helper();

        constant_add(&mut chunk, Value::Num(1.2), line);
        constant_add(&mut chunk, Value::Num(3.4), line);
        chunk.add_op(OpCode::Add, line);

        constant_add(&mut chunk, Value::Num(5.6), line);
        chunk.add_op(OpCode::Multiply, line);

        chunk
            .add_op(OpCode::Negate, line)
            .add_op(OpCode::Return, line);

        let mut vm = VM::new();
        vm.interpret(&chunk);
    }
}
