#![allow(dead_code)]

use owo_colors::OwoColorize;

use crate::{
    chunk::{debug::disassemble_instruction, Chunk, OpCode},
    error::prelude::*,
    utils::{debug::stack_trace, Loc, Pos, Stack},
    value::Value,
};

pub struct VM<'a> {
    chunk: Option<&'a Chunk>,
    ip: usize, // instruction pointer
    stack: Stack<Value>,
}

#[derive(Debug)]
pub enum VMErr {
    // MemoryError,
    NoChunk,
    // RuntimeError,
    NoBytes,
    StackEmpty,
    UnknownOp,
    TypeErr,
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: Stack::new(),
        }
    }

    pub fn interpret(&mut self, chunk: &'a Chunk) -> Result<Value, VMErr> {
        self.chunk = Some(chunk);
        self.ip = 0;

        let res = self.run();

        match res {
            Ok(v) => {
                #[cfg(any(test, debug_assertions))]
                println!("{v}");

                Ok(v)
            }
            Err(e) => {
                let msg = format!("{}: {e:?}", "VM Runtime Error".red());
                let srcline = chunk.offset_to_srcline(self.ip).unwrap_or((0, false)).0;
                let loc = Loc {
                    start: Pos(srcline, 0),
                    end: Pos(srcline, 0),
                };
                log!(loc, msg.as_str());

                Err(e)
            }
        }
    }

    fn run(&mut self) -> Result<Value, VMErr> {
        use OpCode::*;

        let chunk = self.chunk.ok_or(VMErr::NoChunk)?;

        loop {
            #[cfg(any(test, debug_assertions))]
            {
                stack_trace(&self.stack);
                disassemble_instruction(chunk, self.ip);
            }

            let byte = chunk.get_byte(self.ip).ok_or(VMErr::NoBytes)?;
            self.ip += 1;

            if let Ok(op) = OpCode::try_from(*byte) {
                match op {
                    Return => {
                        let val = self.stack.pop().ok_or(VMErr::StackEmpty)?;
                        return Ok(val);
                    }

                    Constant => {
                        let idx = chunk.get_byte(self.ip).ok_or(VMErr::NoBytes)?;
                        self.ip += 1;

                        let val = chunk.get_constant(*idx as usize).ok_or(VMErr::NoBytes)?;
                        self.stack.push(*val);
                    }

                    ConstantLong => {
                        let idx0 = chunk.get_byte(self.ip).ok_or(VMErr::NoBytes)?;
                        self.ip += 1;

                        let idx1 = chunk.get_byte(self.ip).ok_or(VMErr::NoBytes)?;
                        self.ip += 1;

                        let index = (*idx1 as usize) << 8 | (*idx0 as usize);
                        let val = chunk.get_constant(index).ok_or(VMErr::NoBytes)?;
                        self.stack.push(*val);
                    }

                    Negate => {
                        match self.stack.top_mut().ok_or(VMErr::StackEmpty)? {
                            Value::Num(n) => *n = -*n,
                            _ => return Err(VMErr::TypeErr),
                        };
                    }

                    Add | Subtract | Multiply | Divide => {
                        let pop = self.stack.pop().ok_or(VMErr::StackEmpty)?;
                        let top = self.stack.top_mut().ok_or(VMErr::StackEmpty)?;

                        match (pop, top) {
                            (Value::Num(op2), Value::Num(op1)) => match op {
                                OpCode::Add => *op1 += op2,
                                OpCode::Subtract => *op1 -= op2,
                                OpCode::Multiply => *op1 *= op2,
                                OpCode::Divide => *op1 /= op2,

                                _ => return Err(VMErr::UnknownOp),
                            },
                            _ => return Err(VMErr::TypeErr),
                        };
                    }

                    True => self.stack.push(Value::Bool(true)),
                    False => self.stack.push(Value::Bool(false)),

                    Nil => self.stack.push(Value::Nil),

                    Not => {
                        let bool_val = self.stack.pop().ok_or(VMErr::StackEmpty)?.is_falsey();
                        self.stack.push(Value::Bool(bool_val));
                    }

                    Eq | Greater | Less => {
                        let op2 = self.stack.pop().ok_or(VMErr::StackEmpty)?;
                        let op1 = self.stack.pop().ok_or(VMErr::StackEmpty)?;

                        let res = match op {
                            OpCode::Eq => op1 == op2,
                            OpCode::Greater => op1 > op2,
                            OpCode::Less => op1 < op2,
                            _ => return Err(VMErr::TypeErr),
                        };

                        self.stack.push(Value::Bool(res));
                    }
                }
            } else {
                return Err(VMErr::UnknownOp);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{chunk::constant_add_store, value::Value};

    use super::*;

    fn helper() -> (Chunk, Vec<String>) {
        (Chunk::new(), vec!["reference src line\n".to_string()])
    }

    #[test]
    fn it_works() {
        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        for i in 0..10 {
            chunk.add_constant(Value::Num(i as f64));
        }

        chunk.add_op(OpCode::Constant, line).add_byte(0x01, line);
        chunk.add_op(OpCode::Constant, line).add_byte(0x05, line);

        chunk.add_op(OpCode::Return, line);

        log_context! {
            @lines;
            let mut vm = VM::new();
            vm.interpret(&chunk).unwrap();
        }
    }

    #[test]
    fn negate() {
        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        constant_add_store(&mut chunk, Value::Num(3.14), line);

        chunk
            .add_op(OpCode::Negate, line)
            .add_op(OpCode::Return, line);

        log_context! {
            @lines;
            let mut vm = VM::new();
            let v = vm.interpret(&chunk).unwrap();

            assert_eq!(v, Value::Num(-3.14));
        }
    }

    fn check_binary_operator(op: OpCode, n1: f64, n2: f64, res: f64) {
        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        constant_add_store(&mut chunk, Value::Num(n1), line);
        constant_add_store(&mut chunk, Value::Num(n2), line);

        chunk.add_op(op, line).add_op(OpCode::Return, line);

        let v: Value;

        log_context! {
            @lines;
            let mut vm = VM::new();
            v = vm.interpret(&chunk).unwrap();
        }

        assert_eq!(v, Value::Num(res));
    }

    #[test]
    fn add() {
        check_binary_operator(OpCode::Add, 1.0, 2.0, 3.0);
    }

    #[test]
    fn subtract() {
        check_binary_operator(OpCode::Subtract, 1.0, 2.0, -1.0);
    }

    #[test]
    fn multiply() {
        check_binary_operator(OpCode::Multiply, 2.0, 0.5, 1.0);
    }

    #[test]
    fn divide() {
        check_binary_operator(OpCode::Divide, 3.0, 2.0, 1.5);
    }

    #[test]
    fn expr() {
        // (- (* (+ 1.2 3.4) 5.6))

        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        constant_add_store(&mut chunk, Value::Num(1.2), line);
        constant_add_store(&mut chunk, Value::Num(3.8), line);
        chunk.add_op(OpCode::Add, line);

        constant_add_store(&mut chunk, Value::Num(5.0), line);
        chunk.add_op(OpCode::Multiply, line);

        chunk
            .add_op(OpCode::Negate, line)
            .add_op(OpCode::Return, line);

        log_context! {
            @lines;
            let mut vm = VM::new();
            let v = vm.interpret(&chunk).unwrap();

            assert_eq!(v, Value::Num(-25.0));
        }
    }

    fn check_logical_operator(op: OpCode, n1: Value, n2: Value, res: bool) {
        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        constant_add_store(&mut chunk, n1, line);
        constant_add_store(&mut chunk, n2, line);

        chunk.add_op(op, line).add_op(OpCode::Return, line);

        let v: Value;

        log_context! {
            @lines;
            let mut vm = VM::new();
            v = vm.interpret(&chunk).unwrap();
        }

        assert_eq!(v, Value::Bool(res));
    }

    #[test]
    fn not() {
        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        constant_add_store(&mut chunk, Value::Bool(true), line);

        chunk.add_op(OpCode::Not, line).add_op(OpCode::Return, line);

        log_context! {
            @lines;
            let mut vm = VM::new();
            let v = vm.interpret(&chunk).unwrap();

            assert_eq!(v, Value::Bool(false));
        }
    }

    #[test]
    fn eq_logical() {
        let (n1, n2) = (Value::Bool(true), Value::Bool(false));
        check_logical_operator(OpCode::Eq, n1, n2, false);
    }

    #[test]
    fn eq_num() {
        let (n1, n2) = (Value::Num(3.1), Value::Num(3.1));
        check_logical_operator(OpCode::Eq, n1, n2, true);
    }

    #[test]
    fn greater() {
        let (n1, n2) = (Value::Num(5.5), Value::Num(3.2));
        check_logical_operator(OpCode::Greater, n1, n2, true);
    }

    #[test]
    fn less() {
        let (n1, n2) = (Value::Num(5.5), Value::Num(3.2));
        check_logical_operator(OpCode::Less, n1, n2, false);
    }
}
