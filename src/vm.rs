#![allow(dead_code)]

use std::{collections::HashMap, hint::unreachable_unchecked};

use owo_colors::OwoColorize;

use crate::{
    chunk::{debug::disassemble_instruction, Chunk, OpCode},
    error::prelude::*,
    utils::{debug::stack_trace, Stack},
    value::Value,
};

pub struct VM<'a> {
    chunk: Option<&'a Chunk>,
    ip: usize, // instruction pointer
    stack: Stack<Value>,
    pub globals: HashMap<String, Value>,
}

#[derive(Debug)]
pub enum VMErr {
    NoChunk,
    NoMoreBytes,
    UnknownConst,
    UnknownOp,
    UnknownVar,
    StackEmpty,
    TypeErr,
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: Stack::new(),
            globals: HashMap::new(),
        }
    }

    fn line(&self) -> usize {
        if let Some(c) = self.chunk {
            if let Some((l, _)) = c.offset_to_srcline(self.ip) {
                return l;
            }
        }

        return 0;
    }

    fn chunk(&self) -> Result<&'a Chunk, VMErr> {
        self.chunk.ok_or(VMErr::NoChunk)
    }

    fn get_byte(&self, index: usize) -> Result<u8, VMErr> {
        self.chunk()?.get_byte(index).ok_or(VMErr::NoMoreBytes)
    }

    fn get_constant(&self, index: usize) -> Result<&Value, VMErr> {
        self.chunk()?.get_constant(index).ok_or(VMErr::UnknownConst)
    }

    fn recreate_index(&mut self) -> Result<usize, VMErr> {
        let idx0 = self.get_byte(self.ip)?;
        self.ip += 1;

        let idx1 = self.get_byte(self.ip)?;
        self.ip += 1;

        let index = (idx1 as usize) << 8 | (idx0 as usize);
        Ok(index)
    }

    fn stack_pop(&mut self) -> Result<Value, VMErr> {
        self.stack.pop().ok_or(VMErr::StackEmpty)
    }

    fn stack_top_mut(&mut self) -> Result<&mut Value, VMErr> {
        self.stack.top_mut().ok_or(VMErr::StackEmpty)
    }

    fn var_name_at(&self, index: usize) -> Result<&String, VMErr> {
        let key = self.get_constant(index)?;

        let key = match key {
            Value::Symbol(s) => s,
            _ => {
                log!(line self.line(), "VM: invalid variable name");
                return Err(VMErr::TypeErr);
            }
        };

        Ok(key)
    }

    fn get_global_var(&self, key: &String) -> Result<&Value, VMErr> {
        if let Some(v) = self.globals.get(key) {
            Ok(v)
        } else {
            let msg = format!("VM: undefined variable {key}");
            log!(line self.line(), msg.as_str());
            Err(VMErr::UnknownVar)
        }
    }
}

impl<'a> VM<'a> {
    pub fn interpret(&mut self, chunk: &'a Chunk) -> Result<Value, VMErr> {
        self.chunk = Some(chunk);
        self.ip = 0;

        let res = self.run();

        match res {
            Ok(_) => res,

            Err(e) => {
                println!("{} {e:?}", "VM Runtime Error:".red());
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

            let byte = self.get_byte(self.ip)?;
            self.ip += 1;

            if let Ok(op) = OpCode::try_from(byte) {
                match op {
                    Return => {
                        let val = self.stack.pop().unwrap_or(Value::Unit);
                        return Ok(val);
                    }

                    Constant => {
                        let idx = self.get_byte(self.ip)?;
                        self.ip += 1;

                        let val = self.get_constant(idx as usize)?;
                        self.stack.push(val.clone());
                    }

                    ConstantLong => {
                        let index = self.recreate_index()?;
                        let val = self.get_constant(index)?;

                        self.stack.push(val.clone());
                    }

                    Negate => {
                        match self.stack_top_mut()? {
                            Value::Num(n) => *n = -*n,
                            _ => {
                                log!(line self.line(), "VM: not a number");
                                return Err(VMErr::TypeErr);
                            }
                        };
                    }

                    Add | Subtract | Multiply | Divide => {
                        let pop = self.stack_pop()?;
                        let top = self.stack_top_mut()?;

                        match (pop, top) {
                            (Value::Num(op2), Value::Num(op1)) => match op {
                                OpCode::Add => *op1 += op2,
                                OpCode::Subtract => *op1 -= op2,
                                OpCode::Multiply => *op1 *= op2,
                                OpCode::Divide => *op1 /= op2,

                                _ => unsafe {
                                    unreachable_unchecked();
                                },
                            },

                            (Value::Str(op2), Value::Str(op1)) => match op {
                                OpCode::Add => op1.push_str(op2.as_str()),
                                _ => {
                                    log!(line self.line(), "VM: unknown operation on strings");
                                    return Err(VMErr::UnknownOp);
                                }
                            },
                            _ => {
                                log!(line self.line(), "VM: require 2 numbers or 2 strings");
                                return Err(VMErr::TypeErr);
                            }
                        };
                    }

                    True => self.stack.push(Value::Bool(true)),
                    False => self.stack.push(Value::Bool(false)),

                    Nil => self.stack.push(Value::Nil),

                    Not => {
                        let bool_val = self.stack_pop()?.to_logical();
                        self.stack.push(Value::Bool(!bool_val));
                    }

                    Eq | Greater | Less => {
                        let op2 = self.stack_pop()?;
                        let op1 = self.stack_pop()?;

                        let res = match op {
                            OpCode::Eq => op1 == op2,
                            OpCode::Greater => op1 > op2,
                            OpCode::Less => op1 < op2,

                            _ => unsafe {
                                unreachable_unchecked();
                            },
                        };

                        self.stack.push(Value::Bool(res));
                    }

                    Print => {
                        let pop = self.stack_pop()?;
                        println!("{pop}");
                    }

                    Pop => {
                        self.stack_pop()?;
                    }

                    DefineGlobal => {
                        let val = self.stack_pop()?;

                        let index = self.recreate_index()?;
                        let key = self.var_name_at(index)?;

                        self.globals.insert(key.clone(), val);
                    }

                    GetGlobal => {
                        let index = self.recreate_index()?;
                        let key = self.var_name_at(index)?;

                        let val = self.get_global_var(key)?;

                        self.stack.push(val.clone());
                    }

                    SetGlobal => {
                        let val = self.stack.top().ok_or(VMErr::StackEmpty)?.clone();

                        let index = self.recreate_index()?;
                        let key = self.var_name_at(index)?;

                        let msg = format!("VM: undefined variable {key}");

                        match self.globals.get(key) {
                            Some(_) => self.globals.insert(key.clone(), val),
                            None => {
                                log!(line self.line(), msg.as_str());
                                return Err(VMErr::UnknownVar);
                            }
                        };
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

    #[test]
    fn string() {
        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        constant_add_store(&mut chunk, Value::Str("Hello".into()), line);
        constant_add_store(&mut chunk, Value::Str("World".into()), line);

        chunk.add_op(OpCode::Add, line).add_op(OpCode::Return, line);

        log_context! {
            @lines;
            let mut vm = VM::new();
            let v = vm.interpret(&chunk).unwrap();

            assert_eq!(v, Value::Str("HelloWorld".into()));
        }
    }

    #[test]
    fn print() {
        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        constant_add_store(&mut chunk, Value::Str("Hello".into()), line);
        constant_add_store(&mut chunk, Value::Str("World".into()), line);

        chunk
            .add_op(OpCode::Add, line)
            .add_op(OpCode::Print, line)
            .add_op(OpCode::Return, line);

        log_context! {
            @lines;
            let mut vm = VM::new();
            let v = vm.interpret(&chunk).unwrap();

            assert_eq!(v, Value::Unit);
        }
    }

    #[test]
    fn global_var() {
        let (mut chunk, lines) = helper();
        let line = lines.len() - 1;

        let (idx0, idx1) = chunk.add_constant(Value::Symbol("var_1".into())).unwrap();

        constant_add_store(&mut chunk, Value::Num(3.14), line);

        chunk
            .add_op(OpCode::DefineGlobal, line)
            .add_byte(idx0, line)
            .add_byte(idx1, line);

        constant_add_store(&mut chunk, Value::Bool(true), line);

        chunk
            .add_op(OpCode::SetGlobal, line)
            .add_byte(idx0, line)
            .add_byte(idx1, line);

        chunk
            .add_op(OpCode::Print, line)
            .add_op(OpCode::Return, line);

        log_context! {
            @lines;
            let mut vm = VM::new();
            vm.interpret(&chunk).unwrap();

            println!("{:?}", vm.globals);

            // assert_eq!(v, Value::Unit);
        }
    }
}
