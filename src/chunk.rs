#![allow(dead_code)]

use owo_colors::OwoColorize;
use std::fmt::Display;

use crate::value::{Value, ValuePool};

#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Return = 0,

    // load constant values
    Constant,
    ConstantLong,

    // numerical operators
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,

    // boolean
    True,
    False,

    // billion dollar mistake
    Nil,

    // logical operators
    Not,
    Eq,
    Greater,
    Less,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match self {
            Self::Return => "ret",

            Self::Constant => "ldc",      // load constant
            Self::ConstantLong => "ldlc", // load long constant

            Self::Negate => "neg",
            Self::Add => "add",
            Self::Subtract => "sub",
            Self::Multiply => "mul",
            Self::Divide => "div",

            Self::True => "true",
            Self::False => "false",

            Self::Nil => "nil",

            Self::Not => "not",
            Self::Eq => "eq",
            Self::Greater => "grtr",
            Self::Less => "less",
        };

        write!(f, "{:<5}", repr)
    }
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Self::Return),

            0x01 => Ok(Self::Constant),
            0x02 => Ok(Self::ConstantLong),

            0x03 => Ok(Self::Negate),
            0x04 => Ok(Self::Add),
            0x05 => Ok(Self::Subtract),
            0x06 => Ok(Self::Multiply),
            0x07 => Ok(Self::Divide),

            0x08 => Ok(Self::True),
            0x09 => Ok(Self::False),

            0x0a => Ok(Self::Nil),

            0x0b => Ok(Self::Not),
            0x0c => Ok(Self::Eq),
            0x0d => Ok(Self::Greater),
            0x0e => Ok(Self::Less),

            _ => Err(()),
        }
    }
}

// TODO Add a way to access code bytes and constants to remove pub visbility
pub struct Chunk {
    lines: Vec<(usize, u32)>, // bytecode -> src line. RLE (line_no, repeat {min: 1})
    code: Vec<u8>,
    constants: ValuePool,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            lines: Vec::new(),
            code: Vec::new(),
            constants: ValuePool::new(),
        }
    }

    pub fn clear(&mut self) {
        self.lines.clear();
        self.code.clear();
        self.constants.clear();
    }

    pub fn add_byte(&mut self, byte: u8, line: usize) -> &mut Self {
        self.code.push(byte);

        // if last exists and last.line is same as curr_line
        if let Some(prev_line) = self.lines.last_mut() {
            if prev_line.0 == line {
                prev_line.1 += 1;
                return self;
            }
        }

        self.lines.push((line, 1));

        self
    }

    pub fn get_byte(&self, index: usize) -> Option<&u8> {
        self.code.get(index)
    }

    pub fn add_op(&mut self, op: OpCode, line: usize) -> &mut Self {
        self.add_byte(op as u8, line)
    }

    // instruction const can have max 2 operands (u8 << 8 | u8)
    pub fn add_constant(&mut self, val: Value) -> Option<(u8, u8)> {
        let len = self.constants.len();

        if len >= u16::MAX as usize {
            return None;
        }

        self.constants.add(val);

        let byte1 = (len & 0xFF) as u8;
        let byte2 = ((len >> 8) & 0xFF) as u8;

        Some((byte1, byte2))
    }

    pub fn get_constant(&self, index: usize) -> Option<&Value> {
        self.constants.get(index)
    }

    // Returns (source_line_num, whether_this_line_mapped_to_chunk_is_repeated )
    // Skip on the RLE Units, adding the repeat value of the line.
    // If offset is below the accumulated repeat, then we found the line.
    // if skip - offset == repeat, then we are at a new value.
    pub fn offset_to_srcline(&self, offset: usize) -> Option<(usize, bool)> {
        let mut lines_skipped: usize = 0;

        for (line, repeat) in self.lines.iter() {
            lines_skipped += *repeat as usize;

            if lines_skipped > offset {
                let repeated_line = lines_skipped - offset < *repeat as usize; // at the first element
                return Some((*line, repeated_line));
            }
        }

        None
    }
}

pub fn constant_add_store(chunk: &mut Chunk, val: Value, line: usize) -> Option<()> {
    let (b1, b2) = chunk.add_constant(val)?;

    if b2 == 0 {
        chunk.add_op(OpCode::Constant, line).add_byte(b1, line);
    } else {
        chunk
            .add_op(OpCode::ConstantLong, line)
            .add_byte(b1, line)
            .add_byte(b2, line);
    }

    Some(())
}

#[cfg(any(test, debug_assertions))]
pub mod debug {
    use super::*;

    pub fn disassemble(chunk: &Chunk, name: &str) {
        println!("== {name} ==");

        let mut offset = 0;
        while let Some(off) = disassemble_instruction(chunk, offset) {
            offset = off;
        }
    }

    pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> Option<usize> {
        use OpCode::*;

        let instruction = *chunk.get_byte(offset)?;

        // Check if the line is the same, if yes print |
        let (line, line_repeated) = chunk.offset_to_srcline(offset)?;
        if line_repeated {
            print!("{offset:04}    | : ");
        } else {
            print!("{offset:04} {line:4} : ");
        }

        let ret = match OpCode::try_from(instruction) {
            Err(_) => {
                println!("{instruction:<8} ; {}", "Unknown Opcode".red());
                offset + 1
            }

            Ok(opcode) => match opcode {
                Constant => const_op(chunk, offset),
                ConstantLong => const_long_op(chunk, offset),

                _ => simple_op(opcode, offset),
            },
        };

        Some(ret)
    }

    fn simple_op(op: OpCode, offset: usize) -> usize {
        println!("{op}");
        offset + 1
    }

    fn const_op(chunk: &Chunk, offset: usize) -> usize {
        print!("{} ", OpCode::Constant);

        let index = chunk.get_byte(offset + 1);
        match index {
            None => println!("; {}", "Abrupt End".red()),
            Some(index) => {
                print!("{index:02x} ");

                let value = chunk.get_constant(*index as usize);
                match value {
                    None => println!("; {}", "No Value".red()),
                    Some(value) => println!("; {value}"),
                }
            }
        }

        offset + 2
    }

    fn const_long_op(chunk: &Chunk, offset: usize) -> usize {
        print!("{} ", OpCode::ConstantLong);

        let idx0 = chunk.get_byte(offset + 1);
        match idx0 {
            None => println!("; {}", "Abrupt End".red()),

            Some(idx0) => {
                print!("{idx0:2x} ");

                let idx1 = chunk.get_byte(offset + 2);
                match idx1 {
                    None => println!("; {}", "Abrupt End".red()),

                    Some(idx1) => {
                        print!("{idx1:2x} ");

                        let index = (*idx1 as usize) << 8 | (*idx0 as usize);
                        let value = chunk.get_constant(index);
                        match value {
                            None => println!("; {}", "No Value".red()),
                            Some(value) => println!("; {value}"),
                        }
                    }
                }
            }
        }

        offset + 4
    }
}

#[cfg(test)]
mod tests {
    use super::{debug::*, *};

    fn helper() -> (Chunk, usize) {
        (Chunk::new(), 100)
    }

    #[test]
    fn unknown_opcode() {
        let (mut chunk, line) = helper();

        chunk.add_byte(0xff, line).add_byte(0xff, line);

        chunk.add_op(OpCode::Return, line);

        disassemble(&chunk, "Unknown Opcode");
    }

    #[test]
    fn abrupt_end() {
        let (mut chunk, line) = helper();

        chunk.add_op(OpCode::Constant, line);

        disassemble(&chunk, "Abrupt End");
    }

    #[test]
    fn check_rle() {
        let (mut chunk, line) = helper();

        chunk
            .add_op(OpCode::Return, line)
            .add_op(OpCode::Return, line)
            .add_op(OpCode::Return, line + 1)
            .add_op(OpCode::Return, line + 1)
            .add_op(OpCode::Return, line + 1)
            .add_op(OpCode::Return, line + 1)
            .add_op(OpCode::Return, line + 5)
            .add_op(OpCode::Return, line + 5)
            .add_op(OpCode::Return, line + 5);

        disassemble(&chunk, "Check RLE");
    }

    #[test]
    fn check_constant_long() {
        let (mut chunk, line) = helper();

        for i in 0..0x012c {
            // 0..300
            chunk.add_constant(Value::Num(i as f64));
        }

        chunk
            .add_op(OpCode::ConstantLong, line)
            .add_byte(0xaa, line)
            .add_byte(0x02, line);

        chunk.add_op(OpCode::Return, line);

        disassemble(&chunk, "Op Constant Long");
    }
}
