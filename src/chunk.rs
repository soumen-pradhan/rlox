#![allow(dead_code)]

use owo_colors::OwoColorize;
use std::fmt::Display;

use crate::value::{Value, ValuePool};

#[repr(u8)]
pub enum OpCode {
    Return = 0,
    Constant,
    ConstantLong,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match self {
            OpCode::Return => "ret",
            OpCode::Constant => "const",
            OpCode::ConstantLong => "constl",
        };

        write!(f, "{:<8}", repr)
    }
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Return),
            1 => Ok(Self::Constant),
            2 => Ok(Self::ConstantLong),
            _ => Err(()),
        }
    }
}

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

    pub fn add_byte(&mut self, byte: u8, line: usize) -> &mut Self {
        self.code.push(byte);

        // if last exists and last.line is same as curr_line
        if let Some(prev_line) = self.lines.last_mut() {
            if prev_line.0 == line {
                prev_line.1 = prev_line.1 + 1;
                return self;
            }
        }

        self.lines.push((line, 1));

        self
    }

    pub fn add_op(&mut self, op: OpCode, line: usize) -> &mut Self {
        self.add_byte(op as u8, line)
    }

    pub fn add_constant(&mut self, val: Value) -> usize {
        self.constants.add(val);
        self.constants.len()
    }

    // Returns (source_line_num, whether_this_line_mapped_to_chunk_is_repeated )
    // Skip on the RLE Units, adding the repeat value of the line.
    // If offset is below the accumulated repeat, then we found the line.
    // if skip - offset == repeat, then we are at a new value.
    fn offset_to_srcline(&self, offset: usize) -> Option<(usize, bool)> {
        let mut lines_skipped: usize = 0;

        for (line, repeat) in self.lines.iter() {
            lines_skipped += *repeat as usize;

            if lines_skipped - 1 >= offset {
                let repeated_line = lines_skipped - offset < *repeat as usize; // at the first element
                return Some((*line, repeated_line));
            }
        }

        None
    }
}

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("== {name} ==");

    let mut offset = 0;
    while let Some(off) = disassemble_instruction(&chunk, offset) {
        offset = off;
    }
}

fn disassemble_instruction(chunk: &Chunk, offset: usize) -> Option<usize> {
    let instruction = *chunk.code.get(offset)?;

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
            OpCode::Return => simple_op(opcode, offset),
            OpCode::Constant => const_op(chunk, offset),
            OpCode::ConstantLong => const_long_op(chunk, offset),
        },
    };

    Some(ret)
}

fn simple_op(op: OpCode, offset: usize) -> usize {
    println!("{op}");
    offset + 1
}

fn const_op(chunk: &Chunk, offset: usize) -> usize {
    let check_valid = || -> Option<(u8, &Value)> {
        let constant_idx = *chunk.code.get(offset + 1)?;
        let value = chunk.constants.get(constant_idx as usize)?;

        Some((constant_idx, value))
    };

    match check_valid() {
        None => println!("{} ; {}", OpCode::Constant, "Abrupt End".red()),
        Some((idx, val)) => println!("{} {idx:4} ; {val}", OpCode::Constant),
    };

    offset + 2
}

fn const_long_op(chunk: &Chunk, offset: usize) -> usize {
    let check_valid = || -> Option<(usize, &Value)> {
        let idx0 = *chunk.code.get(offset + 1)?;
        let idx1 = *chunk.code.get(offset + 2)?;

        let constant_idx  = (idx1 << 8 | idx0) as usize;
        let value = chunk.constants.get(constant_idx)?;

        Some((constant_idx, value))
    };

    match check_valid() {
        None => println!("{} ; {}", OpCode::Constant, "Abrupt End".red()),
        Some((idx, val)) => println!("{} {idx:4} ; {val}", OpCode::Constant),
    };

    offset + 4
}

#[cfg(test)]
mod tests {
    use super::*;

    fn helper() -> (Chunk, usize) {
        (Chunk::new(), 666)
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
}
