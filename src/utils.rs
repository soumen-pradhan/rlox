#![allow(dead_code)]

use std::ops::{Add, AddAssign};

pub struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn reset(&mut self) {
        self.0.clear()
    }

    pub fn push(&mut self, val: T) {
        self.0.push(val)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub fn top_mut(&mut self) -> Option<&mut T> {
        self.0.last_mut()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Pos(pub usize, pub usize); // (line, col)

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl Add<usize> for Pos {
    type Output = Pos;

    fn add(self, col: usize) -> Self::Output {
        Pos(self.0, self.1 + col)
    }
}

impl AddAssign<usize> for Pos {
    fn add_assign(&mut self, col: usize) {
        self.1 += col;
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Loc {
    pub start: Pos,
    pub end: Pos,
}

impl Loc {
    pub fn empty() -> Self {
        Self {
            start: Pos(0, 0),
            end: Pos(0, 0),
        }
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.start, self.end)
    }
}


#[cfg(any(test, debug_assertions))]
pub mod debug {
    use super::*;
    use std::fmt::Display;

    pub fn stack_trace<T: Display>(stack: &Stack<T>) {
        print!("{:width$}", "", width = 4 + 1 + 4 + 3);
        for val in stack.0.iter() {
            print!("[ {val} ]");
        }
        println!();
    }
}
