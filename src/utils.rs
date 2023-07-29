#![allow(dead_code)]

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
