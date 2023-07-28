#![allow(dead_code)]

use std::fmt::Display;

#[derive(Copy, Clone)]
pub enum Value {
    Num(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{n}")
        }
    }
}

pub struct ValuePool {
    values: Vec<Value>,
}

impl ValuePool {
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }

    pub fn add(&mut self, byte: Value) {
        self.values.push(byte);
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn get(&self, index: usize) -> Option<&Value> {
        self.values.get(index)
    }
}
