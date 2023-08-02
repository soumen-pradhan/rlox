#![allow(dead_code)]

use std::fmt::Display;

#[derive(Copy, Clone)]
pub enum Value {
    Num(f64),
}

const EPSILON: f64 = 1e-200;

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(
                f,
                "{n:.precision$}",
                precision = if n.fract() > EPSILON { 2 } else { 0 }
            ),
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

    pub fn clear(&mut self) {
        self.values.clear();
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

#[cfg(test)]
mod tests {
    // use super::*;

    #[test]
    fn check_fractional_printing() {
        let n: f64 = 3.14;
        let n2: f64 = 3.0;

        println!("{n:.width$}", width = if n.fract() > 1e-6 { 2 } else { 0 });
        println!(
            "{n2:.width$}",
            width = if n2.fract() > 1e-6 { 2 } else { 0 }
        );
        println!("{}", f64::MIN_POSITIVE);
    }
}
