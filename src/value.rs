#![allow(dead_code)]

use std::fmt::Display;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Nil,
    Num(f64),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        use Value::*;

        match self {
            Bool(false) | Nil => false,
            _ => true,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        match self {
            Bool(b) => write!(f, "{b}"),
            Nil => write!(f, "nil"),

            Num(n) => {
                let precision = if n.abs().fract() > f64::EPSILON { 2 } else { 0 };
                write!(f, "{n:.precision$}")
            }
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

fn find_precision(n: f64, max_prec: usize) -> usize {
    const EPSILON: f64 = 1e-8;

    let mut n = n.abs();
    let mut precision = 0;

    while n.fract() > f64::EPSILON && precision < max_prec {
        println!("    {}", n.fract());
        n *= 10.0;
        precision += 1;
    }
    println!("end    {}", n.fract());

    precision
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_fractional_printing() {
        const EPSILON: f64 = 1e-8;
        println!("{EPSILON}");

        for n in [1.0, -1.0, 1.10, 1.12, 1.123, 1.1230001, 1.12345] {
            println!(
                "{n} | {n:.p$} | {}",
                (n as f64).fract(),
                p = find_precision(n, 3)
            );
        }
    }
}
