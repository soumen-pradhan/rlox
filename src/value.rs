#![allow(dead_code)]

use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
    Symbol(String),
    Nil,
    Unit,
}

impl Value {
    pub fn to_logical(&self) -> bool {
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
            Unit => write!(f, "Unit"),

            Num(n) => {
                let precision = if n.abs().fract() > f64::EPSILON { 2 } else { 0 };
                write!(f, "{n:.precision$}")
            }

            Str(s) | Symbol(s) => {
                write!(f, "{s}")
            }
        }
    }
}

pub type ValuePool = Vec<Value>;

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

    #[test]
    fn check_value_to_logical() {
        use Value::*;

        let vals = [
            Bool(true),
            Bool(false),
            Nil,
            Num(3.04),
            Str("waffles xs".into()),
            Symbol("var_2".into()),
            Unit,
        ]
        .map(|v| v.to_logical());

        let expected = [true, false, false, true, true, true, true];

        assert_eq!(vals, expected);
    }
}
