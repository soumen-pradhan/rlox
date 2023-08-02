use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use crate::{compiler::run_code, error::prelude::*};

mod chunk;
mod compiler;
mod error;
mod lexer;
mod utils;
mod value;
mod vm;

fn main() {
    let file = File::open("test/sample.lox").unwrap();
    let buffer = BufReader::new(file);

    let lines = buffer
        .lines()
        .filter_map(|line| line.ok().map(|l| l + "\n"))
        .collect::<Vec<_>>();

    log_context! {
        @lines;
        run_code(&lines);
    }
}
