use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use error::ErrorLogger;
use lexer::Lexer;

mod chunk;
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

    let logger = ErrorLogger { lines: &lines };

    for token in Lexer::new(&lines, &logger).symbols() {
        println!("{token:?}");
    }
}
