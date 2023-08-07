#![allow(dead_code, unused_unsafe)]

use owo_colors::OwoColorize;

use crate::utils::Loc;

pub mod prelude {
    pub use super::{ErrorLogger, Logger, LOGGER};
    pub use crate::{log, log_context};
}

pub trait Logger {
    fn err_loc(&self, loc: Loc, msg: &str);
    fn err_line(&self, line: usize, msg: &str);
}

pub struct ErrorLogger<'a> {
    pub lines: Option<&'a Vec<String>>,
}

pub static mut LOGGER: ErrorLogger<'static> = ErrorLogger { lines: None };

#[macro_export]
macro_rules! log_context {
    { @ $lines:expr; $($token:tt)* } => {
        unsafe {
            let og_lines = LOGGER.lines;
            // Get a reference with 'static lifetime, extending the lifetime of `lines`
            let lines_static: &'static Vec<String> = std::mem::transmute(&$lines);
            LOGGER.lines = Some(lines_static);

            $($token)*

            // Reset LOGGER.lines to None at the end of the block
            LOGGER.lines = og_lines;
        }
    };
}

#[macro_export]
macro_rules! log {
    // TODO expr to specific types
    (line $loc:expr, $msg:expr) => {
        unsafe {
            LOGGER.err_line($loc, $msg);
        }
    };

    ($loc:expr, $msg:expr) => {
        unsafe {
            LOGGER.err_loc($loc, $msg);
        }
    };
}

impl<'a> ErrorLogger<'a> {
    fn get_line(&self, n: usize) -> &str {
        match self.lines {
            None => "",
            Some(lines) => match lines.get(n) {
                None => "",
                Some(line) => line.trim_end_matches('\n'),
            },
        }
    }
}

impl<'a> Logger for ErrorLogger<'a> {
    fn err_loc(&self, loc: Loc, msg: &str) {
        let Loc { start, end } = loc;

        let line_pad = digits(end.0);
        let (e, e_red) = ("", "".red()); // empty str
        let msg = msg.red();

        // single line
        if start.0 == end.0 {
            println!("{} | {}", start.0 + 1, self.get_line(start.0));
            println!(
                "{e:line_pad$} | {e:space_pad$}{e_red:^<caret_pad$} {msg}",
                space_pad = start.1,
                caret_pad = end.1 - start.1 + 1
            );

            return;
        }

        // multiple lines
        {
            let first_line = self.get_line(start.0);
            println!("{:line_pad$} | {first_line}", start.0 + 1);
            println!(
                "{e:line_pad$} | {e:space_pad$}{e_red:^<caret_pad$}",
                space_pad = start.1,
                caret_pad = first_line.len() - start.1
            );

            // print kink if more than 2 lines
            if end.0 > start.0 + 1 {
                println!("{e:line_pad$} \\");
            }

            println!("{:line_pad$} | {}", end.0 + 1, self.get_line(end.0));
            println!(
                "{e:line_pad$} | {e_red:^<caret_pad$} {msg}",
                caret_pad = end.1
            );
        }
    }

    fn err_line(&self, line: usize, msg: &str) {
        println!("{} | {} {}", line + 1, self.get_line(line), msg.red());
    }
}

fn digits(mut n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut width = 0;

    while n > 0 {
        n /= 10;
        width += 1;
    }

    width
}

#[cfg(test)]
mod tests {
    use crate::utils::Pos;

    use super::*;

    #[test]
    fn check() {
        let lines = vec!["Lorem ipsum".to_string()];

        log_context! {
            @lines;

            let loc = Loc {
                start: Pos(0, 6),
                end: Pos(0, 10),
            };

            let msg = "Something went wrong";

            log!(loc, msg);
        }
    }

    #[test]
    fn width_of_numbers() {
        let numbers: Vec<usize> = vec![0, 1, 2, 20, 102, 99999];
        let expected = vec![1, 1, 1, 2, 3, 5];

        let computed = numbers.iter().map(|n| digits(*n)).collect::<Vec<_>>();

        assert_eq!(expected, computed);
    }

    #[test]
    fn single_line_error() {
        let lines = vec![
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit.\n".to_string(),
            "Maecenas rutrum non est quis hendrerit.\n".to_string(),
            "Sed eu risus.\n".to_string(),
        ];

        log_context! {
            @lines;

            let loc = Loc {
                start: Pos(0, 6),
                end: Pos(0, 11),
            };

            let msg = "Something went wrong";

            log!(loc, msg);
        }
    }

    #[test]
    fn multiple_line_error() {
        let lines = vec![
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit.\n".to_string(),
            "Maecenas rutrum non est quis hendrerit.\n".to_string(),
            "Sed eu risus.\n".to_string(),
        ];

        log_context! {
            @lines;

            let loc = Loc {
                start: Pos(0, 0),
                end: Pos(2, 3),
            };

            let msg = "Something went wrong";

            log!(loc, msg);
        }
    }
}
