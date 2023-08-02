#![allow(dead_code)]

use std::{cmp::min, iter::Peekable, ops::Add};

use crate::{
    chunk::{constant_add_store, debug::disassemble, Chunk, OpCode},
    error::prelude::*,
    lexer::{
        Lexer, Token,
        Type::{self, *},
    },
    utils::Loc,
    value::Value, vm::VM,
};

use debug::stack_trace;

pub fn run_code(lines: &Vec<String>) {
    // Assumes token has no whitespace
    let tokens = Lexer::new(&lines)
        .tokens()
        .filter(|s| s.ty != Whitespace)
        .peekable();

    let chunk = parse(tokens).unwrap();

    #[cfg(any(test, debug_assertions))]
    disassemble(&chunk, "simple math");

    let mut vm = VM::new();
    vm.interpret(&chunk);
}

#[derive(Debug, PartialEq, PartialOrd)]
#[repr(u8)]
enum Precedence {
    Not = 0,
    Assign,
    Or,
    And,
    Eq,
    Cmp,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Add<u8> for Precedence {
    type Output = Precedence;

    fn add(self, increment: u8) -> Self::Output {
        let mut repr = self as u8;
        repr = min(repr + increment, 10);

        unsafe { std::mem::transmute(repr) }
    }
}

pub struct Parser<I: Iterator> {
    prev: Token,
    tokens: Peekable<I>,
    chunk: Chunk,
    err: bool,
    panic: bool,
}

// TODO write this as a macro
// parse! {
//     advance
//     expr
// }
fn parse<I>(tokens: Peekable<I>) -> Option<Chunk>
where
    I: Iterator<Item = Token>,
{
    let mut parser = Parser::new(tokens);

    parser
        .expr()?
        .consume(Eof, "Expected end of expression")?
        .ret()?;

    parser.done()
}

type ParserFn<I> = fn(&mut Parser<I>) -> Option<&mut Parser<I>>;

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(tokens: Peekable<I>) -> Self {
        Self {
            prev: Eof + Loc::empty(),
            tokens,
            chunk: Chunk::new(),
            err: false,
            panic: false,
        }
    }

    // Error due to | no more tokens | Unknown token (Lexer error)
    fn advance(&mut self) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "advance");

        self.prev = match self.tokens.next()? {
            t_err if t_err.ty == Unknown => return None,
            t => t,
        };

        Some(self)
    }

    // Error due to | no more tokens | incorrect type
    fn consume(&mut self, ty: Type, err_msg: &str) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "consume");

        match self.tokens.peek()? {
            t if t.ty == ty => self.advance(),

            tok_err => {
                log!(tok_err.loc, err_msg);
                None
            }
        }
    }

    #[rustfmt::skip]
    // (prefix, infix, precedence)
    fn get_rule(ty: &Type) -> (Option<ParserFn<I>>, Option<ParserFn<I>>, Precedence) {
        match ty {
            LParen    => (Some(Self::grouping), None,               Precedence::Not),
            Plus      => (None,                 Some(Self::binary), Precedence::Term),
            Minus     => (Some(Self::unary),    Some(Self::binary), Precedence::Term),
            Semicolon => (None,                 None,               Precedence::Not),
            Slash     => (None,                 Some(Self::binary), Precedence::Factor),
            Star      => (None,                 Some(Self::binary), Precedence::Factor),
            Num(_)    => (Some(Self::number),      None,               Precedence::Not),
            _         => (None,                 None,               Precedence::Not)
        }
    }

    // Error dur to | no more tokens | prefix | infix error
    fn parse_precedence(&mut self, prec: Precedence) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "parse_precedence");

        self.advance()?;

        // first token will always be a prefix expr
        if let (Some(prefix_rule), _, _) = Self::get_rule(&self.prev.ty) {
            prefix_rule(self)?;
        } else {
            log!(self.prev.loc, "Expected an expression (prefix)");
            return None;
        }

        while prec <= Self::get_rule(&self.tokens.peek()?.ty).2 {
            self.advance()?;

            if let (_, Some(infix_rule), _) = Self::get_rule(&self.prev.ty) {
                infix_rule(self)?;
            } else {
                log!(self.prev.loc, "Expected an expression (infix)");
                return None;
            }
        }

        Some(self)
    }

    fn expr(&mut self) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "expr");

        self.parse_precedence(Precedence::Assign)
    }

    // Error due to | not a number token | not enough space in chunk pool
    fn number(&mut self) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "number");

        match self.prev {
            Token { ty: Num(n), loc } => {
                constant_add_store(&mut self.chunk, Value::Num(n), loc.start.0);
                Some(self)
            }
            _ => {
                log!(self.prev.loc, "Expected a number");
                None
            }
        }
    }

    // Error due to | expr error | no RParen after it
    fn grouping(&mut self) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "grouping");

        self.expr()?
            .consume(RParen, "Expected ')' after expression")
    }

    // Error dut to | expr error | not a minus
    // TODO check if works. Negate opcode after the exression bytes
    fn unary(&mut self) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "unary");

        let operator = self.prev.clone();

        self.parse_precedence(Precedence::Unary)?;

        match operator {
            t if t.ty == Minus => {
                self.chunk.add_op(OpCode::Negate, t.loc.start.0);
                Some(self)
            }
            _ => None,
        }
    }

    fn binary(&mut self) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "binary");

        let operator = self.prev.clone();

        let (_, _, prec) = Self::get_rule(&operator.ty);
        self.parse_precedence(prec + 1)?;

        let op_src_line = operator.loc.start.0;

        match operator {
            Token { ty: Plus, .. } => self.chunk.add_op(OpCode::Add, op_src_line),
            Token { ty: Minus, .. } => self.chunk.add_op(OpCode::Subtract, op_src_line),
            Token { ty: Star, .. } => self.chunk.add_op(OpCode::Multiply, op_src_line),
            Token { ty: Slash, .. } => self.chunk.add_op(OpCode::Divide, op_src_line),

            _ => return None,
        };

        Some(self)
    }

    fn ret(&mut self) -> Option<&mut Self> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "ret");

        self.chunk.add_op(OpCode::Return, self.prev.loc.start.0);
        Some(self)
    }

    fn done(self) -> Option<Chunk> {
        if !self.err && !self.panic {
            Some(self.chunk)
        } else {
            None
        }
    }
}

#[cfg(any(test, debug_assertions))]
mod debug {
    use super::*;

    pub fn stack_trace<I>(_parser: &mut Parser<I>, _name: &str)
    where
        I: Iterator<Item = Token>,
    {
        // log!(parser.prev.loc, name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn precedence_inc() {
        use Precedence::*;

        let computed = (0..12).map(|i| Not + i).collect::<Vec<_>>();
        let expected = [
            Not, Assign, Or, And, Eq, Cmp, Term, Factor, Unary, Call, Primary, Primary,
        ];

        assert_eq!(computed, expected);
    }

    #[test]
    fn compute_math() {
        let lines = vec!["(-1 + 2) * 3 - -4\n".to_string()];

        log_context! {
            @lines;
            run_code(&lines);
        }
    }
}
