#![allow(dead_code)]

use std::{cmp::min, iter::Peekable, ops::Add};

use crate::{
    chunk::{constant_add_store, Chunk, OpCode},
    error::prelude::*,
    lexer::{
        Lexer, Token,
        Type::{self, *},
    },
    utils::Loc,
    value::Value,
    vm::VM,
};

use debug::stack_trace;

pub fn run_code(lines: &Vec<String>) {
    // Assumes token has no whitespace
    let tokens = Lexer::new(&lines)
        .tokens()
        .filter(|s| s.ty != Whitespace)
        .peekable();

    let chunk = parse(tokens).unwrap_or_else(|_| Chunk::new());

    let mut vm = VM::new();
    let _res = vm.interpret(&chunk);
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
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

/**
 * program    -> decl* EOF
 * decl       -> funcDecl | varDecl | statement
 * funcDecl   -> "fun" IDENT "(" param? ")" block
 * param      -> IDENT ( "," IDENT )*
 * varDecl    -> "var" IDENT ( "=" expression )? ";"
 * statement  -> exprStmt | ifStmt | printStmt | forStmt | whileStmt | block | retStmt
 * exprStmt   -> expression ";"
 * ifStmt     -> "if" expression "{" statement "}" ( "else" "{" statement "}" )?
 * printStmt  -> "print" expression ";"
 * forStmt    -> "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
 * whileStmt  -> "while" "(" expression ")" statement
 * block      -> "(" decl* ")"
 * retStmt    -> "return" expression? ";"
 * expression -> assignment
 * assignment -> IDENT "=" assignment | logic_or
 * logic_or   -> logic_and ( "or" logic_and )*
 * logic_and  -> equality ( "and" equality )*
 * equality   -> comparison ( ( "!=" | "==" ) comparison )*
 * comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )*
 * term       -> factor ( ( "-" | "+" ) factor )*
 * factor     -> unary ( ( "/" | "*" ) unary )*
 * unary      -> ( "!" | "-" ) unary | call
 * call       -> primary ( "(" arguments? ")" )*
 * arguments  -> expression ( "," expression )*
 * primary    -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENT
 */

pub struct Parser<I: Iterator> {
    prev: Token,
    tokens: Peekable<I>,
    chunk: Chunk,
    curr_prec: Precedence,
}

#[derive(Debug)]
enum ParserErr {
    NoMoreTokens,
    UnexpectedEof,
    UnknownToken,
    IncorrectToken,
    NotPrefix,
    NotInfix,
    ValuePoolOverflow,
}

// TODO write this as a macro
// parse! {
//     advance
//     expr
// }
fn parse<I>(tokens: Peekable<I>) -> Result<Chunk, ParserErr>
where
    I: Iterator<Item = Token>,
{
    let mut parser = Parser::new(tokens);

    while parser.curr()?.ty != Type::Eof {
        parser.decl()?;
    }

    parser.consume(Eof, "expected end of expression")?.ret()?;

    Ok(parser.chunk)
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(tokens: Peekable<I>) -> Self {
        Self {
            prev: Eof + Loc::empty(),
            tokens,
            chunk: Chunk::new(),
            curr_prec: Precedence::Not,
        }
    }

    fn curr(&mut self) -> Result<&Token, ParserErr> {
        self.tokens.peek().ok_or(ParserErr::NoMoreTokens)
    }

    fn next(&mut self) -> Result<Token, ParserErr> {
        self.tokens.next().ok_or(ParserErr::NoMoreTokens)
    }
}

type ParserFn<I> = fn(&mut Parser<I>) -> Result<&mut Parser<I>, ParserErr>;

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    // Error due to | no more tokens | Unknown token (Lexer error)
    fn advance(&mut self) -> Result<&mut Self, ParserErr> {
        self.prev = match self.tokens.next().ok_or(ParserErr::NoMoreTokens)? {
            t_err if t_err.ty == Unknown => {
                log!(t_err.loc, "unknown token found");
                return Err(ParserErr::UnknownToken);
            }
            t => t,
        };

        Ok(self)
    }

    // Error due to | no more tokens | incorrect type
    fn consume(&mut self, ty: Type, err_msg: &str) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, format!("consume {ty:?}").as_str());

        match self.curr()? {
            t if t.ty == ty => self.advance(),

            tok_err => {
                log!(tok_err.loc, err_msg);
                Err(ParserErr::IncorrectToken)
            }
        }
    }

    fn consume_ident(&mut self) -> Result<(u8, u8), ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "consume_ident");

        let val = match &self.prev {
            Token {
                ty: Type::Ident(s), ..
            } => Value::Symbol(s.clone()),
            tok => {
                log!(tok.loc, "expected a variable name");
                return Err(ParserErr::IncorrectToken);
            }
        };

        self.chunk
            .add_constant(val)
            .ok_or(ParserErr::ValuePoolOverflow)
    }

    // Error due to | no more tokens
    fn match_and_consume(&mut self, expected: Type) -> Result<bool, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, format!("consume if [{expected:?}]").as_str());

        match self.curr()? {
            t if t.ty == expected => {
                self.advance()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    // Error due to | no more tokens
    fn sync(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "sync");

        use Type::*;

        while self.curr()?.ty != Eof {
            if self.prev.ty == Semicolon {
                break;
            }

            if matches!(
                self.curr()?.ty,
                KwClass | KwFun | KwVar | KwFor | KwIf | KwWhile | KwPrint | KwReturn
            ) {
                break;
            }

            self.advance()?;
        }

        Ok(self)
    }

    #[rustfmt::skip]
    // (prefix, infix, precedence)
    fn get_rule(ty: &Type) -> (Option<ParserFn<I>>, Option<ParserFn<I>>, Precedence) {
        match ty {
            // Single-character tokens.
            LParen => (Some(Self::grouping), None, Precedence::Not),
            RParen => (None, None, Precedence::Not),
            LBrace => (None, None, Precedence::Not),
            RBrace => (None, None, Precedence::Not),

            Comma     => (None, None, Precedence::Not),
            Dot       => (None, None, Precedence::Not),
            Minus     => (Some(Self::unary), Some(Self::binary), Precedence::Term),
            Plus      => (None, Some(Self::binary), Precedence::Term),
            Semicolon => (None, None, Precedence::Not),
            Slash     => (None, Some(Self::binary), Precedence::Factor),
            Star      => (None, Some(Self::binary), Precedence::Factor),

            // One or two character tokens.
            Bang      => (Some(Self::unary), None, Precedence::Not),
            BangEq    => (None, Some(Self::binary), Precedence::Eq),
            Eq        => (None, None, Precedence::Not),
            EqEq      => (None, Some(Self::binary), Precedence::Eq),
            Greater   => (None, Some(Self::binary), Precedence::Cmp),
            GreaterEq => (None, Some(Self::binary), Precedence::Cmp),
            Less      => (None, Some(Self::binary), Precedence::Cmp),
            LessEq    => (None, Some(Self::binary), Precedence::Cmp),

            // Literals.
            Ident(_) => (Some(Self::variable), None, Precedence::Not),
            Str(_)   => (Some(Self::string), None, Precedence::Not),
            Num(_)   => (Some(Self::number), None, Precedence::Not),

            // Keywords.
            KwAnd    => (None, None, Precedence::Not),
            KwClass  => (None, None, Precedence::Not),
            KwElse   => (None, None, Precedence::Not),
            KwFun    => (None, None, Precedence::Not),
            KwFor    => (None, None, Precedence::Not),
            KwIf     => (None, None, Precedence::Not),
            KwNil    => (Some(Self::literal), None, Precedence::Not),
            KwOr     => (None, None, Precedence::Not),
            KwPrint  => (None, None, Precedence::Not),
            KwReturn => (None, None, Precedence::Not),
            KwSuper  => (None, None, Precedence::Not),
            KwThis   => (None, None, Precedence::Not),
            KwVar    => (None, None, Precedence::Not),
            KwWhile  => (None, None, Precedence::Not),
            KwTrue   => (Some(Self::literal), None, Precedence::Not),
            KwFalse  => (Some(Self::literal), None, Precedence::Not),

            // Ignore
            Comment    => (None, None, Precedence::Not),
            Whitespace => (None, None, Precedence::Not),

            Unknown => (None, None, Precedence::Not),

            Eof => (None, None, Precedence::Not),
        }
    }

    // Error dur to | no more tokens | prefix | infix error
    fn parse_precedence(&mut self, prec: Precedence) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, format!("parse_precedence {prec:?}").as_str());

        self.advance()?;

        // first token will always be a prefix expr
        if let (Some(prefix_rule), _, _) = Self::get_rule(&self.prev.ty) {
            prefix_rule(self)?;
        } else {
            log!(self.prev.loc, "expected an expression (prefix)");
            return Err(ParserErr::NotPrefix);
        }

        self.curr_prec = prec; // used in named_variable

        while prec <= Self::get_rule(&self.curr()?.ty).2 {
            self.advance()?;

            if let (_, Some(infix_rule), _) = Self::get_rule(&self.prev.ty) {
                infix_rule(self)?;
            } else {
                log!(self.prev.loc, "expected an expression (infix)");
                return Err(ParserErr::NotInfix);
            }
        }

        let can_assign = prec <= Precedence::Assign;
        if can_assign && self.match_and_consume(Type::Eq)? {
            log!(self.prev.loc, "invalid assignment target");
            return Err(ParserErr::IncorrectToken);
        }

        Ok(self)
    }

    // Error due to | no more tokens
    fn decl(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "decl");

        let res = if self.match_and_consume(Type::KwVar)? {
            self.var_decl()
        } else {
            self.statement()
        };

        if res.is_err() {
            self.sync()?;
        }

        Ok(self)
    }

    // Error due to | no more tokens | incorrect token | value pool overflow | ...
    fn var_decl(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "var_decl");

        self.advance()?;

        let index = self.consume_ident()?;

        // check the init value
        if self.match_and_consume(Type::Eq)? {
            self.expr()?;
        } else {
            self.chunk.add_op(OpCode::Nil, self.prev.loc.start.0);
        }

        self.consume(Type::Semicolon, "expected ';' after variable declaration")?;

        self.def_variable(index);

        Ok(self)
    }

    fn statement(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "statement");

        if self.match_and_consume(Type::KwPrint)? {
            // print_statement
            self.expr()?
                .consume(Type::Semicolon, "expected ';' after value")?;

            self.chunk.add_op(OpCode::Print, self.prev.loc.start.0);
        } else {
            // expr_statement
            self.expr()?
                .consume(Type::Semicolon, "expected ';' after value")?;

            self.chunk.add_op(OpCode::Pop, self.prev.loc.start.0);
        }
        Ok(self)
    }

    fn expr(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "expr");

        self.parse_precedence(Precedence::Assign)
    }

    // Error due to | not a number token | not enough space in chunk pool
    fn number(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "number");

        match self.prev {
            Token { ty: Num(n), loc } => {
                constant_add_store(&mut self.chunk, Value::Num(n), loc.start.0)
                    .ok_or(ParserErr::ValuePoolOverflow)?;
                Ok(self)
            }
            _ => {
                // unreachable
                log!(self.prev.loc, "expected a number");
                Err(ParserErr::IncorrectToken)
            }
        }
    }

    // Error due to | expr error | no RParen after it
    fn grouping(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "grouping");

        self.expr()?
            .consume(RParen, "Expected ')' after expression")
    }

    // Error dut to | expr error | not a minus
    // TODO check if works. Negate opcode after the exression bytes
    fn unary(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "unary");

        let operator = self.prev.clone();

        self.parse_precedence(Precedence::Unary)?;

        let line = operator.loc.start.0;

        match operator.ty {
            Minus => self.chunk.add_op(OpCode::Negate, line),
            Bang => self.chunk.add_op(OpCode::Not, line),
            _ => {
                log!(operator.loc, "expected an unary operator");
                return Err(ParserErr::IncorrectToken);
            }
        };

        Ok(self)
    }

    fn binary(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "binary");

        let operator = self.prev.clone();

        let (_, _, prec) = Self::get_rule(&operator.ty);
        self.parse_precedence(prec + 1)?;

        let line = operator.loc.start.0;

        match operator.ty {
            Plus => self.chunk.add_op(OpCode::Add, line),
            Minus => self.chunk.add_op(OpCode::Subtract, line),
            Star => self.chunk.add_op(OpCode::Multiply, line),
            Slash => self.chunk.add_op(OpCode::Divide, line),

            BangEq => self
                .chunk
                .add_op(OpCode::Eq, line)
                .add_op(OpCode::Not, line),

            EqEq => self.chunk.add_op(OpCode::Eq, line),
            Greater => self.chunk.add_op(OpCode::Greater, line),

            GreaterEq => self
                .chunk
                .add_op(OpCode::Less, line)
                .add_op(OpCode::Not, line),

            Less => self.chunk.add_op(OpCode::Less, line),

            LessEq => self
                .chunk
                .add_op(OpCode::Greater, line)
                .add_op(OpCode::Not, line),

            _ => {
                log!(operator.loc, "expected a binary operator");
                return Err(ParserErr::IncorrectToken);
            }
        };

        Ok(self)
    }

    fn literal(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "literal");

        let op_src_line = self.prev.loc.start.0;

        match self.prev.ty {
            Type::KwNil => self.chunk.add_op(OpCode::Nil, op_src_line),
            Type::KwTrue => self.chunk.add_op(OpCode::True, op_src_line),
            Type::KwFalse => self.chunk.add_op(OpCode::False, op_src_line),
            _ => {
                log!(self.prev.loc, "expected a valid literal");
                return Err(ParserErr::IncorrectToken);
            }
        };

        Ok(self)
    }

    fn string(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "string");

        if let Type::Str(s) = &self.prev.ty {
            constant_add_store(
                &mut self.chunk,
                Value::Str(s.clone()),
                self.prev.loc.start.0,
            );
            Ok(self)
        } else {
            log!(self.prev.loc, "expected a string literal");
            Err(ParserErr::IncorrectToken)
        }
    }

    fn def_variable(&mut self, (idx0, idx1): (u8, u8)) {
        let line = self.prev.loc.start.0;
        // def idx0 idx1 (always 2 bytes)
        self.chunk
            .add_op(OpCode::DefineGlobal, line)
            .add_byte(idx0, line)
            .add_byte(idx1, line);
    }

    fn variable(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "variable");

        self.named_variable()
    }

    fn named_variable(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "named_variable");

        let (idx0, idx1) = self.consume_ident()?;

        let line = self.prev.loc.start.0;

        // check if assignment should be considered or not
        let can_assign = self.curr_prec <= Precedence::Assign;

        if can_assign && self.match_and_consume(Type::Eq)? {
            self.expr()?.chunk.add_op(OpCode::SetGlobal, line);
        } else {
            self.chunk.add_op(OpCode::GetGlobal, line);
        }

        self.chunk.add_byte(idx0, line).add_byte(idx1, line);

        Ok(self)
    }

    fn ret(&mut self) -> Result<&mut Self, ParserErr> {
        #[cfg(any(test, debug_assertions))]
        stack_trace(self, "ret");

        self.chunk.add_op(OpCode::Return, self.prev.loc.start.0);
        Ok(self)
    }
}

#[cfg(any(test, debug_assertions))]
mod debug {
    use super::*;

    #[allow(unused_variables)]
    pub fn stack_trace<I>(parser: &mut Parser<I>, name: &str)
    where
        I: Iterator<Item = Token>,
    {
        log!(
            parser.prev.loc,
            format!("fn {name}, prev: {:?}", parser.prev.ty).as_str()
        );
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
    fn arithmetic() {
        let lines = vec!["(4 + 2);\n".to_string()];

        log_context! {
            @lines;
            run_code(&lines);
        }
    }

    #[test]
    fn logical() {
        let lines = vec!["!(5 - 4 > 3 * 2 == !nil);\n".to_string()];

        log_context! {
            @lines;
            run_code(&lines);
        }
    }

    #[test]
    fn string() {
        let lines = vec!["\"Ave\" + \"_\" + \"Caesar\";\n".to_string()];

        log_context! {
            @lines;
            run_code(&lines);
        }
    }

    #[test]
    fn print_stmt() {
        let lines = vec!["print (3 + 2) == 5;\n".to_string()];

        log_context! {
            @lines;
            run_code(&lines);
        }
    }

    #[test]
    fn var_stmt() {
        let lines = vec![
            "var name = 5.2;\n".to_string(),
            "print name;\n".to_string(),
            "name = \"Lorem\";\n".to_string(),
            "print name;\n".to_string(),
        ];

        log_context! {
            @lines;
            run_code(&lines);
        }
    }
}
