#![allow(dead_code)]

use std::{collections::HashMap, iter::Peekable, ops::Add};

use lazy_static::lazy_static;

use crate::{
    error::prelude::*,
    utils::{Loc, Pos},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    // Single-character tokens.
    LParen,
    RParen,
    LBrace,
    RBrace,

    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEq,
    Eq,
    EqEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,

    // Literals.
    Ident(String),
    Str(String),
    Num(f64),

    // Keywords.
    KwAnd,
    KwClass,
    KwElse,
    KwFun,
    KwFor,
    KwIf,
    KwNil,
    KwOr,
    KwPrint,
    KwReturn,
    KwSuper,
    KwThis,
    KwVar,
    KwWhile,
    KwTrue,
    KwFalse,

    // Ignore
    Comment,
    Whitespace,

    Unknown,

    Eof,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Type> = HashMap::from([
        ("and", Type::KwAnd),
        ("class", Type::KwClass),
        ("else", Type::KwElse),
        ("fun", Type::KwFun),
        ("for", Type::KwFor),
        ("if", Type::KwIf),
        ("nil", Type::KwNil),
        ("or", Type::KwOr),
        ("print", Type::KwPrint),
        ("return", Type::KwReturn),
        ("super", Type::KwSuper),
        ("this", Type::KwThis),
        ("var", Type::KwVar),
        ("while", Type::KwWhile),
        ("true", Type::KwTrue),
        ("false", Type::KwFalse),
    ]);
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: Type,
    pub loc: Loc,
}

impl Add<Loc> for Type {
    type Output = Token;

    fn add(self, loc: Loc) -> Self::Output {
        Token { ty: self, loc }
    }
}

#[derive(Clone)]
pub struct PosChar(Pos, char);

pub struct Lexer<'a> {
    lines: &'a Vec<String>,
}

impl<'a> Lexer<'a> {
    pub fn new(lines: &'a Vec<String>) -> Self {
        Self { lines }
    }

    pub fn tokens(&self) -> impl Iterator<Item = Token> + 'a {
        let symbols = self
            .lines
            .iter()
            .enumerate()
            .flat_map(|(line_no, line)| {
                line.char_indices()
                    .map(move |(col, c)| PosChar(Pos(line_no, col), c))
            })
            .peekable();

        LexerIterator {
            symbols,
            loc: Loc::empty(),
            done: false,
        }
    }
}

#[derive(Debug)]
enum LexerErr {
    StrUnterminated,
    FloatInvalid,
    DanglingPoint,
}

impl LexerErr {
    fn show(&self) -> &str {
        match self {
            LexerErr::StrUnterminated => "string is unterminated",
            LexerErr::FloatInvalid => "invalid float literal",
            LexerErr::DanglingPoint => "dangling point",
        }
    }
}

#[derive(Clone)]
struct LexerIterator<I: Iterator<Item = PosChar>> {
    symbols: Peekable<I>,
    loc: Loc,
    done: bool,
}

impl<I> Iterator for LexerIterator<I>
where
    I: Iterator<Item = PosChar> + Clone,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        use Type::*;

        // This macro is just a convenience of if else
        #[rustfmt::skip]
        macro_rules! match_consume {
            ($sym:expr, $left:expr, $right:expr) => {
                if self.match_consume($sym) { $left } else { $right }
            };
        }

        let ty = match self.symbols.next() {
            None => {
                self.done = true;
                self.loc.end += 1;
                self.loc.start = self.loc.end;
                Eof
            }

            Some(PosChar(pos, sym)) => {
                self.loc.start = pos;
                self.loc.end = pos;

                match sym {
                    '(' => LParen,
                    ')' => RParen,
                    '{' => LBrace,
                    '}' => RBrace,
                    ',' => Comma,
                    '.' => Dot,
                    '-' => Minus,
                    '+' => Plus,
                    ';' => Semicolon,
                    '*' => Star,

                    '!' => match_consume!('=', BangEq, Bang),
                    '=' => match_consume!('=', EqEq, Eq),
                    '<' => match_consume!('=', LessEq, Less),
                    '>' => match_consume!('=', GreaterEq, Greater),

                    '0'..='9' => self.scan_num(sym).unwrap_or_else(|num_err| {
                        log!(self.loc, num_err.show());
                        Unknown
                    }),

                    'a'..='z' | 'A'..='Z' | '_' => self.scan_ident(sym),

                    '"' => self.scan_str().unwrap_or_else(|str_err| {
                        log!(self.loc, str_err.show());
                        Unknown
                    }),

                    '/' => self.check_skip_comment(),

                    ' ' | '\r' | '\t' | '\n' => self.skip_whitespace(),

                    _ => Unknown,
                }
            }
        };

        Some(ty + self.loc)
    }
}

impl<I> LexerIterator<I>
where
    I: Iterator<Item = PosChar>,
{
    fn peek(&mut self) -> Option<&I::Item> {
        self.symbols.peek()
    }

    fn consume(&mut self) -> Option<PosChar> {
        let ret = self.symbols.next();

        if let Some(PosChar(pos, _sym)) = ret {
            self.loc.end = pos;
        }

        ret
    }

    fn match_consume(&mut self, c: char) -> bool {
        if let Some(PosChar(_pos, sym)) = self.symbols.peek() {
            if *sym == c {
                self.consume();
                return true;
            }
        }

        false
    }

    fn scan_ident(&mut self, curr: char) -> Type {
        let mut literal = String::from(curr);

        while let Some(PosChar(_pos, sym)) = self.peek() {
            let sym = *sym;
            if !sym.is_ascii_alphanumeric() && sym != '_' {
                break;
            }
            literal.push(sym);
            self.consume();
        }

        if let Some(ty) = KEYWORDS.get(literal.as_str()) {
            ty.clone()
        } else {
            Type::Ident(literal)
        }
    }

    // TODO Add string interpolation
    fn scan_str(&mut self) -> Result<Type, LexerErr> {
        let mut literal = String::new();

        while let Some(PosChar(_pos, sym)) = self.symbols.peek() {
            let sym = *sym;

            if sym == '\n' {
                break;
            }

            self.consume();

            if sym == '"' {
                return Ok(Type::Str(literal));
            }

            literal.push(sym);
        }

        Err(LexerErr::StrUnterminated)
    }

    fn check_skip_comment(&mut self) -> Type {
        if let Some(PosChar(_pos, sym)) = self.symbols.peek() {
            match sym {
                '/' => self.skip_line_comment(),
                '*' => self.skip_block_comment(),
                _ => Type::Slash,
            }
        } else {
            Type::Slash
        }
    }

    fn skip_line_comment(&mut self) -> Type {
        self.consume();

        while let Some(PosChar(_pos, sym)) = self.symbols.peek() {
            if *sym == '\n' {
                break;
            }
            self.consume();
        }

        Type::Comment
    }

    fn skip_block_comment(&mut self) -> Type {
        self.consume();

        let mut depth = 1;

        while depth > 0 {
            if let Some(PosChar(_pos, sym)) = self.consume() {
                match sym {
                    '/' => {
                        if let Some(PosChar(_pos, '*')) = self.symbols.peek() {
                            self.consume();
                            depth += 1;
                        }
                    }

                    '*' => {
                        if let Some(PosChar(_pos, '/')) = self.symbols.peek() {
                            self.consume();
                            depth -= 1;
                        }
                    }

                    _ => {
                        continue;
                    }
                };
            } else {
                break;
            }
        }

        Type::Comment
    }

    fn skip_whitespace(&mut self) -> Type {
        while let Some(PosChar(_pos, ' ' | '\r' | '\t' | '\n')) = self.symbols.peek() {
            self.consume();
        }

        Type::Whitespace
    }
}

impl<I> LexerIterator<I>
where
    I: Iterator<Item = PosChar> + Clone,
{
    fn peek_next(&mut self) -> Option<I::Item> {
        let mut copy = self.clone();
        copy.next();
        copy.peek().cloned()
    }

    fn scan_num(&mut self, curr: char) -> Result<Type, LexerErr> {
        let mut literal = String::from(curr);

        while let Some(PosChar(_pos, sym)) = self.symbols.peek() {
            let sym = *sym;

            // check if fractional part exists
            if !sym.is_ascii_digit() {
                if sym == '.' {
                    // check if the dot is a decimal or a method call.
                    // If at least one digit exist after dot, it's a literal
                    if let Some(PosChar(_pos, '0'..='9')) = self.peek_next() {
                        literal.push('.');
                        self.consume(); // consume the '.'

                        while let Some(PosChar(_pos, sym)) = self.symbols.peek() {
                            if !sym.is_ascii_digit() {
                                break;
                            }

                            literal.push(*sym);
                            self.consume();
                        }
                    } else {
                        // dangling dot. method call or float literal ??
                        return Err(LexerErr::DanglingPoint);
                    }
                }

                break;
            }

            literal.push(sym);
            self.consume();
        }

        literal
            .parse::<f64>()
            .map(Type::Num)
            .map_err(|_| LexerErr::FloatInvalid)
    }
}

#[cfg(test)]
mod tests {
    #![allow(unused_unsafe)]

    use crate::error::prelude::*;
    use super::{Type::*, *};

    #[test]
    fn check_primitives() {
        let lines = vec![
            "({  })\n".to_string(),
            ",.-+;*\n".to_string(),
            "! != = == < <= >  >=\n".to_string(),
        ];

        let expected = vec![
            LParen, LBrace, Whitespace, RBrace, RParen, Whitespace, Comma, Dot, Minus, Plus,
            Semicolon, Star, Whitespace, Bang, Whitespace, BangEq, Whitespace, Eq, Whitespace,
            EqEq, Whitespace, Less, Whitespace, LessEq, Whitespace, Greater, Whitespace, GreaterEq,
            Whitespace, Eof,
        ];

        let tokens = Lexer::new(&lines)
            .tokens()
            .map(|tok| tok.ty)
            .collect::<Vec<_>>();

        assert_eq!(expected, tokens);
    }

    #[test]
    fn check_comments() {
        let lines = vec![
            "/  / // Lorem ipsum dolor.\n".to_string(),
            "/* Sed eu risus. */\n".to_string(),
            "/* consectetur /* adipiscing */ elit. */\n".to_string(),
            "/* Maecenas rutrum non\n".to_string(),
            "est quis hendrerit.*/\n".to_string(),
        ];

        let expected = vec![
            Slash, Whitespace, Slash, Whitespace, Comment, Whitespace, Comment, Whitespace,
            Comment, Whitespace, Comment, Whitespace, Eof,
        ];

        log_context! {
            @lines;

            let tokens = Lexer::new(&lines)
                .tokens()
                .map(|tok| tok.ty)
                .collect::<Vec<_>>();

            assert_eq!(expected, tokens);
        }
    }

    #[test]
    fn check_strings() {
        let lines = vec!["\"lorem ipsum\"\n".to_string()];

        let expected = vec![Str("lorem ipsum".to_string()), Whitespace, Eof];

        log_context! {
            @lines;

            let tokens = Lexer::new(&lines)
                .tokens()
                .map(|tok| tok.ty)
                .collect::<Vec<_>>();

            assert_eq!(expected, tokens);

        }

    }

    #[test]
    fn check_strings_error() {
        let lines = vec![
            "\"Lorem ipsum\n".to_string(),
            "\"consectetur adipiscing elit.\"\n".to_string(),
        ];

        let expected = vec![
            Unknown,
            Whitespace,
            Str("consectetur adipiscing elit.".to_string()),
            Whitespace,
            Eof,
        ];

        log_context! {
            @lines;

            let tokens = Lexer::new(&lines)
                .tokens()
                .map(|tok| tok.ty)
                .collect::<Vec<_>>();

            assert_eq!(expected, tokens);
        }

    }

    #[test]
    fn check_numbers() {
        let lines = vec!["2 3. 3.0 3.14\n".to_string(), "3.\n".to_string()];

        let expected = vec![
            Num(2.0),
            Whitespace,
            Unknown,
            Dot,
            Whitespace,
            Num(3.0),
            Whitespace,
            Num(3.14),
            Whitespace,
            Unknown,
            Dot,
            Whitespace,
            Eof,
        ];

        log_context! {
            @lines;

            let tokens = Lexer::new(&lines)
                .tokens()
                .map(|tok| tok.ty)
                .collect::<Vec<_>>();

            println!("{tokens:?}");

            assert_eq!(expected, tokens);
        }

    }

    #[test]
    fn check_keywords() {
        let lines = vec![
            "fun main () {\n".to_string(),
            "    print 3;\n".to_string(),
            "}\n".to_string(),
        ];

        let expected = vec![
            KwFun,
            Whitespace,
            Ident("main".to_string()),
            Whitespace,
            LParen,
            RParen,
            Whitespace,
            LBrace,
            Whitespace,
            KwPrint,
            Whitespace,
            Num(3.0),
            Semicolon,
            Whitespace,
            RBrace,
            Whitespace,
            Eof,
        ];

        log_context! {
            @lines;

            let tokens = Lexer::new(&lines)
                .tokens()
                .map(|tok| tok.ty)
                .collect::<Vec<_>>();

            assert_eq!(expected, tokens);
        }

    }
}
