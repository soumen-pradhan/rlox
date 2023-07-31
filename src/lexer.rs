#![allow(dead_code)]

use std::{iter::Peekable, ops::Add};

use crate::utils::{Loc, Pos};

#[derive(Debug, PartialEq)]
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

pub struct Token {
    ty: Type,
    loc: Loc,
}

impl Token {
    fn new(ty: Type, loc: Loc) -> Self {
        Self { ty, loc }
    }
}

impl Add<Loc> for Type {
    type Output = Token;

    fn add(self, loc: Loc) -> Self::Output {
        Token::new(self, loc)
    }
}

pub struct Lexer<'a> {
    lines: &'a Vec<(usize, String)>,
}

impl<'a> Lexer<'a> {
    pub fn new(lines: &'a Vec<(usize, String)>) -> Self {
        Self { lines }
    }

    fn symbols(&self) -> LexerIterator<impl Iterator<Item = (Pos, char)> + 'a> {
        let symbols = self
            .lines
            .iter()
            .flat_map(|(line_no, line): &(usize, String)| {
                line.char_indices().map(|(col, c)| (Pos(*line_no, col), c))
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
    StrMultiline,
    StrUnterminated
}

struct LexerIterator<T: Iterator> {
    symbols: Peekable<T>,
    loc: Loc,
    done: bool,
}

impl<T> Iterator for LexerIterator<T>
where
    T: Iterator<Item = (Pos, char)>,
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
            ($c:expr, $left:expr, $right:expr) => {
                if self.match_consume($c) { $left } else { $right }
            };
        }

        let ty = match self.symbols.next() {
            None => {
                self.done = true;
                self.loc.end += 1;
                self.loc.start = self.loc.end;
                Eof
            }

            Some((pos, sym)) => {
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

                    '"' => self.scan_str().unwrap(),

                    '/' => self.check_skip_comment(),

                    ' ' | '\r' | '\t' | '\n' => self.skip_whitespace(),

                    _ => Unknown,
                }
            }
        };

        Some(ty + self.loc)
    }
}

impl<T> LexerIterator<T>
where
    T: Iterator<Item = (Pos, char)>,
{
    fn consume(&mut self) -> Option<(Pos, char)> {
        let ret = self.symbols.next();

        if let Some((pos, _sym)) = ret {
            self.loc.end = pos;
        }

        ret
    }

    fn match_consume(&mut self, c: char) -> bool {
        if let Some((_pos, sym)) = self.symbols.peek() {
            if *sym == c {
                self.consume();
                return true;
            }
        }

        false
    }

    /*
    private fun scanStr(): TokenType {
        val str = buildString {
            while (chars.hasNext()) {
                val (char, pos) = chars.peek() ?: break

                Log.end = pos

                if (char == '"') {
                    chars.next()
                    break
                }

                if (pos.char == 1) append("\n")
                append(chars.next().char)
            }
        }

        return if (chars.hasNext()) {
            STRING(str)
        } else {
            Log.err { msg = "Unterminated String" }
            UNKNOWN
        }
    }
    */

    fn scan_str(&mut self) -> Result<Type, LexerErr> {
        use LexerErr::*;

        let mut literal = String::new();

        while let Some((pos, sym)) = self.consume() {
            if pos.1 == 0 {
                return Err(StrMultiline);
            }

            if sym == '"' {
                return Ok(Type::Str(literal));
            }

            literal.push(sym);
        }

        Err(StrUnterminated)
    }

    fn check_skip_comment(&mut self) -> Type {
        if let Some((_pos, sym)) = self.symbols.peek() {
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

        while let Some((pos, _sym)) = self.symbols.peek() {
            // Check if new line (first character wiil be at col 0)
            if pos.1 == 0 {
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
            if let Some((_pos, sym)) = self.consume() {
                match sym {
                    '/' => {
                        if let Some((_pos, '*')) = self.symbols.peek() {
                            self.consume();
                            depth += 1;
                        }
                    }

                    '*' => {
                        if let Some((_pos, '/')) = self.symbols.peek() {
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
        while let Some((_pos, ' ' | '\r' | '\t' | '\n')) = self.symbols.peek() {
            self.consume();
        }

        Type::Whitespace
    }
}

#[cfg(test)]
mod tests {
    use super::{Type::*, *};

    #[test]
    fn check_primitives() {
        let lines = vec![
            (0, "({  })".to_string()),
            (1, ",.-+;*".to_string()),
            (3, "! != = == < <= >  >=".to_string()),
        ];

        let expected = vec![
            LParen, LBrace, Whitespace, RBrace, RParen, Comma, Dot, Minus, Plus, Semicolon, Star,
            Bang, Whitespace, BangEq, Whitespace, Eq, Whitespace, EqEq, Whitespace, Less,
            Whitespace, LessEq, Whitespace, Greater, Whitespace, GreaterEq, Eof,
        ];

        let tokens = Lexer::new(&lines)
            .symbols()
            .map(|tok| tok.ty)
            .collect::<Vec<_>>();

        assert_eq!(expected, tokens);
    }

    #[test]
    fn check_comments() {
        let lines = vec![
            (0, "/  /  // lorem".to_string()),
            (1, "/* efoimekfmdm */".to_string()),
            (3, "/* dsdsdsd /* dfdfawd */ wdadwadaw */".to_string()),
            (4, "/* efoimekfmdm ".to_string()),
            (5, "efoimekfmdm */".to_string()),
        ];

        let expected = vec![
            Slash, Whitespace, Slash, Whitespace, Comment, Comment, Comment, Comment, Eof,
        ];

        let tokens = Lexer::new(&lines)
            .symbols()
            .map(|tok| tok.ty)
            .collect::<Vec<_>>();

        assert_eq!(expected, tokens);
    }

    #[test]
    fn check_strings() {
        let lines = vec![
            (0, "\"lorem ipsum\"".to_string()),
        ];

        let expected = vec![
            Str("lorem ipsum".to_string()), Eof,
        ];

        let tokens = Lexer::new(&lines)
            .symbols()
            .map(|tok| tok.ty)
            .collect::<Vec<_>>();

        assert_eq!(expected, tokens);
    }
}
