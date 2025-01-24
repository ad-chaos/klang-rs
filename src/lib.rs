#![allow(dead_code)]

use std::io::BufRead;

pub struct Lexer<T> {
    stream: T,
}

#[derive(Debug)]
#[rustfmt::skip]
pub enum Token {
    // Keywords
    Auto,       Enum,        Restrict,     Unsigned,
    Break,      Extern,      Return,       Void,
    Case,       Float,       Short,        Volatile,
    Char,       For,         Signed,       While,
    Const,      Goto,        Sizeof,       _Bool,
    Continue,   If,          Static,       _Complex,
    Default,    Inline,      Struct,       _Imaginary,
    Do,         Int,         Switch,
    Double,     Long,        Typedef,
    Else,       Register,    Union,

    Identifier(String),

    // Constants
    IntLit(u64),
    FloatLit(f64),
    CharLit(char),
    StringLit(String),

    // Punctuators

    // (    )
    RBrac, LBrac,

    // [    ]
    RBBrac, LBBrac,

    // {    }
    RBrace, LBrace,

    Dot,       Arrow,    Incr,
    Decr,      Amp,      Star,
    Plus,      Minus,    Tilde,
    Bang,      Slash,    Percent,
    // <<    >>
    RGen,    LGen,
    // <     >
    RAngle,  LAngle,

    Leq,          Geq,          Eq,
    Neq,          Xor,          Or,
    LAnd,         LOr,          Question,
    Colon,        Terminate,    Ellipse,
    Assign,       MulAssign,    DivAssign,
    ModAssign,    AddAssign,    SubAssign,
    LShiftAssign, RShiftAssign, AndAssign,
    XorAssign,    OrAssign,     Comma,
    Hash,         Concat,

    Invalid(String),
    EOF,
}

impl<T> Lexer<T> {
    pub fn new(stream: T) -> Self {
        Self { stream }
    }
}

impl<T> Lexer<T>
where
    T: BufRead,
{
    pub fn next_token(&mut self) -> Option<Token> {
        match self.stream.bytes().peekable().peek() {
            Some()
        }
    }
}

fn parse_ident_or_keyword(lex: &Lexer) -> Option<Token> {}
fn parse_literal(lex: &Lexer) -> Option<Token> {}
fn parse_punctuator(lex: &Lexer) -> Option<Token> {}
