pub struct Lexer<'a> {
    source: &'a str,
    at: usize,
}

#[derive(Debug, PartialEq)]
#[rustfmt::skip]
pub enum TokenType {
    // Keywords
    Auto,      Enum,      Restrict,  Unsigned,
    Break,     Extern,    Return,    Void,
    Case,      Float,     Short,     Volatile,
    Char,      For,       Signed,    While,
    Const,     Goto,      Sizeof,    _Bool,
    Continue,  If,        Static,    _Complex,
    Default,   Inline,    Struct,    _Imaginary,
    Do,        Int,       Switch,
    Double,    Long,      Typedef,
    Else,      Register,  Union,

    Identifier,

    // Constants
    IntLit,
    FloatLit,
    CharLit,
    StringLit,

    // Punctuators

    // (    )
    RBrac, LBrac,

    // [    ]
    RBBrac, LBBrac,

    // {    }
    RBrace, LBrace,

    Dot,   Arrow,  Incr,
    Decr,  Amp,    Star,
    Plus,  Minus,  Tilde,
    Bang,  Div,    Mod,
    // <<    >>
    RShift,    LShift,
    // <     >
    RAngle,  LAngle,

    Leq,           Geq,           Eq,
    Neq,           Xor,           Or,
    LAnd,          LOr,           Question,
    Colon,         Terminate,     Ellipses,
    Assign,        MulAssign,     DivAssign,
    ModAssign,     AddAssign,     SubAssign,
    LShiftAssign,  RShiftAssign,  AndAssign,
    XorAssign,     OrAssign,      Comma,
    Hash,

    Unknown,
    EOF,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub ty: TokenType,
    pub start: usize,
    pub len: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, at: 0 }
    }

    fn advance_by(&mut self, len: usize) -> usize {
        let start = self.at;
        self.at += len;
        let (_, rest) = self.source.split_at(len);
        self.source = rest;
        start
    }

    fn eof(&self) -> Token {
        Token {
            ty: TokenType::EOF,
            start: self.at,
            len: 0,
        }
    }

    fn unknown(&self) -> Token {
        Token {
            ty: TokenType::Unknown,
            len: self.source.len(),
            start: self.at,
        }
    }

    fn token(&mut self, ty: TokenType, len: usize) -> Token {
        Token {
            ty,
            len,
            start: self.advance_by(len),
        }
    }

    fn skip_whitespace(&mut self) {
        let len_before = self.source.len();
        self.source = self.source.trim_ascii_start();
        self.at += len_before - self.source.len();
    }

    pub fn next_token(&mut self) -> Token {
        if self.source.is_empty() {
            return self.eof();
        }

        self.skip_whitespace();

        punctuator(self.source)
            .or_else(|| literal(self.source))
            .or_else(|| identifier(self.source))
            .map(|(ty, len)| self.token(ty, len))
            .unwrap_or_else(|| self.unknown())
    }
}

fn literal(src: &str) -> Option<(TokenType, usize)> {
    string_literal(src)
}

fn string_literal(src: &str) -> Option<(TokenType, usize)> {
    let '"' = src.chars().next()? else {
        return None;
    };

    let end = src[1..].find(['"', '\n'])? + 1;

    let '"' = src.chars().nth(end)? else {
        return None;
    };

    Some((TokenType::StringLit, end + 1))
}

fn identifier(src: &str) -> Option<(TokenType, usize)> {
    if src.chars().next().unwrap().is_ascii_digit() {
        return None;
    }

    let c_ident_pat = |c: char| c.is_ascii_alphanumeric() || c == '_';

    if let Some(len) = src.find(|c| !c_ident_pat(c)) {
        Some((TokenType::Identifier, len))
    } else if src.chars().all(c_ident_pat) {
        Some((TokenType::Identifier, src.len()))
    } else {
        None
    }
}

fn punctuator(src: &str) -> Option<(TokenType, usize)> {
    let three = src.get(..3).unwrap_or("");
    let two = src.get(..2).unwrap_or("");
    let one = src.get(..1).unwrap_or("");
    three_punctuator(three)
        .map(|tokenty| (tokenty, 3))
        .or(two_punctuator(two).map(|tokenty| (tokenty, 2)))
        .or(one_punctuator(one).map(|tokenty| (tokenty, 1)))
}

fn three_punctuator(p: &str) -> Option<TokenType> {
    match p {
        "..." => Some(TokenType::Ellipses),
        "<<=" => Some(TokenType::LShiftAssign),
        ">>=" => Some(TokenType::RShiftAssign),
        _ => None,
    }
}

fn two_punctuator(p: &str) -> Option<TokenType> {
    match p {
        "--" => Some(TokenType::Decr),
        "++" => Some(TokenType::Incr),
        "&&" => Some(TokenType::LAnd),
        "||" => Some(TokenType::LOr),
        "<=" => Some(TokenType::Leq),
        ">=" => Some(TokenType::Geq),
        "!=" => Some(TokenType::Neq),
        "==" => Some(TokenType::Eq),
        "->" => Some(TokenType::Arrow),
        ">>" => Some(TokenType::RShift),
        "<<" => Some(TokenType::LShift),
        "+=" => Some(TokenType::AddAssign),
        "-=" => Some(TokenType::SubAssign),
        "*=" => Some(TokenType::MulAssign),
        "/=" => Some(TokenType::DivAssign),
        "%=" => Some(TokenType::ModAssign),
        "^=" => Some(TokenType::XorAssign),
        "&=" => Some(TokenType::AndAssign),
        "|=" => Some(TokenType::OrAssign),
        _ => None,
    }
}

fn one_punctuator(p: &str) -> Option<TokenType> {
    match p {
        "(" => Some(TokenType::LBrac),
        ")" => Some(TokenType::RBrac),
        "[" => Some(TokenType::LBBrac),
        "]" => Some(TokenType::RBBrac),
        "{" => Some(TokenType::LBrace),
        "}" => Some(TokenType::RBrace),
        "?" => Some(TokenType::Question),
        "~" => Some(TokenType::Tilde),
        ";" => Some(TokenType::Terminate),
        ":" => Some(TokenType::Colon),
        "," => Some(TokenType::Comma),
        "<" => Some(TokenType::LAngle),
        ">" => Some(TokenType::RAngle),
        "." => Some(TokenType::Dot),
        "-" => Some(TokenType::Minus),
        "+" => Some(TokenType::Plus),
        "/" => Some(TokenType::Div),
        "&" => Some(TokenType::Amp),
        "|" => Some(TokenType::Or),
        "*" => Some(TokenType::Star),
        "%" => Some(TokenType::Mod),
        "^" => Some(TokenType::Xor),
        "!" => Some(TokenType::Bang),
        "=" => Some(TokenType::Assign),
        "#" => Some(TokenType::Hash),
        _ => None,
    }
}
