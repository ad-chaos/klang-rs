use std::{error::Error, fmt::Display};

#[derive(Copy, Clone)]
pub struct Lexer<'a> {
    pub source: &'a [u8],
    pub at: usize,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LexError {
    UnknownToken(usize),
    InvalidStringLit(usize),
    UnLexable,
    NeedInput,
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for LexError {}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let ntoken = self.next_token();
        if let Ok(Token {
            ty: TokenType::EOF, ..
        }) = ntoken
        {
            return None;
        }
        Some(ntoken)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
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
    OctIntLit,
    DecIntLit,
    HexIntLit,
    IntLitSuffix,

    DecFloatLit,
    HexFloatLit,
    FloatLitSuffix,

    CharLit,
    StringLit,

    // Punctuators

    // (     )
    RParen,   LParen,
    // [     ]
    RSquare,  LSquare,
    // {     }
    RBrace,  LBrace,
    // <<    >>
    RShift,  LShift,
    // <     >
    RAngle,  LAngle,

    Dot,    Arrow,  Incr,
    Decr,   Amp,    Star,
    Plus,   Minus,  Tilde,
    Bang,   Div,    Mod,
    Leq,    Geq,    Eq,
    Neq,    Xor,    Or,
    LAnd,   LOr,    Colon,
    Comma,  Hash,   Ellipses,

    Question,   Terminate,  Assign,
    MulAssign,  DivAssign,  ModAssign,
    AddAssign,  SubAssign,  OrAssign,
    AndAssign,  XorAssign,

    RShiftAssign, LShiftAssign,

    AnyToken,
    EOF,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Token {
    pub ty: TokenType,
    pub start: usize,
    pub len: usize,
}

impl Token {
    pub fn empty() -> Self {
        Self {
            ty: TokenType::AnyToken,
            start: 0,
            len: 0,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.ty == TokenType::EOF
    }

    fn fuse(mut self, other: Result<Token, LexError>) -> Token {
        if let Ok(token) = other {
            self.len += token.len;
        }
        self
    }

    fn as_type(mut self, ty: TokenType) -> Token {
        self.ty = ty;
        self
    }
}

macro_rules! lex {
    ($self:ident, token: $($rule:ident)|*) => {
        $(match Lexer::$rule($self) {
            Ok(token) => return Ok(token),
            Err(LexError::UnLexable) => {},
            Err(err) => return Err(err)
        })*

        return Err(LexError::UnknownToken($self.at));
    };
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self { source, at: 0 }
    }

    fn advance_by(&mut self, len: usize) -> usize {
        let (_, rest) = self.source.split_at(len);
        self.source = rest;

        let start = self.at;
        self.at += len;
        start
    }

    fn eof(&self) -> Token {
        Token {
            ty: TokenType::EOF,
            start: self.at,
            len: 0,
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

    fn try_string(&mut self, quotechar: u8) -> Result<Token, LexError> {
        let mut bytes = self.source.iter();

        let ltype = if quotechar == b'\'' {
            TokenType::CharLit
        } else {
            TokenType::StringLit
        };

        let skip = match (bytes.next().ok_or(LexError::NeedInput)?, ltype) {
            (b'\'', TokenType::CharLit) => 1,
            (b'"', TokenType::StringLit) => 1,
            (b'L', _) => {
                let Some(b'"' | b'\'') = bytes.next() else {
                    return Err(LexError::UnLexable);
                };
                2
            }
            _ => return Err(LexError::UnLexable),
        };

        let mut string = &self.source[skip..];

        loop {
            let end = string
                .iter()
                .position(|b| *b == quotechar || *b == b'\\' || *b == b'\n')
                .ok_or(LexError::InvalidStringLit(self.at))?;

            match (string.get(end), ltype) {
                (Some(b'"'), TokenType::StringLit) => {
                    string = &string[end..];
                    break;
                }
                (Some(b'\''), TokenType::CharLit) => {
                    string = &string[end..];
                    break;
                }
                (Some(b'\\'), _) => {
                    string = string.get(end + 2..).ok_or(LexError::UnLexable)?;
                }
                _ => return Err(LexError::InvalidStringLit(self.at)),
            }
        }

        Ok(self.token(ltype, self.source.len() - string.len() + 1))
    }

    fn string_literal(&mut self) -> Result<Token, LexError> {
        let rst = *self;
        let string = self.try_string(b'"');
        if string.is_err() {
            *self = rst;
        }

        string
    }

    fn char_literal(&mut self) -> Result<Token, LexError> {
        let rst = *self;
        let chr = self.try_string(b'\'');
        if chr.is_err() {
            *self = rst;
        }

        chr
    }

    fn float_int_digits(&mut self, itype: TokenType) -> Result<Token, LexError> {
        let is_valid_digit: fn(&u8) -> bool = match itype {
            TokenType::HexIntLit => |c| c.is_ascii_hexdigit(),
            TokenType::OctIntLit | TokenType::DecIntLit => |c| c.is_ascii_digit(),
            _ => unreachable!(),
        };

        match self.source.iter().position(|c| !is_valid_digit(c)) {
            Some(len) if len > 0 => return Ok(self.token(itype, len)),
            _ => {}
        }

        if self.source.iter().all(is_valid_digit) {
            return Ok(self.token(itype, self.source.len()));
        }

        Err(LexError::UnLexable)
    }

    fn int_prefixed_digits(&mut self) -> Result<Token, LexError> {
        let fst = self.source.first().ok_or(LexError::NeedInput)?;
        let Some(snd) = self.source.get(1) else {
            if fst.is_ascii_digit() {
                return Ok(self.token(TokenType::DecIntLit, 1));
            }
            return Err(LexError::UnLexable);
        };

        let (itype, is_valid_digit): (TokenType, fn(&u8) -> bool) = match (*fst, *snd) {
            (b'0', b'x' | b'X') => (TokenType::HexIntLit, |c| c.is_ascii_hexdigit()),
            (b'0', b'0'..=b'7') => (TokenType::OctIntLit, |c| matches!(c, b'0'..=b'7')),
            (b'1'..=b'9', b'0'..=b'9') => (TokenType::DecIntLit, |c| c.is_ascii_digit()),
            (b'0'..=b'9', _) => return Ok(self.token(TokenType::DecIntLit, 1)),
            _ => return Err(LexError::UnLexable),
        };

        if let Some(len) = self.source[2..].iter().position(|c| !is_valid_digit(c)) {
            Ok(self.token(itype, len + 2))
        } else if self.source[2..].iter().all(is_valid_digit) {
            Ok(self.token(itype, self.source.len()))
        } else {
            Err(LexError::UnLexable)
        }
    }

    fn int_literal(&mut self) -> Result<Token, LexError> {
        self.int_prefixed_digits()
            .map(|token| token.fuse(self.isuffix()))
    }

    fn try_float(&mut self) -> Result<Token, LexError> {
        let integer = self.int_prefixed_digits();

        let itype = integer
            .map(|token| token.ty)
            .unwrap_or(TokenType::DecIntLit);
        let ftype = matches!(itype, TokenType::HexIntLit)
            .then_some(TokenType::HexFloatLit)
            .unwrap_or(TokenType::DecFloatLit);

        let dot = matches!(self.source.first(), Some(b'.'))
            .then(|| self.token(TokenType::Dot, 1))
            .ok_or(LexError::UnLexable);

        let fractional = self.float_int_digits(itype);

        if integer.or(fractional).is_err() {
            return Err(LexError::UnLexable);
        }

        let (first, second) = if let Ok(token) = integer {
            (token, dot)
        } else if let Ok(token) = dot {
            (token, Ok(Token::empty()))
        } else {
            return Err(LexError::UnLexable);
        };

        let e = if let TokenType::HexFloatLit = ftype {
            matches!(self.source.first(), Some(b'p' | b'P'))
        } else {
            matches!(self.source.first(), Some(b'e' | b'E'))
        }
        .then(|| self.token(TokenType::Identifier, 1))
        .ok_or(LexError::UnLexable);

        if dot.or(e).is_err() || ftype == TokenType::HexFloatLit && e.is_err() {
            return Err(LexError::UnLexable);
        }

        let sign = matches!(self.source.first(), Some(b'+' | b'-'))
            .then(|| self.token(TokenType::Plus, 1))
            .ok_or(LexError::UnLexable);

        let exponent = self.float_int_digits(TokenType::DecIntLit);

        if let Some(Err(_)) = e.is_ok().then_some(exponent) {
            return Err(LexError::UnLexable);
        }

        let fsuffix = matches!(self.source.first(), Some(b'f' | b'F' | b'l' | b'L'))
            .then(|| self.token(TokenType::Identifier, 1))
            .ok_or(LexError::UnLexable);

        Ok(first
            .fuse(second)
            .fuse(fractional)
            .fuse(e)
            .fuse(sign)
            .fuse(exponent)
            .fuse(fsuffix)
            .as_type(ftype))
    }

    fn float_literal(&mut self) -> Result<Token, LexError> {
        let rst = *self;
        let float = self.try_float();
        if float.is_err() {
            *self = rst;
        }

        float
    }

    fn identifier(&mut self) -> Result<Token, LexError> {
        let c_ident_nondigit = |c: &u8| matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'_');
        let c_ident = |c: &u8| matches!(c, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_');

        let fst = self.source.first().ok_or(LexError::NeedInput)?;
        if !c_ident_nondigit(fst) {
            return Err(LexError::UnLexable);
        }

        if let Some(len) = self.source.iter().position(|c| !c_ident(c)) {
            return Ok(self.token(to_keyword(&self.source[..len]), len));
        }

        if self.source.iter().all(c_ident) {
            return Ok(self.token(to_keyword(self.source), self.source.len()));
        }

        Err(LexError::UnLexable)
    }

    fn resolve_static_token(
        &mut self,
        three_token: fn(&[u8]) -> Option<TokenType>,
        two_token: fn(&[u8]) -> Option<TokenType>,
        one_token: fn(&[u8]) -> Option<TokenType>,
    ) -> Option<(TokenType, usize)> {
        let three = self.source.get(..3).unwrap_or(&[0]);
        let two = self.source.get(..2).unwrap_or(&[0]);
        let one = self.source.get(..1).unwrap_or(&[0]);

        three_token(three)
            .map(|ret| (ret, 3))
            .or_else(|| two_token(two).map(|ret| (ret, 2)))
            .or_else(|| one_token(one).map(|ret| (ret, 1)))
    }

    fn isuffix(&mut self) -> Result<Token, LexError> {
        self.resolve_static_token(three_isuffix, two_isuffix, one_isuffix)
            .map(|(ty, len)| self.token(ty, len))
            .ok_or(LexError::UnLexable)
    }

    fn punctuator(&mut self) -> Result<Token, LexError> {
        self.resolve_static_token(three_punctuator, two_punctuator, one_punctuator)
            .map(|(ty, len)| self.token(ty, len))
            .ok_or(LexError::UnLexable)
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        if self.source.is_empty() {
            return Ok(self.eof());
        }

        lex!(self,
        token: float_literal  |
               punctuator     |
               string_literal |
               identifier     |
               char_literal   |
               int_literal
        );
    }
}

fn three_isuffix(p: &[u8]) -> Option<TokenType> {
    matches!(
        p,
        b"ull" | b"uLL" | b"Ull" | b"ULL" | b"llu" | b"llU" | b"LLu" | b"LLU"
    )
    .then_some(TokenType::IntLitSuffix)
}

fn two_isuffix(p: &[u8]) -> Option<TokenType> {
    matches!(
        p,
        b"ll" | b"LL" | b"ul" | b"uL" | b"Ul" | b"UL" | b"lu" | b"lU" | b"Lu" | b"LU"
    )
    .then_some(TokenType::IntLitSuffix)
}

fn one_isuffix(p: &[u8]) -> Option<TokenType> {
    matches!(p, b"l" | b"L" | b"u" | b"U").then_some(TokenType::IntLitSuffix)
}

fn three_punctuator(p: &[u8]) -> Option<TokenType> {
    use TokenType::*;
    match p {
        b"..." => Some(Ellipses),
        b"<<=" => Some(LShiftAssign),
        b">>=" => Some(RShiftAssign),
        _ => None,
    }
}

fn two_punctuator(p: &[u8]) -> Option<TokenType> {
    use TokenType::*;
    match p {
        b"--" => Some(Decr),
        b"++" => Some(Incr),
        b"&&" => Some(LAnd),
        b"||" => Some(LOr),
        b"<=" => Some(Leq),
        b">=" => Some(Geq),
        b"!=" => Some(Neq),
        b"==" => Some(Eq),
        b"->" => Some(Arrow),
        b">>" => Some(RShift),
        b"<<" => Some(LShift),
        b"+=" => Some(AddAssign),
        b"-=" => Some(SubAssign),
        b"*=" => Some(MulAssign),
        b"/=" => Some(DivAssign),
        b"%=" => Some(ModAssign),
        b"^=" => Some(XorAssign),
        b"&=" => Some(AndAssign),
        b"|=" => Some(OrAssign),
        _ => None,
    }
}

fn one_punctuator(p: &[u8]) -> Option<TokenType> {
    use TokenType::*;
    match p {
        b"(" => Some(LParen),
        b")" => Some(RParen),
        b"[" => Some(LSquare),
        b"]" => Some(RSquare),
        b"{" => Some(LBrace),
        b"}" => Some(RBrace),
        b"?" => Some(Question),
        b"~" => Some(Tilde),
        b";" => Some(Terminate),
        b":" => Some(Colon),
        b"," => Some(Comma),
        b"<" => Some(LAngle),
        b">" => Some(RAngle),
        b"." => Some(Dot),
        b"-" => Some(Minus),
        b"+" => Some(Plus),
        b"/" => Some(Div),
        b"&" => Some(Amp),
        b"|" => Some(Or),
        b"*" => Some(Star),
        b"%" => Some(Mod),
        b"^" => Some(Xor),
        b"!" => Some(Bang),
        b"=" => Some(Assign),
        b"#" => Some(Hash),
        _ => None,
    }
}

fn to_keyword(p: &[u8]) -> TokenType {
    use TokenType::*;
    match p {
        b"auto" => Auto,
        b"break" => Break,
        b"case" => Case,
        b"char" => Char,
        b"const" => Const,
        b"continue" => Continue,
        b"default" => Default,
        b"do" => Do,
        b"double" => Double,
        b"else" => Else,
        b"enum" => Enum,
        b"extern" => Extern,
        b"float" => Float,
        b"for" => For,
        b"goto" => Goto,
        b"if" => If,
        b"inline" => Inline,
        b"int" => Int,
        b"long" => Long,
        b"register" => Register,
        b"restrict" => Restrict,
        b"return" => Return,
        b"short" => Short,
        b"signed" => Signed,
        b"sizeof" => Sizeof,
        b"static" => Static,
        b"struct" => Struct,
        b"switch" => Switch,
        b"typedef" => Typedef,
        b"union" => Union,
        b"unsigned" => Unsigned,
        b"void" => Void,
        b"volatile" => Volatile,
        b"while" => While,
        b"_Bool" => _Bool,
        b"_Complex" => _Complex,
        b"_Imaginary" => _Imaginary,
        _ => Identifier,
    }
}
