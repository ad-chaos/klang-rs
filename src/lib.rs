pub struct Lexer<'a> {
    source: &'a [u8],
    at: usize,
}

#[derive(Debug, PartialEq)]
pub enum LexError {
    UnknownToken((usize, usize)),
    InvalidStringLit(usize),
    UnLexable,
    NeedInput,
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

    EOF,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub ty: TokenType,
    pub start: usize,
    pub len: usize,
}

impl Token {
    fn fuse(mut self, other: Result<Token, LexError>) -> Token {
        if let Ok(token) = other {
            self.len += token.len;
        }
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

        return Err(LexError::UnknownToken(($self.at, $self.source.len())));
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

    fn string_literal(&mut self) -> Result<Token, LexError> {
        let b'"' = self.source.iter().next().ok_or(LexError::NeedInput)? else {
            return Err(LexError::UnLexable);
        };

        let end = &self.source[1..]
            .iter()
            .position(|b| *b == b'"' || *b == b'\n')
            .ok_or(LexError::InvalidStringLit(self.at))?
            + 1;

        let Some(b'"') = self.source.get(end) else {
            return Err(LexError::InvalidStringLit(self.at));
        };

        Ok(self.token(TokenType::StringLit, end + 1))
    }

    fn int_literal(&mut self) -> Result<Token, LexError> {
        let [fst, snd] = self.source.get(..2).ok_or(LexError::NeedInput)? else {
            unreachable!()
        };

        let (itype, is_valid_digit): (TokenType, fn(&u8) -> bool) = match fst {
            b'0' => {
                if let b'x' | b'X' = snd {
                    (TokenType::HexIntLit, |c| c.is_ascii_hexdigit())
                } else {
                    (TokenType::OctIntLit, |c| matches!(c, b'0'..=b'7'))
                }
            }
            b'1'..=b'9' => (TokenType::DecIntLit, |c| c.is_ascii_digit()),
            _ => return Err(LexError::UnLexable),
        };

        if !is_valid_digit(snd) && itype != TokenType::HexIntLit {
            return Ok(self.token(itype, 1));
        }

        if let Some(len) = self.source[2..].iter().position(|c| !is_valid_digit(c)) {
            let itoken = self.token(itype, len + 2);
            let isuffix = self.int_suffix();
            return Ok(itoken.fuse(isuffix));
        }

        if self.source[2..].iter().all(is_valid_digit) {
            let itoken = self.token(itype, self.source.len());
            let isuffix = self.int_suffix();
            return Ok(itoken.fuse(isuffix));
        }

        Err(LexError::UnLexable)
    }

    fn float_literal(&mut self) -> Result<Token, LexError> {
        Err(LexError::UnLexable)
    }

    fn char_literal(&mut self) -> Result<Token, LexError> {
        Err(LexError::UnLexable)
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

    fn int_suffix(&mut self) -> Result<Token, LexError> {
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
        if self.source.is_empty() {
            return Ok(self.eof());
        }

        self.skip_whitespace();

        lex!(self,
        token: punctuator     |
               string_literal |
               identifier     |
               int_literal    |
               float_literal  |
               char_literal
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
