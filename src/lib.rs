pub struct Lexer<'a> {
    source: &'a str,
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
    IntLit,
    FloatLit,
    CharLit,
    StringLit,

    // Punctuators

    // (     )
    RBrac,   LBrac,
    // [     ]
    RBBrac,  LBBrac,
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

macro_rules! lex {
    ($self:ident, token: $($rule:ident)|*) => {
        $(match Lexer::$rule($self) {
            Ok(token) => return Ok(token),
            Err(LexError::UnLexable) => {},
            Err(err) => return Err(err)
        })*
    };
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

    fn literal(&mut self) -> Result<Token, LexError> {
        self.string_literal()
    }

    fn string_literal(&mut self) -> Result<Token, LexError> {
        let '"' = self.source.chars().next().ok_or(LexError::NeedInput)? else {
            return Err(LexError::UnLexable);
        };

        let end = self.source[1..]
            .find(['"', '\n'])
            .ok_or(LexError::InvalidStringLit(self.at))?
            + 1;

        let '"' = self.source.chars().nth(end).unwrap() else {
            return Err(LexError::InvalidStringLit(self.at));
        };

        Ok(self.token(TokenType::StringLit, end + 1))
    }

    fn identifier(&mut self) -> Result<Token, LexError> {
        let c_ident_nondigit = |c: char| c.is_ascii_alphabetic() || c == '_';
        let c_ident = |c: char| c.is_ascii_alphanumeric() || c == '_';

        let fst = self.source.chars().next().ok_or(LexError::NeedInput)?;
        if !c_ident_nondigit(fst) {
            return Err(LexError::UnLexable);
        }

        if let Some(len) = self.source.find(|c| !c_ident(c)) {
            Ok(self.token(to_keyword(&self.source[..len]), len))
        } else if self.source.chars().all(c_ident) {
            Ok(self.token(to_keyword(self.source), self.source.len()))
        } else {
            Err(LexError::UnLexable)
        }
    }

    fn punctuator(&mut self) -> Result<Token, LexError> {
        let three = self.source.get(..3).unwrap_or("");
        let two = self.source.get(..2).unwrap_or("");
        let one = self.source.get(..1).unwrap_or("");

        let three = three_punctuator(three).map(|tokenty| (tokenty, 3));
        let two = two_punctuator(two).map(|tokenty| (tokenty, 2));
        let one = one_punctuator(one).map(|tokenty| (tokenty, 1));

        three
            .or(two)
            .or(one)
            .map(|(ty, len)| self.token(ty, len))
            .ok_or(LexError::UnLexable)
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        if self.source.is_empty() {
            return Ok(self.eof());
        }

        self.skip_whitespace();

        lex!(self,
        token: punctuator |
               identifier |
               literal
        );

        Err(LexError::UnknownToken((self.at, self.source.len())))
    }
}

fn three_punctuator(p: &str) -> Option<TokenType> {
    use TokenType::*;
    match p {
        "..." => Some(Ellipses),
        "<<=" => Some(LShiftAssign),
        ">>=" => Some(RShiftAssign),
        _ => None,
    }
}

fn two_punctuator(p: &str) -> Option<TokenType> {
    use TokenType::*;
    match p {
        "--" => Some(Decr),
        "++" => Some(Incr),
        "&&" => Some(LAnd),
        "||" => Some(LOr),
        "<=" => Some(Leq),
        ">=" => Some(Geq),
        "!=" => Some(Neq),
        "==" => Some(Eq),
        "->" => Some(Arrow),
        ">>" => Some(RShift),
        "<<" => Some(LShift),
        "+=" => Some(AddAssign),
        "-=" => Some(SubAssign),
        "*=" => Some(MulAssign),
        "/=" => Some(DivAssign),
        "%=" => Some(ModAssign),
        "^=" => Some(XorAssign),
        "&=" => Some(AndAssign),
        "|=" => Some(OrAssign),
        _ => None,
    }
}

fn one_punctuator(p: &str) -> Option<TokenType> {
    use TokenType::*;
    match p {
        "(" => Some(LBrac),
        ")" => Some(RBrac),
        "[" => Some(LBBrac),
        "]" => Some(RBBrac),
        "{" => Some(LBrace),
        "}" => Some(RBrace),
        "?" => Some(Question),
        "~" => Some(Tilde),
        ";" => Some(Terminate),
        ":" => Some(Colon),
        "," => Some(Comma),
        "<" => Some(LAngle),
        ">" => Some(RAngle),
        "." => Some(Dot),
        "-" => Some(Minus),
        "+" => Some(Plus),
        "/" => Some(Div),
        "&" => Some(Amp),
        "|" => Some(Or),
        "*" => Some(Star),
        "%" => Some(Mod),
        "^" => Some(Xor),
        "!" => Some(Bang),
        "=" => Some(Assign),
        "#" => Some(Hash),
        _ => None,
    }
}

fn to_keyword(p: &str) -> TokenType {
    use TokenType::*;
    match p {
        "auto" => Auto,
        "break" => Break,
        "case" => Case,
        "char" => Char,
        "const" => Const,
        "continue" => Continue,
        "default" => Default,
        "do" => Do,
        "double" => Double,
        "else" => Else,
        "enum" => Enum,
        "extern" => Extern,
        "float" => Float,
        "for" => For,
        "goto" => Goto,
        "if" => If,
        "inline" => Inline,
        "int" => Int,
        "long" => Long,
        "register" => Register,
        "restrict" => Restrict,
        "return" => Return,
        "short" => Short,
        "signed" => Signed,
        "sizeof" => Sizeof,
        "static" => Static,
        "struct" => Struct,
        "switch" => Switch,
        "typedef" => Typedef,
        "union" => Union,
        "unsigned" => Unsigned,
        "void" => Void,
        "volatile" => Volatile,
        "while" => While,
        "_Bool" => _Bool,
        "_Complex" => _Complex,
        "_Imaginary" => _Imaginary,
        _ => Identifier,
    }
}
