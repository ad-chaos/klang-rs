use core::panic;

use klang::{LexError, Lexer, Token, TokenType};
use LexError::*;
use TokenType::*;

// {{{ Macros
macro_rules! t {
    ($lex:ident => $token:ident at [$start:expr, $len:expr]) => {
        assert_eq!(
            $lex.next_token().unwrap(),
            Token {
                ty: $token,
                start: $start,
                len: $len
            }
        );
    };
    ($lex:ident => $err:expr) => {
        assert_eq!($lex.next_token(), Err($err))
    };
    (not $lex:ident => $token:pat) => {
        assert_ne!($lex.next_token().unwrap().ty, $token)
    };
}

macro_rules! lex {
    ($src:literal => $tokens:expr) => {{
        let source = $src;
        let mut lex = Lexer::new(source.as_bytes());

        let mut start = 0;
        for (token, rtoken) in $tokens
            .into_iter()
            .zip(source.split_whitespace())
        {
            t!(lex => token at [start, rtoken.len()]);
            start += 1 + rtoken.len();
        }

        t!(lex => EOF at [source.len(), 0]);
    }};
    ($($rtok:literal => $token:ident),+, $(,)?) => {{
        let source = [$($rtok),+].join(" ");
        let mut lex = Lexer::new(source.as_bytes());

        let mut start = 0;
        for (token, rtoken) in [$($token),+]
            .into_iter()
            .zip(source.split_whitespace())
        {
            t!(lex => token at [start, rtoken.len()]);
            start += 1 + rtoken.len();
        }

        t!(lex => EOF at [source.len(), 0]);
    }};
}
// }}}

#[test]
fn single_byte_tokens() {
    lex!(
        "(" => LParen,
        ")" => RParen,
        "[" => LSquare,
        "]" => RSquare,
        "{" => LBrace,
        "}" => RBrace,
        "?" => Question,
        "~" => Tilde,
        ";" => Terminate,
        ":" => Colon,
        "," => Comma,
        "<" => LAngle,
        ">" => RAngle,
        "." => Dot,
        "-" => Minus,
        "+" => Plus,
        "/" => Div,
        "&" => Amp,
        "|" => Or,
        "*" => Star,
        "%" => Mod,
        "^" => Xor,
        "!" => Bang,
        "#" => Hash,
        "=" => Assign,
    );
}

#[test]
fn two_byte_tokens() {
    lex!(
        "--" => Decr,
        "++" => Incr,
        "&&" => LAnd,
        "||" => LOr,
        "<=" => Leq,
        ">=" => Geq,
        "!=" => Neq,
        "==" => Eq,
        "->" => Arrow,
        ">>" => RShift,
        "<<" => LShift,
        "+=" => AddAssign,
        "-=" => SubAssign,
        "*=" => MulAssign,
        "/=" => DivAssign,
        "%=" => ModAssign,
        "^=" => XorAssign,
        "&=" => AndAssign,
        "|=" => OrAssign,
    );
}

#[test]
fn three_byte_tokens() {
    lex!(
        "..." => Ellipses,
        "<<=" => LShiftAssign,
        ">>=" => RShiftAssign,
    );
}

#[test]
fn keyword_tokens() {
    lex!(
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
    );
}

#[test]
fn ident_tokens() {
    lex!(
        "__klang" => Identifier,
        "KLANG" => Identifier,
        "cogito_ergo_sum" => Identifier,
        "thing123" => Identifier,
    );
}

#[test]
fn string_tokens() {
    let s1: &[u8] = br#"" \0 \\ \" \" ""#;
    let s2: &[u8] = br#""Another-string""#;
    let s3: &[u8] = br#""More Strings!!:O""#;
    let s4: &[u8] = br#""A string""#;
    let s5: &[u8] = br#"L"A \\wide// string literal""#;

    let tests = [s1, s2, s3, s4, s5];

    let source = tests.join(b"\t\n\r ".as_slice());

    let mut lex = Lexer::new(&source);

    for (i, off, len) in tests.iter().enumerate().scan((0, 0, 0), |acc, (i, x)| {
        acc.0 = i;
        acc.1 += acc.2;
        acc.2 = x.len();
        Some(*acc)
    }) {
        t!(lex => StringLit at [i*4 + off, len]);
    }
    t!(lex => EOF at [source.len(), 0]);
}

#[test]
fn char_tokens() {
    let s1: &[u8] = br#"' '"#;
    let s2: &[u8] = br#"'\r'"#;
    let s3: &[u8] = br#"'\n'"#;
    let s4: &[u8] = br#"'abcd'"#;
    let s5: &[u8] = br#"'\x000'"#;

    let tests = [s1, s2, s3, s4, s5];

    let source = tests.join(b"\t\n\r ".as_slice());

    let mut lex = Lexer::new(&source);

    for (i, off, len) in tests.iter().enumerate().scan((0, 0, 0), |acc, (i, x)| {
        acc.0 = i;
        acc.1 += acc.2;
        acc.2 = x.len();
        Some(*acc)
    }) {
        t!(lex => CharLit at [i*4 + off, len]);
    }
    t!(lex => EOF at [source.len(), 0]);
}

#[test]
fn unterm_string_tokens() {
    let mut lex = Lexer::new(br#""unterminated"#);
    t!(lex => InvalidStringLit(0));

    let mut lex = Lexer::new(b"\"unterminated with new line\n\n");
    t!(lex => InvalidStringLit(0));
}

#[test]
fn int_tokens() {
    lex!(
        "0" => DecIntLit,
        "1" => DecIntLit,
        "2" => DecIntLit,
        "3" => DecIntLit,
        "4" => DecIntLit,
        "5" => DecIntLit,
        "6" => DecIntLit,
        "7" => DecIntLit,
        "8" => DecIntLit,
        "9" => DecIntLit,
        "123" => DecIntLit,
        "123uL" => DecIntLit,
        "0Xff" => HexIntLit,
        "0xff" => HexIntLit,
        "0xabcULL" => HexIntLit,
        "0X123LLu" => HexIntLit,
        "0XFFLL" => HexIntLit,
        "0xFFLL" => HexIntLit,
        "0xabc" => HexIntLit,
        "0777llU" => OctIntLit,
        "0777" => OctIntLit,
    );
}

#[test]
fn float_tokens() {
    lex!(
        "1.0" => DecFloatLit,
        "0.5" => DecFloatLit,
        "3.14" => DecFloatLit,
        "2.718" => DecFloatLit,
        "0.001" => DecFloatLit,
        "1e5" => DecFloatLit,
        "2.5e-3" => DecFloatLit,
        "1.23e+2" => DecFloatLit,
        "3e7" => DecFloatLit,
        "0.0004e-2" => DecFloatLit,
        "3.14" => DecFloatLit,
        "0.001" => DecFloatLit,
        "123.456e-7" => DecFloatLit,
        ".123" => DecFloatLit,
        "0.123" => DecFloatLit,
        "1.0e5" => DecFloatLit,
        "1e1" => DecFloatLit,
        "1." => DecFloatLit,
        "1.1" => DecFloatLit,
        "1.e1" => DecFloatLit,
        ".1" => DecFloatLit,
        ".1e1" => DecFloatLit,
        "0.e+1" => DecFloatLit,
        "0.e+1f" => DecFloatLit,
        "1.e1" => DecFloatLit,
        "01.e1" => DecFloatLit,
        "1.L" => DecFloatLit,
        "0x1p1" => HexFloatLit,
        "0x1.8p2" => HexFloatLit,
        "0xA.3p-1" => HexFloatLit,
        "0x1.2p3" => HexFloatLit,
        "0x.1p+1" => HexFloatLit,
        "0x.fp1F" => HexFloatLit,
        "0x1p+1" => HexFloatLit,
    )
}

#[test]
fn not_float_tokens() {
    let source = b"e2 2.3e p3 0x 0xa.af";
    let mut lex = Lexer::new(source);
    loop {
        match lex.next_token() {
            Ok(Token { ty: EOF, .. }) => break,
            Ok(token) => {
                assert_ne!(token.ty, DecFloatLit, "Got a {:?}", token);
                assert_ne!(token.ty, HexFloatLit, "Got a {:?}", token);
            }
            Err(err) => panic!("Error while lexing! {:?}", err),
        }
    }
}
