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
}

macro_rules! lex {
    (concat($($src:literal),+) => $tokens:expr) => {{
        let source = concat!($($src),+);
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
}
// }}}

#[test]
fn single_byte_tokens() {
    lex!("( ) [ ] { } ? ~ ; : , < > . - + / & | * % ^ ! # =" => [
        LParen,   RParen, LSquare,   RSquare, LBrace, RBrace,
        Question, Tilde,  Terminate, Colon,   Comma,  LAngle,
        RAngle,   Dot,    Minus,     Plus,    Div,    Amp,
        Or,       Star,   Mod,       Xor,     Bang,   Hash, Assign,
    ]);
}

#[test]
fn two_byte_tokens() {
    lex!("-- ++ && || <= >= != == -> >> << += -= *= /= %= ^= &= |=" => [
        Decr,      Incr,      LAnd,      LOr,       Leq,
        Geq,       Neq,       Eq,        Arrow,     RShift,
        LShift,    AddAssign, SubAssign, MulAssign, DivAssign,
        ModAssign, XorAssign, AndAssign, OrAssign,
    ]);
}

#[test]
fn three_byte_tokens() {
    lex!("... <<= >>=" => [Ellipses, LShiftAssign, RShiftAssign]);
}

#[test]
fn keyword_tokens() {
    lex!(concat(
        "auto break case char const continue default do double else ",
        "enum extern float for goto if inline int long register restrict ",
        "return short signed sizeof static struct switch typedef union ",
        "unsigned void volatile while _Bool _Complex _Imaginary"
    ) => [
        Auto, Break, Case, Char, Const, Continue, Default, Do, Double, Else, Enum, Extern, Float,
        For, Goto, If, Inline, Int, Long, Register, Restrict, Return, Short, Signed, Sizeof,
        Static, Struct, Switch, Typedef, Union, Unsigned, Void, Volatile, While, _Bool, _Complex,
        _Imaginary,
    ]);
}

#[test]
fn ident_tokens() {
    lex!("__klang KLANG cogito_ergo_sum thing123" => [Identifier, Identifier, Identifier, Identifier]);
}

#[test]
fn string_tokens() {
    let s1 = r#""A string""#;
    let s2 = r#""Another-string""#;
    let s3 = r#""More Strings!!:O""#;

    let source = s1.to_string() + "\t\n\r " + s2 + "\t\n\r " + s3;

    let mut lex = Lexer::new(source.as_bytes());

    t!(lex => StringLit at [0,                       s1.len()]);
    t!(lex => StringLit at [4 + s1.len(),            s2.len()]);
    t!(lex => StringLit at [8 + s1.len() + s2.len(), s3.len()]);
    t!(lex => EOF at [source.len(), 0]);
}

#[test]
fn unterm_string_tokens() {
    let mut lex = Lexer::new(r#""unterminated"#.as_bytes());
    t!(lex => InvalidStringLit(0));

    let mut lex = Lexer::new("\"unterminated with new line\n\n\"".as_bytes());
    t!(lex => InvalidStringLit(0));
}

#[test]
fn int_tokens() {
    lex!(concat(
        "123 0xabc 0777 0X123 0Xff 0xFF ",
        "123uL 0xabcULL 0777llU 0X123LLu 0XffL 0xFFLL"
    ) => [
        DecIntLit, HexIntLit, OctIntLit, HexIntLit, HexIntLit, HexIntLit,
        DecIntLit, HexIntLit, OctIntLit, HexIntLit, HexIntLit, HexIntLit
    ]);
}
