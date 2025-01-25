use klang::{LexError, Lexer, Token, TokenType};
use LexError::*;
use TokenType::*;

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
    ($src:literal => $tokens:expr, $len:expr) => {{
        let source = $src;
        let mut lex = Lexer::new(source);

        for (start, len, token) in $tokens
            .into_iter()
            .enumerate()
            .map(|(i, token)| (($len+1) * i, $len, token))
        {
            t!(lex => token at [start, len]);
        }

        t!(lex => EOF at [source.len(), 0]);
    }};
}

#[test]
fn single_byte_tokens() {
    lex!("( ) [ ] { } ? ~ ; : , < > . - + / & | * % ^ ! # =" => [
        LBrac,    RBrac, LBBrac,    RBBrac, LBrace, RBrace,
        Question, Tilde, Terminate, Colon,  Comma,  LAngle,
        RAngle,   Dot,   Minus,     Plus,   Div,    Amp,
        Or,       Star,  Mod,       Xor,    Bang,   Hash, Assign,
    ], 1);
}

#[test]
fn two_byte_tokens() {
    lex!("-- ++ && || <= >= != == -> >> << += -= *= /= %= ^= &= |=" => [
        Decr,      Incr,      LAnd,      LOr,       Leq,
        Geq,       Neq,       Eq,        Arrow,     RShift,
        LShift,    AddAssign, SubAssign, MulAssign, DivAssign,
        ModAssign, XorAssign, AndAssign, OrAssign,
    ], 2);
}

#[test]
fn three_byte_tokens() {
    lex!("... <<= >>=" => [Ellipses, LShiftAssign, RShiftAssign], 3);
}

#[test]
fn ident_tokens() {
    let source = "__klang KLANG cogito_ergo_sum thing123";
    let mut lex = Lexer::new(source);

    let mut start = 0;
    for ident in source.split(' ') {
        t!(lex => Identifier at [start, ident.len()]);
        start += ident.len() + 1;
    }

    t!(lex => EOF at [source.len(), 0]);
}

#[test]
fn string_tokens() {
    let s1 = r#""A string""#;
    let s2 = r#""Another-string""#;
    let s3 = r#""More Strings!!:O""#;

    let source = s1.to_string() + "\t\n\r " + s2 + "\t\n\r " + s3;

    let mut lex = Lexer::new(source.as_str());

    t!(lex => StringLit at [0,                       s1.len()]);
    t!(lex => StringLit at [4 + s1.len(),            s2.len()]);
    t!(lex => StringLit at [8 + s1.len() + s2.len(), s3.len()]);
    t!(lex => EOF at [source.len(), 0]);
}

#[test]
fn unterm_string_tokens() {
    let mut lex = Lexer::new(r#""unterminated"#);
    t!(lex => InvalidStringLit(0));

    let mut lex = Lexer::new("\"unterminated with new line\n\n\"");
    t!(lex => InvalidStringLit(0));
}
