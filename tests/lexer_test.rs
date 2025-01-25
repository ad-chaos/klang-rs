use klang::{Lexer, Token, TokenType};
use TokenType::*;
#[test]
fn single_byte_tokens() {
    let mut lex = Lexer::new("( ) [ ] { } ? ~ ; : , < > . - + / & | * % ^ ! # =");

    for (i, token) in [
        LBrac, RBrac, LBBrac, RBBrac, LBrace, RBrace, Question, Tilde, Terminate, Colon, Comma,
        LAngle, RAngle, Dot, Minus, Plus, Div, Amp, Or, Star, Mod, Xor, Bang, Hash, Assign,
    ]
    .into_iter()
    .enumerate()
    {
        assert_eq!(
            lex.next_token(),
            Token {
                ty: token,
                start: i * 2,
                len: 1
            }
        );
    }
}

#[test]
fn two_byte_tokens() {
    let mut lex = Lexer::new("-- ++ && || <= >= != == -> >> << += -= *= /= %= ^= &= |=");

    for (i, token) in [
        Decr, Incr, LAnd, LOr, Leq, Geq, Neq, Eq, Arrow, RShift, LShift, AddAssign, SubAssign,
        MulAssign, DivAssign, ModAssign, XorAssign, AndAssign, OrAssign,
    ]
    .into_iter()
    .enumerate()
    {
        assert_eq!(
            lex.next_token(),
            Token {
                ty: token,
                start: i * 3,
                len: 2
            }
        );
    }
}

#[test]
fn three_byte_tokens() {
    let mut lex = Lexer::new("... <<= >>=");

    for (i, token) in [Ellipses, LShiftAssign, RShiftAssign]
        .into_iter()
        .enumerate()
    {
        assert_eq!(
            lex.next_token(),
            Token {
                ty: token,
                start: i * 4,
                len: 3
            }
        );
    }
}

#[test]
fn ident_tokens() {
    let source = "__klang KLANG cogito_ergo_sum thing123";
    let mut lex = Lexer::new(source);

    let mut start = 0;
    for ident in source.split(' ') {
        assert_eq!(
            lex.next_token(),
            Token {
                ty: Identifier,
                start,
                len: ident.len()
            }
        );
        start += ident.len() + 1;
    }
}

#[test]
fn string_tokens() {
    let mut lex = Lexer::new(r#""A string" "Another-String""#);
    let s1 = r#""A string""#;
    let s2 = r#""Another-string""#;
    let s3 = r#""unknown token"#;

    assert_eq!(
        lex.next_token(),
        Token {
            ty: StringLit,
            start: 0,
            len: s1.len()
        }
    );

    assert_eq!(
        lex.next_token(),
        Token {
            ty: StringLit,
            start: 1 + s1.len(),
            len: s2.len()
        }
    );
}
