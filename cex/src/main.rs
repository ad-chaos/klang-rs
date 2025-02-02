use axum::{
    response::Html,
    routing::{get, post},
    Router,
};
use klang::{LexError, Lexer, Token};
use tokio::time::{sleep_until, Duration, Instant};

macro_rules! color {
    ($($token:expr => $color:expr),+ ,) => {
        [$($color),+]
    };
}

static TOKCOLOR: [&str; 96] = color!(
    Auto => "#f7768e",
    Enum => "#f7768e",
    Restrict => "#f7768e",
    Unsigned => "#f7768e",
    Break => "#f7768e",
    Extern => "#f7768e",
    Return => "#f7768e",
    Void => "#f7768e",
    Case => "#f7768e",
    Float => "#f7768e",
    Short => "#f7768e",
    Volatile => "#f7768e",
    Char => "#f7768e",
    For => "#f7768e",
    Signed => "#f7768e",
    While => "#f7768e",
    Const => "#f7768e",
    Goto => "#f7768e",
    Sizeof => "#f7768e",
    _Bool => "#f7768e",
    Continue => "#f7768e",
    If => "#f7768e",
    Static => "#f7768e",
    _Complex => "#f7768e",
    Default => "#f7768e",
    Inline => "#f7768e",
    Struct => "#f7768e",
    _Imaginary => "#f7768e",
    Do => "#f7768e",
    Int => "#f7768e",
    Switch => "#f7768e",
    Double => "#f7768e",
    Long => "#f7768e",
    Typedef => "#f7768e",
    Else => "#f7768e",
    Register => "#f7768e",
    Union => "#f7768e",
    Identifier => "#ffffff",
    OctIntLit => "#e0af68",
    DecIntLit => "#e0af68",
    HexIntLit => "#e0af68",
    IntLitSuffix => "#e0af68",
    DecFloatLit => "#e0af68",
    HexFloatLit => "#e0af68",
    FloatLitSuffix => "#e0af68",
    CharLit => "#ff9e64",
    StringLit => "#9ece6a",
    RParen => "#89ddff",
    LParen => "#89ddff",
    RSquare => "#89ddff",
    LSquare => "#89ddff",
    RBrace => "#89ddff",
    LBrace => "#89ddff",
    RShift => "#89ddff",
    LShift => "#89ddff",
    RAngle => "#89ddff",
    LAngle => "#89ddff",
    Dot => "#89ddff",
    Arrow => "#89ddff",
    Incr => "#89ddff",
    Decr => "#89ddff",
    Amp => "#89ddff",
    Star => "#89ddff",
    Plus => "#89ddff",
    Minus => "#89ddff",
    Tilde => "#89ddff",
    Bang => "#89ddff",
    Div => "#89ddff",
    Mod => "#89ddff",
    Leq => "#89ddff",
    Geq => "#89ddff",
    Eq => "#89ddff",
    Neq => "#89ddff",
    Xor => "#89ddff",
    Or => "#89ddff",
    LAnd => "#89ddff",
    LOr => "#89ddff",
    Colon => "#89ddff",
    Comma => "#89ddff",
    Hash => "#89ddff",
    Ellipses => "#89ddff",
    Question => "#89ddff",
    Terminate => "#89ddff",
    Assign => "#89ddff",
    MulAssign => "#89ddff",
    DivAssign => "#89ddff",
    ModAssign => "#89ddff",
    AddAssign => "#89ddff",
    SubAssign => "#89ddff",
    OrAssign => "#89ddff",
    AndAssign => "#89ddff",
    XorAssign => "#89ddff",
    RShiftAssign => "#89ddff",
    LShiftAssign => "#89ddff",
    AnyToken => "#7dcfff",
    EOF => "#000000",
);

const RED: &str = "#ff000d";

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/ping", get(ping))
        .route("/cex", get(page))
        .route("/cexit", post(highlight));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:25565")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap()
}

async fn page() -> Html<&'static str> {
    Html(include_str!("../index.html"))
}

async fn highlight(body: String) -> Html<String> {
    let lex = Lexer::new(body.as_bytes());
    let mut html = String::new();
    let mut old_end = 0;
    for token in lex {
        match token {
            Ok(Token { ty, start, len }) => {
                if old_end != start {
                    html.push_str(&body[old_end..start]);
                }
                html.push_str(
                    format!(
                        r#"<span style="color: {}">{}</span>"#,
                        TOKCOLOR[ty as usize],
                        &body[start..][..len]
                    )
                    .as_str(),
                );
                old_end = start + len;
            }
            Err(LexError::UnknownToken(pos)) => {
                html.push_str(
                    format!(
                        r#"<span style="color: {}">{}</span>"#,
                        RED,
                        body.as_bytes()[pos]
                    )
                    .as_str(),
                );
            }
            Err(err) => {
                html.push_str(format!(r#"<span style="color: {}">{}</span>"#, RED, err).as_str());
            }
        }
    }

    sleep_until(Instant::now() + Duration::from_millis(10)).await;

    Html(html)
}

async fn ping() -> &'static str {
    "pong"
}
