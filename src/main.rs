use std::{error::Error, time::Instant};

use klang::Lexer;

type AppResult = Result<(), Box<dyn Error>>;

fn inflate_to(buf: &[u8], mb: f64) {
    println!(
        "Repeat {} times",
        (buf.len() as f64 / (1024f64 * 1024f64 * mb)) as usize
    );
}

fn main() -> AppResult {
    let source = std::fs::read("unity-md4c.c")?;
    inflate_to(&source, 10f64);
    // for _ in 0..3 {
    //     source.extend_from_within(..);
    // }

    println!("{:.2} MB", source.len() as f64 / (1024. * 1024.));

    let start = Instant::now();
    let mut lex = Lexer::new(&source);
    let mut ntokens = 0;
    loop {
        let token = lex.next_token()?;
        if token.is_eof() {
            break;
        }
        ntokens += 1;
    }

    println!(
        "{} tokens lexed in {} ms",
        ntokens,
        start.elapsed().as_millis()
    );

    Ok(())
}
