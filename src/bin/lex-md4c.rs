use std::{error::Error, io::Read, time::Instant};

use klang::Lexer;

type AppResult = Result<(), Box<dyn Error>>;

fn main() -> AppResult {
    let filename = std::env::args().nth(1).ok_or("Not enough arguments")?;
    let mut file = std::fs::File::open(filename)?;
    let mut source = Vec::with_capacity(60 * 1024 * 1024);
    file.read_to_end(&mut source)?;

    let last = *source.last().expect("zero length file");

    let mut source: Vec<u8> = source
        .windows(2)
        .map(|b| if b == b"\\\n" { b"" } else { b })
        .filter(|b| !b.is_empty())
        .map(|b| b.first().unwrap())
        .copied()
        .collect();
    source.push(last);

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
    let end = start.elapsed().as_micros();

    println!("{} tokens/μs, {:.2}ms", ntokens / end, end as f64 / 1000.);

    Ok(())
}
