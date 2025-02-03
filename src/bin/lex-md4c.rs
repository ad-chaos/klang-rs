use std::{error::Error, io::Read, time::Instant};

use klang::Lexer;

type AppResult = Result<(), Box<dyn Error>>;

fn inflates_to(size: usize, repeats: usize) {
    let size = size << repeats;
    println!("File size: {:.2} MB", (size as f64 / (1024. * 1024.)));
}

fn main() -> AppResult {
    let filename = std::env::args().nth(1).ok_or("Not enough arguments")?;
    let mut file = std::fs::File::open(filename)?;
    let mut source = Vec::new();
    file.read_to_end(&mut source)?;
    let orig_size = source.len();


    const N: usize = 5;
    for _ in 0..N {
        source.extend_from_within(..);
    }
    inflates_to(orig_size, N);

    let start = Instant::now();

    for _ in 0..5 {
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
            "{} tokens, {:.2} tokens/μs",
            ntokens,
            ntokens as f64 / start.elapsed().as_micros() as f64,
        );
    }

    println!("Total time: {}ms", start.elapsed().as_millis());

    Ok(())
}
