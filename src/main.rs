use std::{error::Error, io::Read};

use klang::Lexer;

fn main() -> Result<(), Box<dyn Error>> {
    let mut buf = Vec::new();
    std::io::stdin().read_to_end(&mut buf)?;
    let lex = Lexer::new(&buf);
    for token in lex {
        println!("{:?}", token);
    }
    Ok(())
}
