use std::error::Error;

use klang::Lexer;

type AppResult = Result<(), Box<dyn Error>>;

fn main() -> AppResult {
    let source = std::fs::read("unity-md4c.c")?;
    let mut lex = Lexer::new(&source);

    println!("{}", String::from_utf8(source[120429-100..][..200].to_vec())?);

    let mut ntokens = 0;
    loop {
        let token = lex.next_token()?;
        if token.is_eof() {
            break;
        }
        ntokens += 1;
    }

    println!("{} tokens lexed", ntokens);

    Ok(())
}
