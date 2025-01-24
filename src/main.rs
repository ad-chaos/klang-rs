use klang::Lexer;
use std::{error::Error, fs::File, io::BufReader};

use clap::Parser;

type AppResult = Result<(), Box<dyn Error>>;

#[derive(Parser, Debug)]
#[command(version, about, long_about = "something")]
struct Args {
    #[arg(short, long)]
    filename: String,
}

fn main() -> AppResult {
    let args = Args::parse();
    let mut lexer = Lexer::new(BufReader::new(File::open(args.filename)?));

    println!("{:?}", lexer.next_token());

    Ok(())
}
