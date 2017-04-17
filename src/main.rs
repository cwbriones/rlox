// error_chain! can recurse deeply
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;

use std::env;
use std::error::Error;
use std::io::prelude::*;
use std::fs::File;

use scanner::Scanner;
use scanner::Token;
use scanner::TokenType;

use parser::Parser;
use eval::Eval;
use eval::Context;

mod errors;
mod parser;
mod scanner;
mod multipeek;
mod eval;

fn main() {
    let mut args = env::args();
    let _ = args.next();
    if let Some(ref sourcefile) = args.next() {
        if let Err(err) = run_file(sourcefile) {
            println!("Error: {}", err);
        }
    } else {
        println!("Usage: rlox [script]");
    }
}

fn run_file(filename: &str) -> Result<(), Box<Error>> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let scanner = Scanner::new(&contents);
    let tokens = scanner.filter(|t| {
        if let &Ok(Token{ty: TokenType::Comment, ..}) = t {
            false
        } else {
            true
        }
    }).collect::<Result<Vec<Token>, _>>()?;

    let mut parser = Parser::new(&tokens);
    let mut context = Context::new();
    let expr = parser.expression()?;
    let evald = expr.eval(&mut context)?;

    println!("{:?}", expr);
    println!("{:?}", evald);

    Ok(())
}
