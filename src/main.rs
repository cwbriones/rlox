// error_chain! can recurse deeply
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;
extern crate rustyline;
extern crate env_logger;
#[macro_use]
extern crate log;

use std::env;
use std::io::prelude::*;
use std::fs::File;

use eval::Eval;
use eval::StandardContext;
use errors::Result;
use parser::Parser;
use repl::Repl;

mod errors;
mod environment;
mod parser;
mod eval;
mod value;
mod repl;

#[cfg(test)]
mod tests;

fn main() {
    env_logger::init().expect("Failed to initialize logger");

    let mut args = env::args();
    let _ = args.next();

    if let Some(ref arg) = args.next() {
        match &arg[..] {
            "help" => {
                println!("Usage: rlox [script]");
                ::std::process::exit(0);
            },
            sourcefile => {
                if let Err(err) = run_file(sourcefile) {
                    eprintln!("[error]: {}", err);
                    ::std::process::exit(1);
                }
                ::std::process::exit(0);
            }
        }
    }
    Repl::new().run();
}

fn run_file(filename: &str) -> Result<()> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut context = StandardContext::new();
    let mut parser = Parser::new(&contents);
    parser.parse().and_then(|stmts| {
        stmts.as_slice().eval(&mut context)
    })?;
    Ok(())
}
