extern crate rustyline;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate failure;
#[macro_use]
extern crate failure_derive;

use std::env;
use std::io::prelude::*;
use std::fs::File;

use eval::Eval;
use eval::StandardContext;
use parser::Parser;
use repl::Repl;

mod environment;
mod parser;
mod eval;
mod value;
mod repl;

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
            }
        }
    } else {
        Repl::new().run();
    }
}

fn run_file(filename: &str) -> Result<(), failure::Error> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut context = StandardContext::new();
    let mut parser = Parser::new(&contents);
    match parser.parse() {
        Ok(stmts) => {
            stmts.as_slice().eval(&mut context)?;
        },
        Err(errors) => {
            for err in errors {
                eprintln!("[error]: Parse: {}", err);
            }
            ::std::process::exit(1);
        }
    }
    Ok(())
}
