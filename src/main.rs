// error_chain! can recurse deeply
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;
extern crate rustyline;

use std::env;
use std::io::stdin;
use std::io::prelude::*;
use std::fs::File;
use std::error::Error;

use rustyline::error::ReadlineError;

use eval::Eval;
use eval::Context;
use value::Value;
use errors::Result;

mod errors;
mod parser;
mod scanner;
mod multipeek;
mod eval;
mod value;

fn main() {
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
                    println!("Error: {}", err);
                    ::std::process::exit(1);
                }
                ::std::process::exit(0);
            }
        }
    }
    repl();
}

fn repl() {
    let mut rl = rustyline::Editor::<()>::new();
    let mut context = Context::new();

    println!("Welcome to lox! Use Ctrl-C to exit.");
    loop {
        let readline = rl.readline("rlox> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match line.eval(&mut context) {
                    Ok(Value::Void) => {},
                    Ok(lit) => println!("{}", lit),
                    Err(err) => println!("[error]: {}", err.description()),
                }
            },
            Err(ReadlineError::Interrupted) => {
                break
            },
            Err(ReadlineError::Eof) => {
                break
            },
            Err(err) => {
                println!("[error]: {:?}", err);
                break
            }
        }
    }
    println!("Goodbye!");
}

fn run_file(filename: &str) -> Result<()> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut context = Context::new();

    contents.eval(&mut context)?;
    Ok(())
}
