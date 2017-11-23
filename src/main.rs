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
use std::error::Error;

use rustyline::error::ReadlineError;

use parser::Parser;
use eval::Eval;
use eval::Context;
use value::Value;
use errors::Result;

mod errors;
mod environment;
mod parser;
mod eval;
mod value;

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
                match eval(&line, &mut context) {
                    Ok(Value::Void) => {},
                    Ok(lit) => println!("{}", lit),
                    Err(err) => eprintln!("[error]: {}", err.description()),
                }
            },
            Err(ReadlineError::Interrupted) => {
                break
            },
            Err(ReadlineError::Eof) => {
                break
            },
            Err(err) => {
                error!("{:?}", err);
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
    eval(&contents, &mut context)?;
    Ok(())
}

fn eval(program: &str, context: &mut Context) -> Result<Value> {
    let mut parser = Parser::new(program);
    let statements = parser.parse()?;
    for stmt in statements {
        stmt.eval(context)?;
    }
    Ok(Value::Void)
}
