extern crate rustyline;
extern crate env_logger;
#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;

extern crate parser;

use std::env;
use std::io::prelude::*;
use std::fs::File;

use eval::Interpreter;
use parser::Parser;
use repl::Repl;
use pretty_printer::PrettyPrinter;

mod environment;
mod eval;
mod value;
mod repl;
mod pretty_printer;
mod resolver;

fn main() {
    env_logger::init().expect("Failed to initialize logger");

    let mut args = env::args();
    let _ = args.next();

    if let Some(arg) = args.next() {
        let res = match &arg[..] {
            "help" => {
                println!("Usage: rlox [script]");
                ::std::process::exit(0);
            },
            "print" => {
                let arg = args.next();
                if arg.is_none() {
                    eprintln!("[error]: Missing argument to print");
                    ::std::process::exit(1);
                }
                pretty_print(&arg.unwrap())
            },
            sourcefile => execute(sourcefile),
        };
        if let Err(err) = res {
            eprintln!("[error]: {}", err);
            ::std::process::exit(2);
        }
    } else {
        Repl::new().run();
    }
}

fn pretty_print(filename: &str) -> Result<(), failure::Error> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut parser = Parser::new(&contents);
    let stmts = match parser.parse() {
        Ok(stmts) => stmts,
        Err(errors) => {
            for err in errors {
                eprintln!("[error]: Parse: {}", err);
            }
            ::std::process::exit(1);
        }
    };
    let output = PrettyPrinter::new().pretty_print(&stmts);
    println!("{}", output);
    Ok(())
}

fn execute(filename: &str) -> Result<(), failure::Error> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut parser = Parser::new(&contents);
    match parser.parse() {
        Ok(mut stmts) => {
            let mut resolver = resolver::Resolver::new();
            debug!("--------------- Resolution ----------------");
            resolver.resolve(&mut stmts)?;
            debug!("--------------- Execution  ----------------");
            Interpreter::new().interpret(&stmts[..])?;
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
