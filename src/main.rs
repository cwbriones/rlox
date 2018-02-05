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
use repl::Repl;
use pretty_printer::PrettyPrinter;

mod environment;
mod eval;
mod value;
mod repl;
mod pretty_printer;

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

macro_rules! report_and_bail (
    ($expr:expr) => (
        match $expr {
            Ok(ok) => ok,
            Err(errors) => show_errors(errors),
        }
    );
);

fn show_errors<E: failure::Fail>(errors: Vec<E>) -> ! {
    for err in errors {
        eprintln!("[error]: Parse: {}", err);
    }
    ::std::process::exit(1);
}

fn pretty_print(filename: &str) -> Result<(), failure::Error> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let stmts = report_and_bail!(parser::parse(&contents));
    let output = PrettyPrinter::new().pretty_print(&stmts);
    println!("{}", output);
    Ok(())
}

fn execute(filename: &str) -> Result<(), failure::Error> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut stmts = report_and_bail!(parser::parse(&contents));
    report_and_bail!(parser::resolve(&mut stmts));
    Interpreter::new().interpret(&stmts[..])?;
    Ok(())
}
