use std::env;
use std::io::prelude::*;
use std::fs::File;

use failure::err_msg;

use compile::Compiler;

extern crate parser;
extern crate failure;
extern crate bit_vec;

#[macro_use]
mod chunk;
mod debug;
mod compile;
mod vm;
mod gc;

fn main() {
    let mut args = env::args();
    let _app = args.next();

    if let Some(arg) = args.next() {
        let res = match &arg[..] {
            "help" => help(args),
            "debug" => debug(args),
            sourcefile => execute(sourcefile),
        };
        if let Err(err) = res {
            eprintln!("[error]: {}", err);
            ::std::process::exit(2);
        }
    } else {
        println!("Usage: rlox [script]");
        ::std::process::exit(1);
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

fn help(_args: env::Args) -> Result<(), failure::Error> {
    println!("Usage: rlox [script]");
    println!("       rlox help  - Show help like this.");
    println!("       rlox debug - Show the compiled bytecode for a script, without executing.");
    Ok(())
}

fn debug(mut args: env::Args) -> Result<(), failure::Error> {
    let filename = match args.next() {
        Some(filename) => filename,
        None => return Err(err_msg("missing file")),
    };
    let chunk = compile(&filename)?;
    let disassembler = debug::Disassembler::new(&chunk);
    disassembler.disassemble();
    Ok(())
}

fn execute(filename: &str) -> Result<(), failure::Error> {
    let chunk = compile(&filename)?;
    vm::VM::new(chunk).run();
    Ok(())
}

fn compile(filename: &str) -> Result<chunk::Chunk, failure::Error> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut stmts = report_and_bail!(parser::parse(&contents));
    report_and_bail!(parser::resolve(&mut stmts));
    Ok(Compiler::new().compile(filename, &stmts))
}

fn show_errors<E: failure::Fail>(errors: Vec<E>) -> ! {
    for err in errors {
        eprintln!("[error]: Parse: {}", err);
    }
    ::std::process::exit(1);
}
