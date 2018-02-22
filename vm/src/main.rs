use std::env;
use std::io::prelude::*;
use std::fs::File;

use compile::Compiler;

extern crate parser;
extern crate failure;

#[macro_use]
mod chunk;
mod value;
mod debug;
mod compile;
mod vm;

fn main() {
    let mut args = env::args();
    let _ = args.next();

    if let Some(arg) = args.next() {
        let res = match &arg[..] {
            "help" => {
                println!("Usage: rlox [script]");
                ::std::process::exit(0);
            },
            sourcefile => execute(sourcefile, env::var("DEBUG").is_ok()),
        };
        if let Err(err) = res {
            eprintln!("[error]: {}", err);
            ::std::process::exit(2);
        }
    } else {
        println!("Usage: rlox [script]");
        ::std::process::exit(1);
    }

    // let mut chunk = Chunk::new("test chunk".into());
    // let idx = chunk.add_constant(Value::float(2.0));
    // chunk.write(Op::Constant(idx), 1);
    // let idx = chunk.add_constant(Value::float(3.0));
    // chunk.write(Op::Constant(idx), 1);
    // chunk.write(Op::Multiply, 1);
    // let idx = chunk.add_constant(Value::float(1.0));
    // chunk.write(Op::Constant(idx), 1);
    // chunk.write(Op::Add, 1);
    // chunk.write(Op::Print, 1);
}

macro_rules! report_and_bail (
    ($expr:expr) => (
        match $expr {
            Ok(ok) => ok,
            Err(errors) => show_errors(errors),
        }
    );
);

fn execute(filename: &str, debug: bool) -> Result<(), failure::Error> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut stmts = report_and_bail!(parser::parse(&contents));
    report_and_bail!(parser::resolve(&mut stmts));
    let chunk = Compiler::new().compile(filename, &stmts);

    if debug {
        let disassembler = debug::Disassembler::new(&chunk);
        disassembler.disassemble();
    }

    vm::VM::new(chunk).run();

    Ok(())
}

fn show_errors<E: failure::Fail>(errors: Vec<E>) -> ! {
    for err in errors {
        eprintln!("[error]: Parse: {}", err);
    }
    ::std::process::exit(1);
}
