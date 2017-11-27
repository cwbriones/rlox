use rustyline;
use rustyline::error::ReadlineError;

use eval::{Eval, StandardContext};
use failure::Error;
use value::Value;
use parser::Parser;

const PROMPT: &str = "rlox> ";
const BLOCK_PROMPT: &str = "    > ";

pub struct Repl {
    editor: rustyline::Editor<()>, context: StandardContext,
}

impl Repl {
    pub fn new() -> Self {
        let editor = rustyline::Editor::<()>::new();
        let context = StandardContext::new();
        Repl {
            editor,
            context
        }
    }

    pub fn run(mut self) {
        println!("Welcome to lox! Use Ctrl-C to exit.");
        loop {
            match self.read() {
                Ok(line) => {
                    self.editor.add_history_entry(&line);
                    let res = self.eval(&line);
                    self.print(res);
                },
                Err(ref err) if self.should_quit(err) => break,
                Err(err) => eprintln!("[error]: {}", err),
            }
        }
        println!("Goodbye!");
    }

    fn should_quit(&self, err: &Error) -> bool {
        err.downcast_ref::<ReadlineError>().map(|err| {
            match *err {
                ReadlineError::Interrupted | ReadlineError::Eof => true,
                _ => false,
            }
        }).unwrap_or(false)
    }

    fn read(&mut self) -> Result<String, Error> {
        let mut line = self.editor.readline(PROMPT)?;
        if line.trim_right().ends_with('{') {
            self.read_block(&mut line)?;
        }
        Ok(line)
    }

    fn read_block(&mut self, line: &mut String) -> Result<(), Error> {
        let mut next = self.editor.readline(BLOCK_PROMPT)?;
        while next.trim() != "}" {
            line.push_str("\n      ");
            line.push_str(&next);
            next = self.editor.readline(BLOCK_PROMPT)?;
        }
        line.push_str("\n      ");
        line.push_str(&next);
        Ok(())
    }

    fn eval(&mut self, line: &str) -> Result<Value, Error> {
        let mut parser = Parser::new(line);
        let ctx = &mut self.context;
        if line.ends_with('}') || line.ends_with(';') {
            let stmt = parser.parse_statement()?;
            stmt.eval(ctx).map_err(Into::into)
        } else {
            let expr = parser.expression()?;
            expr.eval(ctx).map_err(Into::into)
        }
    }

    fn print(&self, res: Result<Value, Error>) {
        match res {
            Ok(Value::Void) => {},
            Ok(lit) => println!("{}", lit),
            Err(err) => eprintln!("[error]: {}", err),
        }
    }
}
