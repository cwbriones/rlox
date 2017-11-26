use rustyline;
use rustyline::error::ReadlineError;

use eval::{Eval, StandardContext};
use errors;
use value::Value;
use parser::Parser;

const PROMPT: &str = "rlox> ";
const BLOCK_PROMPT: &str = "    > ";

pub struct Repl {
    editor: rustyline::Editor<()>,
    context: StandardContext,
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

    fn read(&mut self) -> Result<String, ReadlineError> {
        let mut line = self.editor.readline(PROMPT)?;
        if line.trim_right().ends_with('{') {
            self.read_block(&mut line)?;
        }
        Ok(line)
    }

    fn read_block(&mut self, line: &mut String) -> Result<(), ReadlineError> {
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

    fn eval(&mut self, line: &str) -> errors::Result<Value> {
        let mut parser = Parser::new(line);
        let ctx = &mut self.context;
        if line.ends_with('}') || line.ends_with(';') {
            parser.parse_statement().and_then(|stmt| stmt.eval(ctx))
        } else {
            parser.expression().and_then(|expr| expr.eval(ctx))
        }
    }

    fn print(&self, res: errors::Result<Value>) {
        match res {
            Ok(Value::Void) => {},
            Ok(lit) => println!("{}", lit),
            Err(err) => eprintln!("[error]: {}", err),
        }
    }
}
