use std::fmt::Write;

use eval::Eval;
use eval::Context;
use environment::Environment;
use value::Value;
use parser::Parser;

pub struct CapturingContext {
    environment: Environment,
    buf: String,
}

impl CapturingContext {
    pub fn new() -> Self {
        CapturingContext {
            environment: Environment::new(),
            buf: String::new(),
        }
    }

    pub fn captured(&self) -> &str {
        &self.buf
    }
}

impl Context for CapturingContext {
    fn env(&self) -> &Environment {
        &self.environment
    }

    fn env_mut(&mut self) -> &mut Environment {
        &mut self.environment
    }

    fn push_env(&mut self) {
        self.environment = self.environment.extend();
    }

    fn pop_env(&mut self) {
        if let Some(p) = self.environment.parent() {
            self.environment = p;
        }
    }

    fn println(&mut self, v: &Value) {
        writeln!(&mut self.buf, "{}", v).expect("Failed to write to buffer");
    }
}

macro_rules! define_test (
    ($name:ident) => (
        #[test]
        fn $name() {
            let mut context = CapturingContext::new();
            let prog = include_str!(concat!("examples/", stringify!($name), ".lox"));
            let mut parser = Parser::new(prog);
            parser.parse().and_then(|prog| prog.as_slice().eval(&mut context)).unwrap();

            let out = include_str!(concat!("examples/", stringify!($name), ".lox.out"));
            assert_eq!(out, context.captured());
        }
    );
);

define_test!(arithmetic);
define_test!(variables_and_scope);
define_test!(if_statement);
define_test!(while_loop);
define_test!(for_loop);
define_test!(fibonacci);
define_test!(logical_operators);
