use std::fmt::Write;

use eval::Eval;
use eval::Context;
use environment::Environment;
use value::Value;

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
            prog.eval(&mut context).unwrap();

            let out = include_str!(concat!("examples/", stringify!($name), ".lox.out"));
            assert_eq!(out, context.captured());
        }
    );
);

define_test!(simple_env);
define_test!(arithmetic);
