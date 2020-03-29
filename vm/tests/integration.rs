#[macro_use]
extern crate integration;

#[cfg(debug_assertions)]
const BINARY: &str = "../target/debug/vm";

#[cfg(not(debug_assertions))]
const BINARY: &str = "../target/release/vm";

#[cfg(test)]
define_integration_tests!();
