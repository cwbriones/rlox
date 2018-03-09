use std::fs::{self, File};
use std::process::Command;
use std::io::prelude::*;
use std::io::BufReader;

#[cfg(debug_assertions)]
const BINARY: &str = "target/debug/rlox";

#[cfg(not(debug_assertions))]
const BINARY: &str = "target/release/rlox";

const EXPECT: &str = "expect: ";
const EXPECT_ERR: &str = "expect runtime error: ";
const ERR_LOG: &str = "[error]: ";

#[test]
fn foo() {
    panic!("This should fail");
}

fn execute_test(filename: &str) {
    fs::metadata(BINARY).expect("Could not locate binary");

    let file =
        File::open(&filename).expect("Could not open testcase file");

    let file = BufReader::new(file);

    // Create an iterator over "expect" comments
    let expects =
        file.lines()
            .map(|l| l.expect("should be valid UTF-8"))
            .filter_map(|mut line| {
                line.find("// expect").map(|idx| line.split_off(idx + 3))
            });

    let mut expected_out = String::new();
    let mut expected_err = String::new();
    for expect in expects {
        if expect.starts_with(EXPECT) {
            let o = &expect[EXPECT.len()..].trim();
            expected_out.push_str(o);
            expected_out.push('\n');
        } else if expect.starts_with(EXPECT_ERR) {
            let o = &expect[EXPECT_ERR.len()..].trim();
            expected_err.push_str(ERR_LOG);
            expected_err.push_str(o);
            expected_err.push('\n');
        }
    }
    // Fix newlines.
    expected_out = expected_out.replace("\\n", "\n");
    expected_err = expected_err.replace("\\n", "\n");

    let output =
        Command::new(BINARY)
            .args(&[filename])
            .output()
            .expect("Failed to execute process");

    if output.status.success() {
       let output = String::from_utf8_lossy(&output.stdout);
       assert_eq!(expected_out, output);
    } else {
       let output = String::from_utf8_lossy(&output.stderr);
       assert_eq!(expected_err, output);
    };
}

macro_rules! define_test (
    ($name:ident) => (
        #[test]
        fn $name() {
            let filename = concat!("tests/lox/", stringify!($name), ".lox");
            execute_test(filename);
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
define_test!(loop_break);
define_test!(functions);
define_test!(resolving);
define_test!(equality);
define_test!(native);
define_test!(class);
define_test!(inheritance);
define_test!(lambda);
define_test!(list);

define_test!(divide_by_zero);
