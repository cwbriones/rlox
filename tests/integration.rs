use std::fs::{self, File};
use std::process::Command;
use std::io::Read;

#[cfg(debug_assertions)]
const BINARY: &str = "target/debug/rlox";

#[cfg(not(debug_assertions))]
const BINARY: &str = "target/release/rlox";

fn execute_test(filename: &str) {
    let output_file = filename.to_owned() + ".out";

    fs::metadata(BINARY).expect("Could not locate binary");
    fs::metadata(filename).expect("Could not locate testcase");

    let mut expected_output = String::new();

    File::open(&output_file)
        .expect("Could not locate testcase output")
        .read_to_string(&mut expected_output)
        .expect("Failed to read output file");

    let output =
        Command::new(BINARY)
            .args(&[filename])
            .output()
            .expect("Failed to execute process");
    let output = if output.status.success() {
       String::from_utf8_lossy(&output.stdout)
    } else {
       String::from_utf8_lossy(&output.stderr)
    };
    assert_eq!(output, expected_output);
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

define_test!(divide_by_zero);
