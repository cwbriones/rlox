use std::fs::{self, File};
use std::process::Command;
use std::io::prelude::*;
use std::io::BufReader;
use std::env;

const EXPECT: &str = "expect: ";
const EXPECT_ERR: &str = "expect runtime error: ";
const EXPECT_PARSE_ERR: &str = "Error at";
const LINE_PARSE_ERR: &str = "[line";

const ERR_LOG: &str = "[error]: ";

#[macro_export]
macro_rules! define_integration_tests (
    () => (
        pub use $crate::execute_test;

        define_test_mod!(assignment,
            associativity,
            global,
            grouping,
            infix_operator,
            local,
            prefix_operator,
            syntax,
            to_this,
            undefined
        );

        define_test_mod!(block, empty, scope);

        define_test_mod!(bool, equality, not);

        define_test_mod!(call, bool, nil, num, object, string);

        define_test_mod!(class,
            empty, inherited_method, local_reference_self, reference_self);

        define_test_mod!(closure,
            assign_to_closure,
            assign_to_shadowed_later,
            close_over_function_parameter,
            close_over_later_variable,
            close_over_method_parameter,
            closed_closure_in_function,
            nested_closure,
            open_closure_in_function,
            reference_closure_multiple_times,
            reuse_closure_slot,
            shadow_closure_with_local,
            unused_closure,
            unused_later_closure);

        define_test_mod!(comments,
            line_at_eof,
            only_line_comment,
            only_line_comment_and_line,
            unicode);

        define_test_mod!(constructor,
            arguments,
            call_init_explicitly,
            default,
            default_arguments,
            early_return,
            extra_arguments,
            init_not_method,
            missing_arguments,
            return_in_nested_function,
            return_value);

        // TODO: These are only used in the earlier parsing chapters,
        // and should be moved to equivalent parsing unit tests.
        //
        // define_test_mod!(expressions,
        //     evaluate,
        //     parse);

        define_test_mod!(field,
            call_function_field,
            call_nonfunction_field,
            get_and_set_method,
            get_on_bool,
            get_on_class,
            get_on_function,
            get_on_nil,
            get_on_num,
            get_on_string,
            many,
            method,
            method_binds_this,
            on_instance,
            set_evaluation_order,
            set_on_bool,
            set_on_class,
            set_on_function,
            set_on_nil,
            set_on_num,
            set_on_string,
            undefined);

        define_test_mod!(for_statement,
            class_in_body,
            closure_in_body,
            fun_in_body,
            return_closure,
            return_inside,
            scope,
            statement_condition,
            statement_increment,
            statement_initializer,
            syntax,
            var_in_body);

        define_test_mod!(function,
            body_must_be_block,
            empty_body,
            extra_arguments,
            local_mutual_recursion,
            local_recursion,
            missing_arguments,
            missing_comma_in_parameters,
            mutual_recursion,
            parameters,
            print,
            recursion,
            too_many_arguments,
            too_many_parameters);

        define_test_mod!(if_statement,
            class_in_else,
            class_in_then,
            dangling_else,
            else_clause,
            fun_in_else,
            fun_in_then,
            if_statement,
            truth,
            var_in_else,
            var_in_then);

        define_test_mod!(inheritance,
            inherit_from_function,
            inherit_from_nil,
            inherit_from_number,
            inherit_methods,
            parenthesized_superclass,
            set_fields_from_base_class);

        define_test_mod!(limit,
            loop_too_large,
            reuse_constants,
            stack_overflow,
            too_many_constants,
            too_many_locals,
            too_many_upvalues);

        define_test_mod!(logical_operator,
            and,
            and_truth,
            or,
            or_truth);

        define_test_mod!(method,
            arity,
            empty_block,
            extra_arguments,
            missing_arguments,
            not_found,
            refer_to_name,
            too_many_arguments,
            too_many_parameters);

        define_test_mod!(nil, literal);

        define_test_mod!(number,
            decimal_point_at_eof,
            leading_dot,
            literals,
            trailing_dot);

        define_test_mod!(operator,
            add,
            add_bool_nil,
            add_bool_num,
            add_bool_string,
            add_nil_nil,
            add_num_nil,
            add_string_nil,
            comparison,
            divide,
            divide_nonnum_num,
            divide_num_nonnum,
            equals,
            equals_class,
            equals_method,
            greater_nonnum_num,
            greater_num_nonnum,
            greater_or_equal_nonnum_num,
            greater_or_equal_num_nonnum,
            less_nonnum_num,
            less_num_nonnum,
            less_or_equal_nonnum_num,
            less_or_equal_num_nonnum,
            multiply,
            multiply_nonnum_num,
            multiply_num_nonnum,
            negate,
            negate_nonnum,
            not,
            not_class,
            not_equals,
            subtract,
            subtract_nonnum_num,
            subtract_num_nonnum);

        define_test_mod!(print, missing_argument);

        define_test_mod!(regression, regression);

        define_test_mod!(return_statement,
            after_else,
            after_if,
            after_while,
            at_top_level,
            in_function,
            in_method,
            return_nil_if_no_value);

        // TODO: These are only used in the earlier parsing chapters,
        // and should be moved to equivalent parsing unit tests.
        //
        // define_test_mod!(scanning,
        //     identifiers,
        //     keywords,
        //     numbers,
        //     punctuators,
        //     strings,
        //     whitespace);

        define_test_mod!(string,
            error_after_multiline,
            literals,
            multiline,
            unterminated);

        define_test_mod!(super_keyword,
            bound_method,
            call_other_method,
            call_same_method,
            closure,
            constructor,
            extra_arguments,
            indirectly_inherited,
            missing_arguments,
            no_superclass_bind,
            no_superclass_call,
            no_superclass_method,
            parenthesized,
            reassign_superclass,
            super_at_top_level,
            super_in_closure_in_inherited_method,
            super_in_inherited_method,
            super_in_top_level_function,
            super_without_dot,
            super_without_name,
            this_in_superclass_method);

        define_test_mod!(this,
            closure,
            nested_class,
            nested_closure,
            this_at_top_level,
            this_in_method,
            this_in_top_level_function);

        define_test_mod!(variable,
            collide_with_parameter,
            duplicate_local,
            duplicate_parameter,
            early_bound,
            in_middle_of_block,
            in_nested_block,
            local_from_method,
            redeclare_global,
            redefine_global,
            scope_reuse_in_different_blocks,
            shadow_and_local,
            shadow_global,
            shadow_local,
            undefined_global,
            undefined_local,
            uninitialized,
            unreached_undefined,
            use_false_as_var,
            use_global_in_initializer,
            use_local_in_initializer,
            use_nil_as_var,
            use_this_as_var);

        define_test_mod!(while_statement,
            class_in_body,
            closure_in_body,
            fun_in_body,
            return_closure,
            return_inside,
            syntax,
            var_in_body);
    );
);

#[macro_export]
macro_rules! define_test_mod (
    ($mod:ident, $($testcase:ident),+) => (
        mod $mod {
            use crate::BINARY;
            use super::execute_test;

            const PATH: &str = concat!("integration/lox-tests/", stringify!($mod));
            define_tests!($($testcase),+);
        }
    );
);

#[macro_export]
macro_rules! define_tests (
    ($testcase:ident) => (
        #[test]
        fn $testcase() {
            let filename = concat!(stringify!($testcase), ".lox");
            execute_test(BINARY, PATH, filename);
        }
    );
    ($testcase:ident, $($rest:ident),*) => (
        #[test]
        fn $testcase() {
            let filename = concat!(stringify!($testcase), ".lox");
            execute_test(BINARY, PATH, filename);
        }
        define_tests!($($rest),*);
    );
);

pub fn execute_test(binary: &str, mod_path: &str, filename: &str) {
    fs::metadata(binary).expect("Could not locate binary");

    let mut path = env::current_dir().unwrap();
    path.pop();
    path.push(mod_path);
    path.push(filename);
    println!("{:?}", path);

    let file = File::open(&path).expect("Could not open testcase file {}");

    let file = BufReader::new(file);

    // Create an iterator over "expect" comments
    let expects =
        file.lines()
            .map(|l| l.expect("should be valid UTF-8"))
            .filter_map(line_filter);

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
        } else if expect.starts_with(EXPECT_PARSE_ERR) || expect.starts_with(LINE_PARSE_ERR) {
            let i = expect.find(": ").unwrap();
            // FIXME: This transforms the expected parse error into the format rlox reports,
            // consider making this the same as clox.
            expected_err.push_str(ERR_LOG);
            expected_err.push_str("Parse: ");
            expected_err.push_str(&expect[i + 2..]);
            expected_err.push('\n');
        }
    }
    // Fix newlines.
    expected_out = expected_out.replace("\\n", "\n");
    expected_err = expected_err.replace("\\n", "\n");

    let output =
        Command::new(binary)
            .args(&[path])
            .output()
            .expect("Failed to execute process");

    if output.status.success() {
       let output = String::from_utf8_lossy(&output.stdout);
       assert_eq!(expected_out, output);
    } else {
       let output = String::from_utf8_lossy(&output.stderr);
       // TODO: Make this more intelligent with backtraces.
       // We check the prefix because the testcases do not include trace information.
       assert!(output.starts_with(&expected_err), r"
===============================================================================
Expected Output

{}
===============================================================================
Got Output

{}
===============================================================================
", expected_err.trim(), output.trim());
    };
}

fn line_filter(mut line: String) -> Option<String> {
    line.find("// expect")
        .or_else(|| line.find("// Error at"))
        .or_else(|| line.find("// [line"))
        .map(|idx| line.split_off(idx + 3)) // remove the comment
}
