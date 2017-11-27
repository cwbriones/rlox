// Create the Error, ErrorKind, ResultExt, and Result types
error_chain! {
    // Automatic conversions from other types defined by error_chain!
    links {}

    // Automatic conversions from other types defined outside of error_chain!
    foreign_links {
        Io(::std::io::Error);
    }

    // Define additional `ErrorKind` variants. The syntax here is
    // the same as `quick_error!`, but the `from()` and `cause()`
    // syntax is not supported.
    errors {
        DivideByZero {
            description("Attempted to divide by zero")
            display("division by zero")
        }

        UnexpectedEOF {
            description("unexpected end of input")
            display("unexpected end of input")
        }

        UnexpectedChar(c: char) {
            description("unexpected character")
            display("unexpected character '{}'", c)
        }

        UnterminatedString {
            description("unterminated string")
            display("unterminated string, expected '\"'")
        }

        UndefinedVariable(s: String) {
            description("undefined variable")
            display("variable '{}' could not be resolved", s)
        }

        ParserError(errors: Vec<Error>) {
            description("there was an error while parsing")
        }
    }
}
