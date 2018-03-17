rlox
====

Following along with the book [Crafting Interpreters](http://www.craftinginterpreters.com/), in Rust.

An overview of the language can be found [here](http://www.craftinginterpreters.com/the-lox-language.html).

In general, I try to follow the book as close as possible conceptually. That said, this is also an exercise in idiomatic Rust and so things can differ greatly from the Java implementation. Namely sum types and pattern matching see heavy use, with a few macro invocations sprinked in as well.

# VM

A version of the clox VM (using the mostly same bytecode) can be found in the `vm` branch.

It's still a work in progress, seeming more like a straight C to rust port in some places. This will see another pass of optimizations and "rustification" at some later point. The implementation of UpValues in particular is a mess, since Rust's borrow checker is not aware of the lifetime of stack variables.

# License

This code is available under the [MIT License](http://github.com/cwbriones/rlox/tree/master/LICENSE).
