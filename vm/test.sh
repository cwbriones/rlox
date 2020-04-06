#!/bin/bash

# Wrapper around cargo test to avoid testing features that are not
# implemented in the VM.

cargo test --\
    --skip inheritance\
    --skip limit\
    --skip super_keyword\
    --skip call::object\
    --skip if_statement::class_in_else\
    --skip if_statement::class_in_then\
    $@
