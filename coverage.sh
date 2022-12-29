#!/bin/bash -e

cargo install grcov

RUSTFLAGS="-C instrument-coverage" cargo test
find . -iname "*.profraw" | xargs grcov -o coverage_report -t html -s src -b .
