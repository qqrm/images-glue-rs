# AGENTS Instructions

## Required validation before commit/PR
Run all of the following and fix any issues before committing:

1. `cargo fmt --all`
2. `cargo check`
3. `cargo build`
4. `cargo clippy --all-targets --all-features -- -D warnings`

If any command fails, resolve the problem and rerun until all pass.
