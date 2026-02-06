set shell := ["pwsh", "-NoLogo", "-NoProfile", "-Command"]

# Show available recipes (use: just --list)
default:
    just --list

bootstrap:
    rustup target add wasm32-unknown-unknown
    cargo install trunk --locked

dev:
    trunk serve --address 127.0.0.1 --port 8080

build:
    trunk build --release

fmt:
    cargo fmt --all

clippy:
    cargo clippy --all-targets --all-features -- -D warnings
