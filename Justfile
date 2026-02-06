# Works on Windows (PowerShell). Requires:
# - rustup target add wasm32-unknown-unknown
# - trunk (cargo install trunk)

set shell := ["pwsh", "-NoLogo", "-NoProfile", "-Command"]

# Default action: show available commands
default: help

help:
    @just --list

# Start dev server (http://127.0.0.1:8080)
dev:
    trunk serve --open

# Clean build artifacts
clean:
    cargo clean

# Build a release bundle into dist/
build:
    trunk build --release

# Local Pages preview (serves dist/)
serve-dist:
    trunk serve --dist dist --open
