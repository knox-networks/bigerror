# Justfiles are processed by the just command runner (https://just.systems/).
# You can install it with `brew install just` or `cargo install just`
_default:
    just --list

# Run rustfmt
fmt:
    rustup run nightly cargo fmt -- \
      --config-path ./fmt/rustfmt.toml

# Run clippy fix and rustfmt afterwards
fix *args: && fmt
  cd {{invocation_directory()}}; cargo clippy --fix --all-targets --all-features {{args}}


# Prints the error stack for a given test to stdout
printerr test $PRINTERR="true":
  @cargo test --quiet --lib -- --exact test::{{test}} --nocapture

printerr-all $PRINTERR="true" $RUST_TEST_THREADS="1":
  @cargo test --lib -- --exact --nocapture
