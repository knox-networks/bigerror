# Justfiles are processed by the just command runner (https://just.systems/).
# You can install it with `brew install just` or `cargo install just`
_default:
    just --list

# Run rustfmt
fmt:
    rustup run nightly cargo fmt -- \
      --config-path ./fmt/rustfmt.toml

[no-cd, group("lint")]
lint *args:
    cargo clippy --all-targets --all-features --no-deps {{args}} -- -D warnings

# Run clippy fix and rustfmt afterwards
[no-cd]
fix *args: && fmt
  cargo clippy --fix --all-targets --all-features {{args}}


# Prints the error stack for a given test to stdout
printerr test $PRINTERR="true":
  @cargo test --quiet --lib -- --exact test::{{test}} --nocapture

printerr-all $PRINTERR="true" $RUST_TEST_THREADS="1":
  @cargo test --lib -- --exact --nocapture
