# Justfiles are processed by the just command runner (https://just.systems/).
# You can install it with `brew install just` or `cargo install just`
_default:
    just --list

# Run rustfmt
fmt:
    rustup run nightly cargo fmt -- \
      --config-path ./fmt/rustfmt.toml

# Prints the error stack for a given test to stdout
printerr test $PRINTERR="true":
  @cargo test --quiet --lib -- --exact {{test}} --nocapture

printerr-all $PRINTERR="true":
  @cargo test --quiet --lib -- --exact --nocapture
