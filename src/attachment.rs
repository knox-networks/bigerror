use std::time::Duration;

use tracing::error;

pub use error_stack::{self, Context, Report, ResultExt};
pub use thiserror;

use crate::reportable;

pub trait Display: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {}

impl<A> Display for A where A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {}

pub trait Debug: std::fmt::Debug + Send + Sync + 'static {}

impl<A> Debug for A where A: std::fmt::Debug + Send + Sync + 'static {}

// simple key-value pair attachment
#[derive(Debug, thiserror::Error)]
#[error("{0}: {1}")]
pub struct KeyValue<K, V>(pub K, pub V);

impl KeyValue<String, String> {
    pub fn dbg(key: impl Debug, value: impl Debug) -> Self {
        Self(format!("{key:?}"), format!("{value:?}"))
    }
}

#[derive(Debug)]
// Field differs from [`KeyValue`] in that the id/key points to a preexisting [`Index`] or
// [`Property`]
pub struct Field<Id, S> {
    // the identifiable property of a data structure
    // such has  `hash_map["key"]` or a `struct.property`
    id: Id,
    status: S,
}
// TODO
// field!(some_struct.property) -> "some_struct.property"
// field!(%some_struct.property)

impl<Id: Display, S: Display> std::fmt::Display for Field<Id, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.id, self.status)
    }
}

impl<Id: Display, S: Display> Field<Id, S> {
    pub fn new(key: Id, status: S) -> Self {
        Self { id: key, status }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("must be {0}")]
pub struct MustBe<T>(T);

#[derive(Debug, thiserror::Error)]
pub struct OneOf<T: Display, const U: usize>([T; U], &'static str);

impl<T: Display, const U: usize> std::fmt::Display for OneOf<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // assume that the variants are in a report attachment list
        if self.0.is_empty() {
            return write!(f, "one of:");
        }
        let these: Vec<String> = self.0.iter().map(|t| format!("{t}")).collect();
        write!(
            f,
            "one of these: {}",
            these
                // https://gist.github.com/green-s/fbd0d374b290781ac9b3f8ff03e3245d
                .iter()
                .map(|s| &**s)
                .collect::<Vec<&str>>()
                .join(self.1)
        )
    }
}

/// MustBe(OneOf([Type::of::<String>(), Type::of::<usize>()]));
/// MustBe(OneOf([ty!(String), ty!(usize)]));
/// InvalidInput::expected_actual(, id)

#[macro_export]
macro_rules! oneof {
    ($($t:ty,)+,?) => {
        $crate::attachment::OneOf([$($crate::ty!($t),)*])
    };
    ($($t:ty)|+) => {
        $crate::attachment::OneOf([$($crate::ty!($t),)*])
    };
    ($($lit:literal,)+) => {
        $crate::attachment::OneOf([$(stringify!($lit),)*], ", ")
    };
    ($( $lit:literal )|+ ) => {
        $crate::attachment::OneOf([$(stringify!($lit),)*], " or ")
    }

}

/// wrapper attachment that is used to refer to the type of an object
/// rather than the value
pub struct Type(&'static str);

impl Type {
    // const fn when type_name is const fn in stable
    pub fn of<T>() -> Self {
        Self(std::any::type_name::<T>())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", self.0)
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Type").field(&self.0).finish()
    }
}
#[macro_export]
macro_rules! ty {
    ($t:ident) => {
        $crate::attachment::Type::of::<$t>()
    };
}

#[derive(Debug, thiserror::Error)]
#[error("already present")]
pub struct AlreadyPresent;
reportable!(AlreadyPresent);

#[derive(Debug, thiserror::Error)]
#[error("missing")]
pub struct Missing;

#[derive(Debug, thiserror::Error)]
#[error("unsupported")]
pub struct Unsupported;

#[derive(Debug, thiserror::Error)]
#[error("invalid")]
pub struct Invalid;

#[derive(Debug, thiserror::Error)]
#[error("invalid")]
pub struct Expectation<E, A> {
    expected: E,
    actual: A,
}

#[derive(Debug)]
pub struct DisplayDuration(pub Duration);
impl std::fmt::Display for DisplayDuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hms_string(self.0))
    }
}

impl From<Duration> for DisplayDuration {
    fn from(duration: Duration) -> Self {
        Self(duration)
    }
}

/// convert a [`Duration`] into a "0H00m00s" string
pub fn hms_string(duration: Duration) -> String {
    if duration.is_zero() {
        return "ZERO".to_string();
    }
    let s = duration.as_secs();
    let ms = duration.subsec_millis();
    // if only milliseconds available
    if s == 0 {
        return format!("{ms}ms");
    }
    // Grab total hours from seconds
    let (h, s) = (s / 3600, s % 3600);
    let (m, s) = (s / 60, s % 60);

    let mut hms = String::new();
    if h != 0 {
        hms += &format!("{h:02}H");
    }
    if m != 0 {
        hms += &format!("{m:02}m");
    }
    hms += &format!("{s:02}s");

    hms
}

// this is meant to explicitly indicate
// that the underyling `A` is being
// used as an index key for getter methods
// such as `HashMap` keys and `Vec` indices
#[derive(Debug, thiserror::Error)]
#[error("Index[{0}]")]
pub struct Index<I: std::fmt::Display>(pub I);

// Property references a struct's field
#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct StructField<I: std::fmt::Display>(pub I);

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use error_stack::bail;

    use crate::{InvalidInput, Reportable};

    use super::*;

    #[derive(Debug)]
    enum MyFlag {
        Help,
        Version,
        DryRun,
    }

    impl FromStr for MyFlag {
        type Err = Report<InvalidInput>;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let flag = match s {
                "--help" | "-h" => Self::Help,
                "--version" | "-v" => Self::Version,
                "--dry-run" => Self::DryRun,
                _ => bail!(InvalidInput::attach(s.to_string())),
            };
            Ok(flag)
        }
    }

    #[test]
    fn oneof_comma() {
        crate::init_colour();
        /// InvalidInput::expected_actual(, id)
        fn output() -> Result<MyFlag, Report<InvalidInput>> {
            let flag = "--verbose";
            flag.parse::<MyFlag>().attach_printable(MustBe(oneof!(
                "--help",
                "--version",
                "--dry-run",
            )))
        }
        eprintln!("{:?}", output().unwrap_err())
    }

    #[test]
    fn oneof_pipe() {
        crate::init_colour();
        /// InvalidInput::expected_actual(, id)
        fn output() -> Result<MyFlag, Report<InvalidInput>> {
            let flag = "--verbose";
            flag.parse::<MyFlag>()
                .attach_printable(MustBe(oneof!("--help" | "--version" | "--dry-run")))
        }
        eprintln!("{:?}", output().unwrap_err())
    }
}
