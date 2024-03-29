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
    ($type:ty) => {
        $crate::attachment::Type::of::<$type>()
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
pub struct Expectation<E, A> {
    pub expected: E,
    pub actual: A,
}

#[derive(Debug, thiserror::Error)]
pub struct FromTo<F, T>(pub F, pub T);

#[allow(dead_code)]
enum Symbol {
    Vertical,
    VerticalRight,
    Horizontal,
    HorizontalLeft,
    HorizontalDown,
    ArrowRight,
    CurveRight,
    Space,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let utf8 = match self {
            Self::Vertical => "\u{2502}",       // │
            Self::VerticalRight => "\u{251c}",  // ├
            Self::Horizontal => "\u{2500}",     // ─
            Self::HorizontalLeft => "\u{2574}", // ╴
            Self::HorizontalDown => "\u{252c}", // ┬
            Self::ArrowRight => "\u{25b6}",     // ▶
            Self::CurveRight => "\u{2570}",     // ╰
            Self::Space => " ",
        };
        write!(f, "{}", utf8)
    }
}

impl<E: Display, A: Display> std::fmt::Display for Expectation<E, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cr = Symbol::CurveRight;
        let hl = Symbol::HorizontalLeft;
        write!(
            f,
            "{}\n{cr}{hl}{}",
            KeyValue("expected", &self.expected),
            KeyValue("actual", &self.actual)
        )
    }
}
impl<F: Display, T: Display> std::fmt::Display for FromTo<F, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cr = Symbol::CurveRight;
        let hl = Symbol::HorizontalLeft;
        let from = KeyValue("from", &self.0);
        let to = KeyValue("to", &self.1);
        write!(f, "{from}\n{cr}{hl}{to}",)
    }
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
// that the underlying `A` is being
// used as an index key for getter methods
// such as `HashMap` keys and `Vec` indices
#[derive(Debug, thiserror::Error)]
#[error("Index[{0}]")]
pub struct Index<I: std::fmt::Display>(pub I);
