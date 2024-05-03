use std::time::Duration;

use tracing::error;

pub use error_stack::{self, Context, Report, ResultExt};
pub use thiserror;

use crate::reportable;

pub trait Display: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {}

impl<A> Display for A where A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {}

pub trait Debug: std::fmt::Debug + Send + Sync + 'static {}

impl<A> Debug for A where A: std::fmt::Debug + Send + Sync + 'static {}

// used to wrap types that only implement `std::fmt::Debug`
#[derive(Debug)]
pub struct Dbg<A: Debug>(pub A);

impl<A: Debug> std::fmt::Display for Dbg<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

// simple key-value pair attachment
#[derive(Debug, PartialEq)]
pub struct KeyValue<K, V>(pub K, pub V);

impl<K: std::fmt::Display, V: std::fmt::Display> std::fmt::Display for KeyValue<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

impl<K: Display, V: Debug> KeyValue<K, Dbg<V>> {
    pub fn dbg(key: K, value: V) -> Self {
        Self(key, Dbg(value))
    }
}

/// Allows one to quickly specify a [`KeyValue`] pair, optionally using a
/// `ty:` prefix using the `$value` [`Type`] as the key
#[macro_export]
macro_rules! kv {
    (ty: $value: expr) => {
        $crate::KeyValue($crate::Type::of_val(&$value), $value)
    };
    ($value: expr) => {
        $crate::KeyValue(stringify!($value), $value)
    };
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
#[derive(PartialEq)]
pub struct Type(&'static str);

impl Type {
    // const fn when type_name is const fn in stable
    pub fn of<T>() -> Self {
        Self(std::any::type_name::<T>())
    }

    pub fn of_val<T: ?Sized>(_val: &T) -> Self {
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
        let curve_right = Symbol::CurveRight;
        let horizontal_left = Symbol::HorizontalLeft;
        let expected = KeyValue("expected", &self.expected);
        let actual = KeyValue("actual", &self.actual);
        // "expected": expected
        // ╰╴"actual": actual
        write!(f, "{expected}\n{curve_right}{horizontal_left}{actual}")
    }
}
impl<F: Display, T: Display> std::fmt::Display for FromTo<F, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let curve_right = Symbol::CurveRight;
        let horizontal_left = Symbol::HorizontalLeft;
        let from = KeyValue("from", &self.0);
        let to = KeyValue("to", &self.1);
        // "from": from
        // ╰╴"to": to
        write!(f, "{from}\n{curve_right}{horizontal_left}{to}")
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

impl std::ops::Deref for DisplayDuration {
    type Target = Duration;

    fn deref(&self) -> &Self::Target {
        &self.0
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
// used as an index key for getter methods in a collection
// such as `HashMap` keys and `Vec` indices
#[derive(Debug, thiserror::Error)]
#[error("idx [{0}: {}]", std::any::type_name::<I>())]
pub struct Index<I: std::fmt::Display>(pub I);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn kv_macro() {
        let foo = "Foo";

        // foo: "Foo"
        assert_eq!(kv!(foo), KeyValue("foo", "Foo"));
        // <&str>: "Foo"
        assert_eq!(kv!(ty: foo), KeyValue(Type::of_val(&foo), "Foo"));

        let foo = 13;

        // <i32>: 13
        assert_eq!(kv!(ty: foo), KeyValue(Type::of_val(&foo), 13));
        // ensure literal values are handled correctly
        assert_eq!(kv!(ty: 13), KeyValue(Type::of_val(&13), 13));
    }
}
