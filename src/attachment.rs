use std::time::Duration;

use tracing::error;

pub use error_stack::{self, Context, Report, ResultExt};
pub use thiserror;

use crate::reportable;

pub trait Display: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {}

impl<A> Display for A where A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {}

pub trait Debug: std::fmt::Debug + Send + Sync + 'static {}

impl<A> Debug for A where A: std::fmt::Debug + Send + Sync + 'static {}

#[derive(Debug)]
pub struct Field<K, S> {
    key: K,
    status: S,
}

impl<K: Display, S: Display> std::fmt::Display for Field<K, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.status)
    }
}

impl<K: Display, S: Display> Field<K, S> {
    pub fn new(key: K, status: S) -> Self {
        Self { key, status }
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
