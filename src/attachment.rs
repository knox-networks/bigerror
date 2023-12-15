use tracing::error;

pub use error_stack::{self, Context, Report, ResultExt};
pub use thiserror;

use crate::reportable;

pub trait Display: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {}

impl<A> Display for A where A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {}

pub trait Debug: std::fmt::Debug + Send + Sync + 'static {}

impl<A> Debug for A where A: std::fmt::Debug + Send + Sync + 'static {}

#[derive(Debug, thiserror::Error)]
#[error("\"{name}\": {status}")]
pub struct Field<S> {
    name: &'static str,
    status: S,
}

impl<S> Field<S> {
    pub fn new(name: &'static str, status: S) -> Self {
        Self { name, status }
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

// this is meant to explicitly indicate
// that the underyling `A` is being
// used as an index key for getter methods
// such as `HashMap` keys and `Vec` indices
#[derive(Debug, thiserror::Error)]
#[error("Index[{0}]")]
pub struct Index<I: std::fmt::Display>(pub I);
