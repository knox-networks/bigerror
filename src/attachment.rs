use tracing::error;

pub use error_stack::{self, Context, IntoReport, Report, ResultExt};
pub use thiserror;

use crate::reportable;

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
