use tracing::error;

pub use error_stack::{self, Context, IntoReport, Report, ResultExt};
pub use thiserror;

use crate::reportable;
#[derive(Debug, thiserror::Error)]
#[error("AlreadyPresent")]
pub struct AlreadyPresent;
reportable!(AlreadyPresent);

impl AlreadyPresent {}

#[derive(Debug, thiserror::Error)]
#[error("MissingField: {0}")]
pub struct MissingField(&'static str);
impl MissingField {
    #[track_caller]
    pub fn new(field: &'static str) -> Report<Self> {
        Report::new(Self(field))
    }
}
