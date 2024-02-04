use crate::{
    attachment::{self, DisplayDuration, Field, Unsupported},
    AttachExt, ContextHeader, Reportable,
};

use std::{path::Path, time::Duration};
use tracing::error;

pub use error_stack::{self, Context, Report, ResultExt};
pub use thiserror;

use crate::reportable;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct BoxError(Box<dyn std::error::Error + 'static + Send + Sync>);

// this is a `BoxError` that satistifes `core::error::Error`
// using `core::fmt::Debug` and `core::fmt::Display`
#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct BoxCoreError(Box<dyn CoreError>);

#[derive(Debug, thiserror::Error)]
#[error("NetworkError")]
pub struct NetworkError;
reportable!(NetworkError);

#[derive(Debug, thiserror::Error)]
#[error("ParseError")]
pub struct ParseError;
reportable!(ParseError);

#[derive(Debug, thiserror::Error)]
#[error("NotFound")]
pub struct NotFound;
reportable!(NotFound);

#[derive(Debug, thiserror::Error)]
#[error("DbError")]
pub struct DbError;
reportable!(DbError);

#[derive(Debug, thiserror::Error)]
#[error("ConversionError")]
pub struct ConversionError;
reportable!(ConversionError);

#[derive(Debug, thiserror::Error)]
#[error("InvalidInput{0}")]
pub struct InvalidInput(Header);

// a context header
#[derive(Debug)]
pub struct Header(Option<Box<dyn attachment::Display>>);

impl Header {
    pub fn new(value: impl attachment::Display) -> Self {
        Self(Some(Box::new(value)))
    }
}

impl std::fmt::Display for Header {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(attachment) = &self.0 {
            // lead with a colon so that the parent context does not have to worry about optionality
            write!(f, ": {attachment}")
        } else {
            write!(f, "")
        }
    }
}

impl ContextHeader for InvalidInput {
    fn set_header(value: impl attachment::Display) -> Self {
        Self(Header::new(value))
    }
}

impl Default for InvalidInput {
    fn default() -> Self {
        Self(Header(None))
    }
}

#[derive(Debug, thiserror::Error)]
#[error("InvalidStatus")]
pub struct InvalidStatus;
reportable!(InvalidStatus);

#[derive(Debug, thiserror::Error)]
#[error("ConfigError")]
pub struct ConfigError;
reportable!(ConfigError);

#[derive(Debug, thiserror::Error)]
#[error("BuildError")]
pub struct BuildError;
reportable!(BuildError);

#[derive(Debug, thiserror::Error)]
#[error("{}", Field::new("timeout", DisplayDuration(*.0)))]
pub struct Timeout(pub Duration);

pub trait CoreError: core::fmt::Debug + core::fmt::Display + Send + Sync + 'static {}

impl<T> CoreError for T where T: core::fmt::Debug + core::fmt::Display + Send + Sync + 'static {}

impl BoxError {
    #[track_caller]
    pub fn new<E>(err: E) -> Report<Self>
    where
        E: std::error::Error + 'static + Send + Sync,
    {
        Report::new(Self(Box::new(err)))
    }

    #[track_caller]
    pub fn from(err: Box<dyn std::error::Error + 'static + Send + Sync>) -> Report<Self> {
        Report::new(Self(err))
    }
}
impl BoxCoreError {
    #[track_caller]
    pub fn new<E: CoreError>(err: E) -> Report<Self> {
        Report::new(Self(Box::new(err)))
    }

    #[track_caller]
    pub fn from(err: Box<dyn CoreError>) -> Report<Self> {
        Report::new(Self(err))
    }
}

impl InvalidInput {
    #[track_caller]
    pub fn with_path(path: impl AsRef<Path>) -> Report<Self> {
        let path = path.as_ref().display().to_string();
        Report::new(Self::default()).attach_kv("path", path)
    }

    #[track_caller]
    pub fn unsupported() -> Report<Self> {
        Self::attach(Unsupported)
    }
}

impl ConversionError {
    #[track_caller]
    pub fn new<F: ?Sized, T: ?Sized>() -> Report<Self> {
        let from = std::any::type_name::<F>();
        let to = std::any::type_name::<T>();
        Report::new(Self)
            .attach_printable(format!("from: {from}"))
            .attach_printable(format!("to: {to}"))
    }

    #[track_caller]
    pub fn from<F, T>(ctx: impl Context) -> Report<Self>
    where
        F: ?Sized,
        T: ?Sized,
    {
        let from = std::any::type_name::<F>();
        let to = std::any::type_name::<T>();
        Self::report(ctx)
            .attach_printable(format!("from: {from}"))
            .attach_printable(format!("to: {to}"))
    }
}

impl NotFound {
    #[track_caller]
    pub fn with_field(field: &'static str) -> Report<Self>
where {
        Report::new(Self).attach_printable(Field::new(field, attachment::Missing))
    }
}

impl ParseError {
    #[track_caller]
    pub fn with_field(field: &'static str) -> Report<Self>
where {
        Report::new(Self).attach_printable(Field::new(field, attachment::Invalid))
    }
}
