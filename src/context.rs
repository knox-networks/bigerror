use crate::{
    attachment::{self, DisplayDuration, Field, Unsupported},
    AttachExt, Reportable, WithHeader,
};

use std::{borrow::Cow, path::Path, time::Duration};
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
#[error("FsError")]
pub struct FsError;
reportable!(FsError);

#[derive(Debug, thiserror::Error)]
#[error("SetupError")]
pub struct SetupError;
reportable!(SetupError);

#[derive(Debug, thiserror::Error)]
#[error("ConversionError")]
pub struct ConversionError;
reportable!(ConversionError);

#[derive(Debug, thiserror::Error)]
#[error("InvalidInput{0}")]
pub struct InvalidInput(pub(crate) Header);

// a context header
#[derive(Debug, Default)]
pub enum Header {
    Slice(&'static str),
    String(String),
    #[default]
    Empty,
}

impl From<String> for Header {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&'static str> for Header {
    fn from(value: &'static str) -> Self {
        Self::Slice(value)
    }
}

impl std::fmt::Display for Header {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            Header::Slice(msg) => msg,
            Header::String(s) => s.as_str(),
            Header::Empty => return write!(f, ""),
        };
        // lead with a colon so that the parent context does not have to worry about optionality
        write!(f, ": {msg}")
    }
}

impl WithHeader for InvalidInput {
    fn header(msg: impl Into<Header>) -> Self {
        Self(msg.into())
    }
}

impl Default for InvalidInput {
    fn default() -> Self {
        Self(Header::Empty)
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
