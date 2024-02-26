use std::{path::Path, time::Duration};

use error_stack::Context;

use crate::{
    attachment::{self, Unsupported},
    ty, AttachExt, Report, Reportable,
};

use crate::{attachment::DisplayDuration, reportable, Field};

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
#[error("InvalidInput")]
pub struct InvalidInput;
reportable!(InvalidInput);

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
        Report::new(Self).attach_kv("path", path)
    }

    #[track_caller]
    pub fn type_name<T: ?Sized>() -> Report<Self> {
        let type_name = std::any::type_name::<T>();
        Report::new(Self).attach_printable(format!("type: {type_name}"))
    }

    #[track_caller]
    pub fn unsupported() -> Report<Self> {
        Report::new(Self).attach_printable(Unsupported)
    }
}

impl ConversionError {
    #[track_caller]
    pub fn new<F, T>() -> Report<Self> {
        Report::new(Self)
            .attach_printable(format!("from: {}", ty!(F)))
            .attach_printable(format!("to: {}", ty!(T)))
    }

    #[track_caller]
    pub fn from<F, T>(ctx: impl Context) -> Report<Self> {
        Self::report(ctx)
            .attach_printable(format!("from: {}", ty!(F)))
            .attach_printable(format!("to: {}", ty!(T)))
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
