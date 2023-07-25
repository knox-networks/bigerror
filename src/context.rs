use crate::{
    attachment::{self, Field, Unsupported},
    AttachExt, Reportable,
};

use std::path::Path;
use tracing::error;

pub use error_stack::{self, Context, IntoReport, Report, ResultExt};
pub use thiserror;

use crate::reportable;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct BoxError(Box<dyn std::error::Error + 'static + Send + Sync>);

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
    pub fn expected_actual<A>(expected: A, actual: A) -> Report<Self>
    where
        A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static,
    {
        Self::with_kv("expected", expected).attach_kv("actual", actual)
    }

    #[track_caller]
    pub fn unsupported() -> Report<Self> {
        Report::new(Self).attach_printable(Unsupported)
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
