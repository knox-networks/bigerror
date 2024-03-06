use std::{path::Path, time::Duration};

use error_stack::Context;

use crate::{
    attachment::{self, FromTo, Unsupported},
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
#[error("DecodeError")]
/// Represents errors emitted during while processing bytes into an object.
/// * byte types can can be represented by objects such as `&[u8]`, `bytes::Bytes`, and `Vec<u8>`
/// * used by codecs/serializers/deserializers
///   * used by codecs/serializers/deserializers
///
///  here's an example of types/traits that can emit encode/decode errors:
///  * https://docs.rs/tonic/latest/tonic/codec/trait.Encoder.html
///  * https://docs.rs/rkyv/latest/rkyv/ser/serializers/type.AllocSerializer.html
///  * https://docs.rs/serde/latest/serde/trait.Serializer.html
pub struct DecodeError;
reportable!(DecodeError);

/// Represents errors emitted during while turning an object into bytes.
/// See [`DecodeError`] for more details.
#[derive(Debug, thiserror::Error)]
#[error("EncodeError")]
pub struct EncodeError;
reportable!(EncodeError);

#[derive(Debug, thiserror::Error)]
#[error("AuthError")]
pub struct AuthError;
reportable!(AuthError);

#[derive(Debug, thiserror::Error)]
#[error("NetworkError")]
pub struct NetworkError;
reportable!(NetworkError);

/// Emitted while processing a string (UTF-8 or otherwise).
/// Usually associated with the [`std::str::FromStr`] trait and the `.parse::<SomeT>()` method
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

/// Emitted when casting existing [non scalar](https://en.wikipedia.org/w/index.php?title=Scalar_processor&useskin=vector#Scalar_data_type)
/// objects (such as structs, enums, and unions) into each other.
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
#[error("InvalidState")]
pub struct InvalidState;
reportable!(InvalidState);

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
        Report::new(Self).attach_printable(FromTo(ty!(F), ty!(T)))
    }
    #[track_caller]
    pub fn from<F, T>(ctx: impl Context) -> Report<Self> {
        Self::report(ctx).attach_printable(FromTo(ty!(F), ty!(T)))
    }
}

impl NotFound {
    #[track_caller]
    pub fn with_field(field: &'static str) -> Report<Self> {
        Report::new(Self).attach_printable(Field::new(field, attachment::Missing))
    }
}

impl ParseError {
    #[track_caller]
    pub fn with_field(field: &'static str) -> Report<Self> {
        Report::new(Self).attach_printable(Field::new(field, attachment::Invalid))
    }
}
