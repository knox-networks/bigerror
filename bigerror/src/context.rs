use std::{any, path::Path, time::Duration};

use crate::{
    attachment::{
        FromTo, Unsupported, {self},
    },
    ty, AttachExt, Context, Report, Reportable,
};

use crate::{attachment::DisplayDuration, Field};

/// Used to enacpsulate opaque `dyn std::error::Error` types
#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct BoxError(Box<dyn std::error::Error + 'static + Send + Sync>);

/// this is a [`BoxError`] that satistifes [`core::error::Error`]
/// using [`core::fmt::Debug`] and [`core::fmt::Display`]
#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct BoxCoreError(Box<dyn CoreError>);
/// Represents errors emitted during while processing bytes into an object.
/// * byte types can can be represented by objects such as `&[u8]`, `bytes::Bytes`, and `Vec<u8>`
/// * used by codecs/serializers/deserializers
///   * used by codecs/serializers/deserializers
///
///  here's an example of types/traits that can emit encode/decode errors:
///  * https://docs.rs/tonic/latest/tonic/codec/trait.Encoder.html
///  * https://docs.rs/rkyv/latest/rkyv/ser/serializers/type.AllocSerializer.html
///  * https://docs.rs/serde/latest/serde/trait.Serializer.html
#[crate::derive_ctx]
pub struct DecodeError;

/// Emitted while turning an object into bytes.
/// See [`DecodeError`] for more details.
#[crate::derive_ctx]
pub struct EncodeError;

#[crate::derive_ctx]
pub struct DeriveError;

/// Emitted during an authorization/verification check
#[crate::derive_ctx]
pub struct AuthError;

#[crate::derive_ctx]
pub struct NetworkError;

/// Emitted while processing a string (UTF-8 or otherwise).
/// Usually associated with the [`std::str::FromStr`] trait and the `.parse::<SomeT>()` method
#[crate::derive_ctx]
pub struct ParseError;

/// Represents the conversion of an `Option::<T>::None` into a [`Report`]
#[crate::derive_ctx]
pub struct NotFound;

#[crate::derive_ctx]
pub struct DbError;

/// An error that is related to filesystem operations such as those in [`std::fs`]
#[crate::derive_ctx]
pub struct FsError;

/// Emitted during the startup/provisioning phase of a program
#[crate::derive_ctx]
pub struct SetupError;

/// Emitted during transformations between [non scalar](https://en.wikipedia.org/w/index.php?title=Scalar_processor&useskin=vector#Scalar_data_type)
/// objects (such as structs, enums, and unions).
#[crate::derive_ctx]
pub struct ConversionError;

#[crate::derive_ctx]
pub struct InvalidInput;

#[crate::derive_ctx]
pub struct InvalidStatus;

#[crate::derive_ctx]
pub struct InvalidState;

/// Emitted during runtime, indicates problems with input/default settings
#[crate::derive_ctx]
pub struct ConfigError;

/// Typically emitted by a `build.rs` failure
#[crate::derive_ctx]
pub struct BuildError;

#[derive(Debug, thiserror::Error)]
#[error("{}", Field::new("timeout", DisplayDuration(*.0)))]
pub struct Timeout(pub Duration);

#[crate::derive_ctx]
pub struct AssertionError;

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
        let type_name = any::type_name::<T>();
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
