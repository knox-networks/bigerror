use derive_more as dm;
use std::{path::Path, time::Duration};

use error_stack::Context;

use crate::{
    attachment::{self, simple_type_name, Display, FromTo, Unsupported},
    ty, AttachExt, Index, Report, ThinContext,
};

use crate::{attachment::DisplayDuration, Field};

/// Used to enacpsulate opaque `dyn std::error::Error` types
#[derive(Debug, dm::Display)]
#[display("{_0}")]
pub struct BoxError(Box<dyn core::error::Error + 'static + Send + Sync>);
impl ::core::error::Error for BoxError {}

/// Represents errors emitted during while processing bytes into an object.
/// * byte types can can be represented by objects such as `&[u8]`, `bytes::Bytes`, and `Vec<u8>`
/// * used by codecs/serializers/deserializers
///   * used by codecs/serializers/deserializers
///
///  here's an example of types/traits that can emit encode/decode errors:
///  * <https://docs.rs/tonic/latest/tonic/codec/trait.Encoder.html>
///  * <https://docs.rs/rkyv/latest/rkyv/ser/serializers/type.AllocSerializer.html>
///  * <https://docs.rs/serde/latest/serde/trait.Serializer.html>
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct DecodeError;

/// Emitted while turning an object into bytes.
/// See [`DecodeError`] for more details.
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct EncodeError;

/// Emitted during an authorization/verification check
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct AuthError;

#[derive(ThinContext)]
#[bigerror(crate)]
pub struct NetworkError;

/// Emitted while processing a string (UTF-8 or otherwise).
/// Usually associated with the [`std::str::FromStr`] trait and the `.parse::<SomeT>()` method
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct ParseError;

/// Represents the conversion of an `Option::<T>::None` into a [`Report`]
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct NotFound;

#[derive(ThinContext)]
#[bigerror(crate)]
pub struct DbError;

/// An error that is related to filesystem operations such as those in [`std::fs`]
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct FsError;

/// Emitted during the startup/provisioning phase of a program
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct SetupError;

/// Emitted during transformations between [non scalar](https://en.wikipedia.org/w/index.php?title=Scalar_processor&useskin=vector#Scalar_data_type)
/// objects (such as structs, enums, and unions).
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct ConversionError;

#[derive(ThinContext)]
#[bigerror(crate)]
pub struct InvalidInput;

#[derive(ThinContext)]
#[bigerror(crate)]
pub struct InvalidStatus;

#[derive(ThinContext)]
#[bigerror(crate)]
pub struct InvalidState;

/// Emitted during runtime, indicates problems with input/default settings
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct ConfigError;

/// Typically emitted by a `build.rs` failure
#[derive(ThinContext)]
#[bigerror(crate)]
pub struct BuildError;

#[derive(Debug, dm::Display)]
#[display("{}", Field::new("timeout", DisplayDuration(self.0)))]
pub struct Timeout(pub Duration);
impl ::core::error::Error for Timeout {}

#[derive(ThinContext)]
#[bigerror(crate)]
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

impl InvalidInput {
    #[track_caller]
    pub fn with_path(path: impl AsRef<Path>) -> Report<Self> {
        let path = path.as_ref().display().to_string();
        Report::new(Self).attach_kv("path", path)
    }

    #[track_caller]
    pub fn type_name<T: ?Sized>() -> Report<Self> {
        let type_name = simple_type_name::<T>();
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

    pub fn with_index<T, K: Display>(key: K) -> Report<Self> {
        Self::with_kv(Index(key), ty!(T))
    }
}

impl ParseError {
    #[track_caller]
    pub fn with_field(field: &'static str) -> Report<Self> {
        Report::new(Self).attach_printable(Field::new(field, attachment::Invalid))
    }
}
