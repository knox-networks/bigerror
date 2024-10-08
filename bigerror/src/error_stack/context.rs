#[cfg(nightly)]
use core::error::{Error, Request};
use core::fmt;
#[cfg(all(not(nightly), feature = "std"))]
use std::error::Error;

use crate::{error_stack::Report, ThinContext};

/// Defines the current context of a [`Report`].
///
/// When in a `std` environment or on a nightly toolchain, every [`Error`] is a valid `Context`.
/// This trait is not limited to [`Error`]s and can also be manually implemented on a type.
///
/// ## Example
///
/// Used for creating a [`Report`] or for switching the [`Report`]'s context:
///
/// ```rust
/// use std::{fmt, fs, io};
///
/// use bigerror::{Context, BigResult, Report, ResultExt};
///
/// # type Config = ();
/// #[derive(Debug)]
/// pub enum ConfigError {
///     ParseError,
/// }
///
/// impl fmt::Display for ConfigError {
///     # #[allow(unused_variables)] // `fmt` is not used in this example
///     fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
///         # const _: &str = stringify! {
///         ...
///         # }; Ok(())
///     }
/// }
///
/// // In this scenario, `Error` is not implemented for `ConfigError` for some reason, so implement
/// // `Context` manually.
/// impl Context for ConfigError {}
///
/// pub fn read_file(path: &str) -> BigResult<String, io::Error> {
///     // Creates a `Report` from `io::Error`, the current context is `io::Error`
///     fs::read_to_string(path).map_err(Report::new)
/// }
///
/// pub fn parse_config(path: &str) -> BigResult<Config, ConfigError> {
///     // The return type of `parse_config` requires another context. By calling `change_context`
///     // the context may be changed.
///     read_file(path).change_context(ConfigError::ParseError)?;
///
///     # const _: &str = stringify! {
///     ...
///     # }; Ok(())
/// }
/// # let err = parse_config("invalid-path").unwrap_err();
/// # assert!(err.contains::<io::Error>());
/// # assert!(err.contains::<ConfigError>());
/// ```
pub trait Context: fmt::Display + fmt::Debug + Send + Sync + 'static {
    /// Provide values which can then be requested by [`Report`].
    #[cfg(nightly)]
    #[allow(unused_variables)]
    fn provide<'a>(&'a self, request: &mut Request<'a>) {}
}

// For Reference, old error-stack blanket `From<C>` impl:
// ===
// impl<C> From<C> for Report<C>
// where
//     C: Context,
// {
//     #[track_caller]
//     #[inline]
//     fn from(context: C) -> Self {
//         Self::new(context)
//     }
// }
// ===
impl<E, C> From<E> for Report<C>
where
    E: Context,
    C: ThinContext,
{
    #[track_caller]
    #[inline]
    fn from(err: E) -> Self {
        Report::new(err).change_context(C::VALUE)
    }
}

impl<C: Error + Send + Sync + 'static> Context for C {
    #[cfg(nightly)]
    fn provide<'a>(&'a self, request: &mut Request<'a>) {
        Error::provide(self, request);
    }
}
