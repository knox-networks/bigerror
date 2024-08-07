//! Compatibility module to convert errors from other libraries into [`Report`].
//!
//! In order to convert these error types, use [`IntoReportCompat::into_report()`].

use crate::error_stack::Report;

#[cfg(feature = "anyhow")]
mod anyhow;
#[cfg(feature = "eyre")]
mod eyre;

/// Compatibility trait to convert from external libraries to [`Report`].
///
/// **Note**: It's not possible to implement [`Context`] on other error libraries' types from within
/// `error-stack` as the trait has a blanket implementation relying on [`Error`]. Thus, implementing
/// the trait would violate the orphan rule; the upstream crate could implement [`Error`] and this
/// would imply an implementation for [`Context`]. This also implies, that it's not possible to
/// implement [`ResultExt`] from within `error-stack`.
///
/// [`ResultExt`]: error_stack::ResultExt
/// [`Context`]: error_stack::Context
/// [`Error`]: core::error::Error
pub trait IntoReportCompat: Sized {
    /// Type of the [`Ok`] value in the [`BigResult`]
    type Ok;

    /// Type of the resulting [`Err`] variant wrapped inside a [`Report<E>`].
    ///
    /// [`Report<E>`]: error_stack::Report
    type Err;

    /// Converts the [`Err`] variant of the [`BigResult`] to a [`Report`]
    ///
    /// [`Report`]: error_stack::Report
    fn into_report(self) -> Result<Self::Ok, Report<Self::Err>>;
}
