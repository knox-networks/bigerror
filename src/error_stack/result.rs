use core::fmt;

use crate::{Context, Report};

/// [`std::result::Result`]`<T, `[`Report<C>`](Report)`>`
///
/// A reasonable return type to use throughout an application.
///
/// The `BigResult` type can be used with one or two parameters, where the first parameter represents
/// the [`Ok`] arm and the second parameter `Context` is used as in [`Report<C>`].
///
/// # Examples
///
/// `BigResult` can also be used in `fn main()`:
///
/// ```rust
/// # fn has_permission(_: (), _: ()) -> bool { true }
/// # fn get_user() -> Result<(), AccessError> { Ok(()) }
/// # fn get_resource() -> Result<(), AccessError> { Ok(()) }
/// # #[derive(Debug)] enum AccessError { PermissionDenied((), ()) }
/// # impl core::fmt::Display for AccessError {
/// #    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { Ok(()) }
/// # }
/// # impl bigerror::Context for AccessError {}
/// use bigerror::{ensure, BigResult};
///
/// fn main() -> BigResult<(), AccessError> {
///     let user = get_user()?;
///     let resource = get_resource()?;
///
///     ensure!(
///         has_permission(user, resource),
///         AccessError::PermissionDenied(user, resource)
///     );
///
///     # const _: &str = stringify! {
///     ...
///     # }; Ok(())
/// }
/// ```
pub type BigResult<T, C> = core::result::Result<T, Report<C>>;

/// Extension trait for [`core::result::Result`] to provide context information on
/// [`Report`]s.
pub trait ResultExt {
    /// The [`Context`] type of the [`BigResult`].
    type Context: Context;

    /// Type of the [`Ok`] value in the [`BigResult`]
    type Ok;

    /// Adds a new attachment to the [`Report`] inside the [`BigResult`].
    ///
    /// Applies [`Report::attach`] on the [`Err`] variant, refer to it for more information.
    fn attach<A>(self, attachment: A) -> core::result::Result<Self::Ok, Report<Self::Context>>
    where
        A: Send + Sync + 'static;

    /// Lazily adds a new attachment to the [`Report`] inside the [`BigResult`].
    ///
    /// Applies [`Report::attach`] on the [`Err`] variant, refer to it for more information.
    fn attach_lazy<A, F>(
        self,
        attachment: F,
    ) -> core::result::Result<Self::Ok, Report<Self::Context>>
    where
        A: Send + Sync + 'static,
        F: FnOnce() -> A;

    /// Adds a new printable attachment to the [`Report`] inside the [`BigResult`].
    ///
    /// Applies [`Report::attach_printable`] on the [`Err`] variant, refer to it for more
    /// information.
    fn attach_printable<A>(
        self,
        attachment: A,
    ) -> core::result::Result<Self::Ok, Report<Self::Context>>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static;

    /// Lazily adds a new printable attachment to the [`Report`] inside the [`BigResult`].
    ///
    /// Applies [`Report::attach_printable`] on the [`Err`] variant, refer to it for more
    /// information.
    fn attach_printable_lazy<A, F>(
        self,
        attachment: F,
    ) -> core::result::Result<Self::Ok, Report<Self::Context>>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
        F: FnOnce() -> A;

    /// Changes the context of the [`Report`] inside the [`BigResult`].
    ///
    /// Applies [`Report::change_context`] on the [`Err`] variant, refer to it for more information.
    fn change_context<C>(self, context: C) -> core::result::Result<Self::Ok, Report<C>>
    where
        C: Context;

    /// Lazily changes the context of the [`Report`] inside the [`BigResult`].
    ///
    /// Applies [`Report::change_context`] on the [`Err`] variant, refer to it for more information.
    fn change_context_lazy<C, F>(self, context: F) -> core::result::Result<Self::Ok, Report<C>>
    where
        C: Context,
        F: FnOnce() -> C;
}

impl<T, C> ResultExt for core::result::Result<T, C>
where
    C: Context,
{
    type Context = C;
    type Ok = T;

    #[track_caller]
    fn attach<A>(self, attachment: A) -> BigResult<T, C>
    where
        A: Send + Sync + 'static,
    {
        match self {
            Ok(value) => Ok(value),
            Err(error) => Err(Report::new(error).attach(attachment)),
        }
    }

    #[track_caller]
    fn attach_lazy<A, F>(self, attachment: F) -> BigResult<T, C>
    where
        A: Send + Sync + 'static,
        F: FnOnce() -> A,
    {
        match self {
            Ok(value) => Ok(value),
            Err(error) => Err(Report::new(error).attach(attachment())),
        }
    }

    #[track_caller]
    fn attach_printable<A>(self, attachment: A) -> BigResult<T, C>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        match self {
            Ok(value) => Ok(value),
            Err(error) => Err(Report::new(error).attach_printable(attachment)),
        }
    }

    #[track_caller]
    fn attach_printable_lazy<A, F>(self, attachment: F) -> BigResult<T, C>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
        F: FnOnce() -> A,
    {
        match self {
            Ok(value) => Ok(value),
            Err(error) => Err(Report::new(error).attach_printable(attachment())),
        }
    }

    #[track_caller]
    fn change_context<C2>(self, context: C2) -> BigResult<T, C2>
    where
        C2: Context,
    {
        match self {
            Ok(value) => Ok(value),
            Err(error) => Err(Report::new(error).change_context(context)),
        }
    }

    #[track_caller]
    fn change_context_lazy<C2, F>(self, context: F) -> BigResult<T, C2>
    where
        C2: Context,
        F: FnOnce() -> C2,
    {
        match self {
            Ok(value) => Ok(value),
            Err(error) => Err(Report::new(error).change_context(context())),
        }
    }
}

impl<T, C> ResultExt for BigResult<T, C>
where
    C: Context,
{
    type Context = C;
    type Ok = T;

    #[track_caller]
    fn attach<A>(self, attachment: A) -> Self
    where
        A: Send + Sync + 'static,
    {
        // Can't use `map_err` as `#[track_caller]` is unstable on closures
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.attach(attachment)),
        }
    }

    #[track_caller]
    fn attach_lazy<A, F>(self, attachment: F) -> Self
    where
        A: Send + Sync + 'static,
        F: FnOnce() -> A,
    {
        // Can't use `map_err` as `#[track_caller]` is unstable on closures
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.attach(attachment())),
        }
    }

    #[track_caller]
    fn attach_printable<A>(self, attachment: A) -> Self
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        // Can't use `map_err` as `#[track_caller]` is unstable on closures
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.attach_printable(attachment)),
        }
    }

    #[track_caller]
    fn attach_printable_lazy<A, F>(self, attachment: F) -> Self
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
        F: FnOnce() -> A,
    {
        // Can't use `map_err` as `#[track_caller]` is unstable on closures
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.attach_printable(attachment())),
        }
    }

    #[track_caller]
    fn change_context<C2>(self, context: C2) -> BigResult<T, C2>
    where
        C2: Context,
    {
        // Can't use `map_err` as `#[track_caller]` is unstable on closures
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.change_context(context)),
        }
    }

    #[track_caller]
    fn change_context_lazy<C2, F>(self, context: F) -> BigResult<T, C2>
    where
        C2: Context,
        F: FnOnce() -> C2,
    {
        // Can't use `map_err` as `#[track_caller]` is unstable on closures
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.change_context(context())),
        }
    }
}
