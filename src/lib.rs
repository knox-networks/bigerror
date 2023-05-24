use std::{fmt, path::Path};
use tracing::error;

pub use error_stack;
pub use error_stack::{Context, IntoReport, Report, ResultExt};
pub use thiserror;

pub trait Reportable
where
    Self: Sized + Context,
{
    fn report<E: Context>(e: E) -> Report<Self>;
    fn attach<A>(value: A) -> Report<Self>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static;
    fn attach_debug<A>(value: A) -> Report<Self>
    where
        A: fmt::Debug + Send + Sync + 'static;
    fn with_kv<A>(key: &str, value: A) -> Report<Self>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static;
    fn with_kv_debug<A>(key: &str, value: A) -> Report<Self>
    where
        A: std::fmt::Debug + Send + Sync + 'static;
    fn report_with_kv<A, E: Context>(e: E, key: &str, value: A) -> Report<Self>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static;
    fn report_inner<E, C>(e: E) -> Report<Self>
    where
        C: Context,
        E: ToReport<C>;
}

pub trait ReportAs: Sized {
    /// Type of the [`Ok`] value in the [`Result`]
    type Ok;

    fn report_as<C: Reportable>(self) -> Result<Self::Ok, Report<C>>;
}

impl<T, E: Context> ReportAs for Result<T, E> {
    type Ok = T;
    fn report_as<C: Reportable>(self) -> Result<T, Report<C>> {
        self.map_err(C::report)
    }
}

// TODO convert to #[derive(Reportable)]
#[macro_export]
macro_rules! reportable {
    ($context:ident) => {
        impl $crate::Reportable for $context {
            #[track_caller]
            fn report<E>(e: E) -> $crate::Report<Self>
            where
                E: $crate::Context,
            {
                $crate::Report::new(e).change_context(Self)
            }

            #[track_caller]
            fn attach<A>(value: A) -> $crate::Report<Self>
            where
                A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static,
            {
                $crate::Report::new(Self).attach_printable(value)
            }

            #[track_caller]
            fn attach_debug<A>(value: A) -> $crate::Report<Self>
            where
                A: std::fmt::Debug + Send + Sync + 'static,
            {
                $crate::Report::new(Self).attach_printable(format!("{value:?}"))
            }

            #[track_caller]
            fn with_kv<A>(key: &str, value: A) -> $crate::Report<Self>
            where
                A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static,
            {
                use $crate::AttachExt;
                $crate::Report::new(Self).attach_kv(key, value)
            }
            #[track_caller]
            fn with_kv_debug<A>(key: &str, value: A) -> $crate::Report<Self>
            where
                A: std::fmt::Debug + Send + Sync + 'static,
            {
                use $crate::AttachExt;
                $crate::Report::new(Self).attach_kv_debug(key, value)
            }
            #[track_caller]
            fn report_with_kv<A, E: $crate::Context>(
                e: E,
                key: &str,
                value: A,
            ) -> $crate::Report<Self>
            where
                A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static,
            {
                use $crate::AttachExt;
                $crate::Report::new(e)
                    .change_context(Self)
                    .attach_kv(key, value)
            }
            #[track_caller]
            fn report_inner<E, C>(e: E) -> $crate::Report<Self>
            where
                C: $crate::Context,
                E: $crate::ToReport<C>,
            {
                e.to_report().change_context(Self)
            }
        }
    };
}

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
#[error("MissingField")]
pub struct MissingField(&'static str);

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

impl InvalidInput {
    #[track_caller]
    pub fn with_path(path: impl AsRef<Path>) -> Report<Self> {
        let path = path.as_ref().display().to_string();
        Report::new(Self).attach_kv("path", path)
    }

    pub fn type_name<T: ?Sized>() -> Report<Self> {
        let type_name = std::any::type_name::<T>();
        Report::new(Self).attach_printable(format!("type: {type_name}"))
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
}

impl MissingField {
    #[track_caller]
    pub fn new(field: &'static str) -> Report<Self> {
        Report::new(Self(field))
    }
}

impl NotFound {
    #[track_caller]
    pub fn with_field(field: &'static str) -> Report<Self>
where {
        MissingField::new(field).change_context(Self)
    }
}

pub trait AttachExt {
    #[track_caller]
    fn attach_kv<A>(self, key: &str, value: A) -> Self
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static;
    fn attach_kv_debug<A>(self, key: &str, value: A) -> Self
    where
        A: fmt::Debug + Send + Sync + 'static;
}

impl<C> AttachExt for error_stack::Report<C> {
    #[track_caller]
    fn attach_kv<A>(self, key: &str, value: A) -> Self
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        self.attach_printable(format!("{key}: {value}"))
    }

    #[track_caller]
    fn attach_kv_debug<A>(self, key: &str, value: A) -> Self
    where
        A: fmt::Debug + Send + Sync + 'static,
    {
        self.attach_printable(format!("{key}: {value:?}"))
    }
}

impl<T, C> AttachExt for Result<T, Report<C>> {
    #[track_caller]
    fn attach_kv<A>(self, key: &str, value: A) -> Self
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        self.attach_printable(format!("{key}: {value}"))
    }

    #[track_caller]
    fn attach_kv_debug<A>(self, key: &str, value: A) -> Self
    where
        A: fmt::Debug + Send + Sync + 'static,
    {
        self.attach_printable(format!("{key}: {value:?}"))
    }
}

// intended to be a quick passthrough for propagating errors to the message log
// in a functional matter
pub trait LogError<T, E>
where
    E: fmt::Debug,
{
    // swallows and logs error
    fn log_err(self);
    // swallows and logs error with attachment
    fn log_attached_err<A>(self, attachment: A)
    where
        A: fmt::Debug + Send + Sync + 'static;
    // logs error and forwards
    fn and_log_err(self) -> Result<T, E>;
    // logs error and forwards with attachment
    fn and_attached_err<A>(self, attachment: A) -> Result<T, E>
    where
        A: fmt::Debug + Send + Sync + 'static;
}

impl<T, E> LogError<T, E> for Result<T, E>
where
    E: fmt::Debug,
{
    fn log_err(self) {
        if let Err(e) = self {
            error!(err = ?e);
        }
    }

    fn log_attached_err<A>(self, attachment: A)
    where
        A: fmt::Debug + Send + Sync + 'static,
    {
        if let Err(e) = self {
            error!(err = ?e, "{attachment:?}");
        }
    }

    fn and_log_err(self) -> Result<T, E> {
        if let Err(e) = &self {
            error!(err = ?e);
        }
        self
    }

    fn and_attached_err<A>(self, attachment: A) -> Result<T, E>
    where
        A: fmt::Debug + Send + Sync + 'static,
    {
        if let Err(e) = &self {
            error!(err = ?e, "{attachment:?}");
        }
        self
    }
}

pub trait ToReport<T> {
    #[track_caller]
    fn to_report(self) -> Report<T>;

    #[track_caller]
    fn change_context<C: Context>(self, context: C) -> Report<C>
    where
        Self: Sized,
    {
        self.to_report().change_context(context)
    }
}

pub trait FromReport<C> {
    fn to_report(self) -> Report<C>;
}

/// USAGE:
/// * `impl From<SomeError as ToReport<_> $(as $context:path)*> for OurError::Report(OurReportError)`
///  - Implements `From<E> where E: ToReport<_>` for  errors that implement [`ToReport`]
/// * impl From<Report<SomeError>>> for OurError::Report(TransactionError)
///  - Implements `From<Report<SomeError>>` for `Report<OurReportError>`
/// * `impl From<SomeError $(as $context:path)*> for OurError::Report(TransactionError)`
///  - Implements `From<SomeError>` for `Report<OurReportError>`
///  - Used to cast an error to `OurReportError`, can use `as` to do multiple `::from` conversion:
///    `impl From<SomeError as MiddleError>` does a concrete implementation of the `impl` below where `E` is `SomeError`:
///    `impl From<E> for OurError where E: Into<MiddleError> {}`
#[macro_export]
macro_rules! from_report {
    // ------
    // From<ToReport<_> as ctx>
    ({ impl From<$from:path as ToReport<_> $(as $context:path)*> $($tail:tt)* }) => {
        from_report!(@t[] @report_t[] @to_report[$from $(as $context)*,] $($tail)*);
    };
    // From<ToReport<_>>
    ({ impl From<$from:path as ToReport<_>> $($tail:tt)* }) => {
        from_report!(@t[] @report_t[] @to_report[$from,] $($tail)*);
    };
    // From<Report<T>>
    ({ impl From<Report<$from:path>> $($tail:tt)* }) => {
        from_report!(@t[] @report_t[$from] @to_report[] $($tail)*);
    };
    // From<T>
    ({ impl From<$from:path $(as $context:path)*> $($tail:tt)*  }) => {
        from_report!(@t[$from $(as $context)*,] @report_t[] @to_report[] $($tail)*);
    };
    // ------
    // From<Report<T>>
    (
    @t[$($t:path $(as $t_context:path)*,)*]
    @report_t[$($report_t:path)*]
    @to_report[$($to_report:path $(as $to_context:path)*,)*]
    impl From<Report<$from:path>> $($tail:tt)*) => {
        from_report!(
            @t[$($t $(as $t_context)*,)*]
            @report_t[$($report_t)* $from]
            @to_report[$($to_report $(as $to_context)*,)*]
            $($tail)*
        );
    };
    // From<ToReport<_> as ctx>
    (
    @t[$($t:path $(as $t_context:path)*,)*]
    @report_t[$($report_t:path)*]
    @to_report[$($to_report:path $(as $to_context:path)*,)*]
    impl From<$from:path as ToReport<_> $(as $context:path)*> $($tail:tt)*) => {
        from_report!(
            @t[$($t $(as $t_context)*,)*]
            @report_t[$($report_t)*]
            @to_report[$($to_report $(as $to_context)*,)* $from $(as $context)*,]
            $($tail)*
        );
    };
    // From<ToReport<_>>
    (
    @t[$($t:path $(as $t_context:path)*,)*]
    @report_t[$($report_t:path)*]
    @to_report[$($to_report:path $(as $to_context:path)*,)*]
    impl From<$from:path as ToReport<_>> $($tail:tt)*) => {
        from_report!(
            @t[$($t $(as $t_context)*,)*]
            @report_t[$($report_t)*]
            @to_report[$($to_report $(as $to_context)*,)* $from,]
            $($tail)*
        );
    };
    // From<T>
    (
    @t[$($t:path $(as $t_context:path)*,)*]
    @report_t[$($report_t:path)*]
    @to_report[$($to_report:path $(as $to_context:path)*,)*]
    impl From<$from:path $(as $context:path)*> $($tail:tt)*) => {
        from_report!(
            @t[$($t $(as $t_context)*,)* $from $(as $context)*,]
            @report_t[$($report_t)*]
            @to_report[$($to_report $(as $to_context)*,)*]
            $($tail)*
        );
    };
    (
    @t[$($t:path $(as $t_context:path)*,)*]
    @report_t[$($report_t:path)*]
    @to_report[$($to_report:path $(as $to_context:path)*,)*]
    for $for:ident::$variant:ident($inner:path)) => {
        // T
        $(impl From<$t> for $for {
            #[track_caller]
            fn from(e: $t) -> Self {
                let report = $crate::Report::new(e)
                $(.change_context($t_context))*
                  .change_context($inner);
                Self::$variant(report)
            }
        })*

        // Report<T>
        $(impl From<$crate::Report<$report_t>> for $for {
            #[track_caller]
            fn from(r: $crate::Report<$report_t>) -> Self {
                Self::$variant(r.change_context($inner))
            }
        })*
        // ToReport<_>
        $(impl From<$to_report> for $for {
            #[track_caller]
            fn from(e: $to_report) -> Self {
                use $crate::ToReport;
                let report =  e.to_report()
                $(.change_context($to_context))*
                  .change_context($inner);
                Self::$variant(report)
            }
        })*
        // original
        impl From<$crate::Report<$inner>> for $for {
            #[track_caller]
            fn from(r: $crate::Report<$inner>) -> Self {
                Self::$variant(r)
            }
        }
    };
    (impl From<Report<$from:path>> for $for:ident::$variant:ident) => {
        impl From<$crate::Report<$from>> for $for {
            #[track_caller]
            fn from(r: $crate::Report<$from>) -> Self {
                Self::$variant(r)
            }
        }
    };
    (impl From<$from:path as ToReport<$context:path>> for $for:ident::$variant:ident) => {
        impl From<$from> for $for {
            #[track_caller]
            fn from(e: $from) -> Self {
                Self::$variant(<$from as $crate::error::ToReport<_>>::change_context(
                    e, $context,
                ))
            }
        }
    };
    (impl From<$from:path $(as $context:path)*> for $for:ident::$variant:ident) => {
        impl From<$from> for $for {
            #[track_caller]
            fn from(e: $from) -> Self {
                let report = $crate::Report::new(e)
                    $(.change_context($context))* ;
                Self::$variant(report)
            }
        }
    };
}

#[macro_export]
macro_rules! to_report {
    (impl ToReport<$reportable:path> for $for:ident::$variant:ident) => {
        impl $crate::ToReport<$reportable> for $for {
            fn to_report(self) -> $crate::Report<$reportable> {
                #[allow(unreachable_patterns)]
                match self {
                    Self::$variant(inner) => inner,
                    _ => $crate::Report::new(self).change_context($reportable),
                }
            }
        }
    };
}
