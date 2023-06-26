use error_stack::fmt::ColorMode;
use std::fmt;
use tracing::{debug, error, info, trace, warn, Level};

pub use error_stack::{self, Context, IntoReport, Report, ResultExt};
pub use thiserror;

pub mod attachment;
pub mod context;

pub use context::*;

// TODO we'll have to do a builder pattern here at
// some point
pub fn init_colour() {
    Report::set_color_mode(ColorMode::Color);
}

pub fn init_emphasis() {
    Report::set_color_mode(ColorMode::Emphasis);
}

pub fn init_no_ansi() {
    Report::set_color_mode(ColorMode::None);
}

/// `Reportable` behaves as an `error_stack::ContextExt`
/// ideally used for zero sized errors or ones that hold a `'static` ref/value
pub trait Reportable
where
    Self: Sized + Context,
{
    fn report<C: Context>(ctx: C) -> Report<Self>;
    // TODO
    // fn report_dyn_err(err: impl std::error::Error + 'static + Send + Sync)
    // -> Report<Self>;
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
    fn report_with_kv<A, C: Context>(ctx: C, key: &str, value: A) -> Report<Self>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static;
    fn report_inner<C: Context>(err: impl ToReport<C>) -> Report<Self>;
    fn value() -> Self;
}

/// Extends [`error_stack::IntoReport`] to allow an implicit `E -> Report<C>` inference
pub trait ReportAs {
    /// Type of the [`Ok`] value in the [`Result`]
    type Ok;

    fn report_as<C: Reportable>(self) -> Result<Self::Ok, Report<C>>;
}

impl<T, E: Context> ReportAs for Result<T, E> {
    type Ok = T;
    #[track_caller]
    fn report_as<C: Reportable>(self) -> Result<T, Report<C>> {
        self.into_report().change_context(C::value())
    }
}

pub trait IntoContext {
    fn into_ctx<C: Reportable>(self) -> Report<C>;
}

impl<T: Context> IntoContext for Report<T> {
    #[track_caller]
    fn into_ctx<C: Reportable>(self) -> Report<C> {
        self.change_context(C::value())
    }
}

pub trait ResultIntoContext {
    /// Type of the [`Ok`] value in the [`Result`]
    type Ok;

    fn into_ctx<C: Reportable>(self) -> Result<Self::Ok, Report<C>>;
}

impl<T, E: Context> ResultIntoContext for Result<T, Report<E>> {
    type Ok = T;
    #[track_caller]
    fn into_ctx<C: Reportable>(self) -> Result<T, Report<C>> {
        self.change_context(C::value())
    }
}

// TODO convert to #[derive(Reportable)]
#[macro_export]
macro_rules! reportable {
    ($context:ident) => {
        impl $crate::Reportable for $context {
            #[track_caller]
            fn report<C: $crate::Context>(ctx: C) -> $crate::Report<Self> {
                $crate::Report::new(ctx).change_context(Self)
            }

            // #[track_caller]
            // fn report_dyn_err(err: impl std::error::Error + 'static + Send + Sync) -> Report<Self> {
            //     $crate::Report::new($crate::BoxError::new(err)).change_context(Self)
            // }

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
            fn report_with_kv<A, C: $crate::Context>(
                ctx: C,
                key: &str,
                value: A,
            ) -> $crate::Report<Self>
            where
                A: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static,
            {
                use $crate::AttachExt;
                $crate::Report::new(ctx)
                    .change_context(Self)
                    .attach_kv(key, value)
            }
            #[track_caller]
            fn report_inner<C>(err: impl $crate::ToReport<C>) -> $crate::Report<Self>
            where
                C: $crate::Context,
            {
                err.to_report().change_context(Self)
            }
            fn value() -> Self {
                $context
            }
        }
    };
}

pub trait AttachExt {
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
    fn and_log(self, level: Level) -> Result<T, E>;
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
    fn and_log(self, level: Level) -> Result<T, E> {
        if let Err(err) = &self {
            match level {
                Level::TRACE => trace!(?err),
                Level::DEBUG => debug!(?err),
                Level::INFO => info!(?err),
                Level::WARN => warn!(?err),
                Level::ERROR => error!(?err),
            }
        }
        self
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
            #[track_caller]
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

pub trait ResultToReport<T> {
    /// Type of the [`Ok`] value in the [`Result`]
    type Ok;

    #[track_caller]
    fn to_report(self) -> Result<Self::Ok, Report<T>>;

    #[track_caller]
    fn change_context<C: Context>(self, context: C) -> Result<Self::Ok, Report<C>>
    where
        Self: Sized,
    {
        self.to_report().change_context(context)
    }
}

impl<T, E, C> ResultToReport<C> for Result<T, E>
where
    E: ToReport<C>,
{
    type Ok = T;

    #[track_caller]
    fn to_report(self) -> Result<T, Report<C>> {
        self.map_err(ToReport::to_report)
    }
}

/// Used to produce [`NotFound`] reports from an [`Option`]
pub trait OptionReport {
    type Some;
    fn ok_or_not_found(self) -> Result<Self::Some, Report<NotFound>>;
    fn ok_or_not_found_kv<A>(self, key: &str, value: A) -> Result<Self::Some, Report<NotFound>>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static;
    fn ok_or_not_found_field(self, field: &'static str) -> Result<Self::Some, Report<NotFound>>;
}

impl<T> OptionReport for Option<T> {
    type Some = T;

    fn ok_or_not_found(self) -> Result<T, Report<NotFound>> {
        self.ok_or_else(|| Report::new(NotFound))
    }

    fn ok_or_not_found_kv<A>(self, key: &str, value: A) -> Result<T, Report<NotFound>>
    where
        A: fmt::Display + fmt::Debug + Send + Sync + 'static,
    {
        self.ok_or_else(|| NotFound::with_kv(key, value))
    }

    fn ok_or_not_found_field(self, field: &'static str) -> Result<T, Report<NotFound>> {
        self.ok_or_else(|| NotFound::with_field(field))
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[derive(Debug, thiserror::Error)]
    #[error("MyError")]
    pub struct MyError;
    reportable!(MyError);

    #[derive(Debug, thiserror::Error)]
    pub enum Error {
        #[error("{0:?}")]
        Report(Report<MyError>),
    }

    from_report!({

        impl From<std::string::ParseError as ParseError>

        for Error::Report(MyError)
    });

    to_report!(impl ToReport<MyError> for Error::Report);

    #[test]
    fn report_as() {
        fn output() -> Result<usize, Report<MyError>> {
            "NaN".parse::<usize>().report_as()
        }

        let _ = output().unwrap_err();
    }
    #[test]
    fn reportable() {
        fn output() -> Result<usize, Report<MyError>> {
            "NaN".parse::<usize>().map_err(MyError::report)
        }

        let _ = output().unwrap_err();
    }
    #[test]
    fn box_reportable() {
        fn output() -> Result<usize, Box<dyn std::error::Error + Sync + Send>> {
            Ok("NaN".parse::<usize>().map_err(Box::new)?)
        }

        let _ = output()
            .map_err(BoxError::from)
            .change_context(MyError)
            .unwrap_err();
    }

    #[test]
    fn convresion_error() {
        fn output() -> Result<usize, Report<ConversionError>> {
            "NaN"
                .parse::<usize>()
                .map_err(ConversionError::from::<&str, usize>)
                .attach_printable(ParseError)
        }

        let _ = output().change_context(MyError).unwrap_err();
    }

    #[test]
    fn error_in_error_handling() {
        fn output() -> Result<usize, Report<ConversionError>> {
            "NaN"
                .parse::<usize>()
                .map_err(ConversionError::from::<&str, usize>)
                .map_err(|e| match "More NaN".parse::<u32>() {
                    Ok(attachment) => e.attach_printable(attachment),
                    Err(attachment_err) => e
                        .attach_printable(ParseError)
                        .attach_kv("val: \"More Nan\"", attachment_err),
                })
        }

        let _ = output().change_context(MyError).unwrap_err();
    }
    #[test]
    fn option_report() {
        assert!(None::<()>.ok_or_not_found().is_err());

        let id: u32 = 0xdeadbeef;
        assert!(None::<bool>.ok_or_not_found_kv("id", id).is_err());
        assert!(Some(true).ok_or_not_found_kv("id", id).unwrap());

        struct OptionField<'a> {
            name: Option<&'a str>,
        }

        let field_none = OptionField { name: None };
        assert!(field_none.name.ok_or_not_found_field("name").is_err());

        let field_some = OptionField {
            name: Some("biggy"),
        };
        assert_eq!(
            "biggy",
            field_some.name.ok_or_not_found_field("name").unwrap()
        );
    }

    #[test]
    fn into_ctx() {
        fn output() -> Result<usize, Report<MyError>> {
            "NaN"
                .parse::<usize>()
                .map_err(|e| ConversionError::from::<&str, usize>(e).into_ctx())
        }

        let _ = output().unwrap_err();
    }

    #[test]
    fn result_into_ctx() {
        fn output() -> Result<usize, Report<MyError>> {
            "NaN"
                .parse::<usize>()
                .map_err(ConversionError::from::<&str, usize>)
                .into_ctx()
        }

        assert!(output().is_err())
    }
}
