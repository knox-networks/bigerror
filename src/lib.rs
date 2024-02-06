use error_stack::fmt::ColorMode;
use std::fmt;
use tracing::{debug, error, info, trace, warn, Level};

pub use error_stack::{self, bail, Context, Report, ResultExt};
pub use thiserror;

pub mod attachment;
pub mod context;

pub use attachment::Field;

use attachment::{install_attachment_hooks, Debug, Display, Expectation, Index, KeyValue};
pub use context::*;

// TODO we'll have to do a builder pattern here at
// some point
pub fn init_colour() {
    Report::set_color_mode(ColorMode::Color);
    install_attachment_hooks();
}

pub fn init_emphasis() {
    Report::set_color_mode(ColorMode::Emphasis);
    install_attachment_hooks();
}

pub fn init_no_ansi() {
    Report::set_color_mode(ColorMode::None);
    install_attachment_hooks();
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
        A: Display;
    fn attach_dbg<A>(value: A) -> Report<Self>
    where
        A: Debug;
    fn with_kv<K, V>(key: K, value: V) -> Report<Self>
    where
        K: Display,
        V: Display;
    fn with_kv_dbg<K, V>(key: K, value: V) -> Report<Self>
    where
        K: Debug,
        V: Debug;
    fn with_field_status<K, S>(key: K, status: S) -> Report<Self>
    where
        K: Display,
        S: Display;

    fn value() -> Self;

    #[track_caller]
    fn expected_actual<A: attachment::Display>(expected: A, actual: A) -> Report<Self> {
        Self::attach(Expectation { expected, actual })
    }

    #[track_caller]
    fn with_variant<A: Display>(value: A) -> Report<Self> {
        Self::with_kv_dbg(attachment::Type::of::<A>(), value)
    }

    #[track_caller]
    fn with_variant_dbg<A: Debug>(value: A) -> Report<Self> {
        Self::with_kv_dbg(attachment::Type::of::<A>(), value)
    }

    #[track_caller]
    fn with_type<A>() -> Report<Self> {
        Self::attach(attachment::Type::of::<A>())
    }

    #[track_caller]
    fn with_type_status<A: Send + Sync + 'static>(status: impl Display) -> Report<Self> {
        Self::attach(Field::new(attachment::Type::of::<A>(), status))
    }
}

pub trait WithHeader: Default {
    fn header(msg: impl Into<Header>) -> Self;
    fn header_fmt(value: impl Display) -> Self {
        Self::header(format!("{value}"))
    }
}

/// Extends [`error_stack::IntoReport`] to allow an implicit `E -> Report<C>` inference
pub trait ReportAs<T> {
    fn report_as<C: Reportable>(self) -> Result<T, Report<C>>;
}

impl<T, E: Context> ReportAs<T> for Result<T, E> {
    #[track_caller]
    fn report_as<C: Reportable>(self) -> Result<T, Report<C>> {
        // TODO #[track_caller] on closure
        // https://github.com/rust-lang/rust/issues/87417
        // self.map_err(|e| Report::new(C::value()).attach_printable(e))
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(Report::new(C::value()).attach_printable(e)),
        }
    }
}

impl<T> ReportAs<T> for &'static str {
    #[track_caller]
    fn report_as<C: Reportable>(self) -> Result<T, Report<C>> {
        Err(Report::new(C::value()).attach_printable(self))
    }
}

impl<T> ReportAs<T> for String {
    #[track_caller]
    fn report_as<C: Reportable>(self) -> Result<T, Report<C>> {
        Err(Report::new(C::value()).attach_printable(self))
    }
}

pub trait IntoContext {
    fn into_ctx<C2: Reportable>(self) -> Report<C2>;
}

impl<C: Context> IntoContext for Report<C> {
    #[inline]
    #[track_caller]
    fn into_ctx<C2: Reportable>(self) -> Report<C2> {
        self.change_context(C2::value())
    }
}

pub trait ResultIntoContext: ResultExt {
    fn into_ctx<C2: Reportable>(self) -> Result<Self::Ok, Report<C2>>;
}

impl<T, C> ResultIntoContext for Result<T, Report<C>>
where
    C: Context,
{
    #[inline]
    #[track_caller]
    fn into_ctx<C2: Reportable>(self) -> Result<T, Report<C2>> {
        self.change_context(C2::value())
    }
}

impl<T> Reportable for T
where
    T: Sized + Context + Default + WithHeader,
{
    fn report<C: Context>(ctx: C) -> Report<Self> {
        Report::new(ctx).change_context(Self::default())
    }

    fn attach<A>(attachment: A) -> Report<Self>
    where
        A: Display,
    {
        Report::new(Self::default()).attach_printable(attachment)
    }

    fn attach_dbg<A>(attachment: A) -> Report<Self>
    where
        A: Debug,
    {
        Self::attach(format!("{attachment:?}"))
    }

    fn with_kv<K, V>(key: K, value: V) -> Report<Self>
    where
        K: Display,
        V: Display,
    {
        Self::attach(KeyValue(key, value))
    }

    fn with_kv_dbg<K, V>(key: K, value: V) -> Report<Self>
    where
        K: Debug,
        V: Debug,
    {
        Self::attach(KeyValue::dbg(key, value))
    }

    fn with_field_status<K, S>(key: K, status: S) -> Report<Self>
    where
        K: Display,
        S: Display,
    {
        Self::attach(Field::new(key, status))
    }

    fn value() -> Self {
        Self::default()
    }
}

// macro used to implement [`Reportable`] on zero-sized types
#[macro_export]
macro_rules! reportable {
    ($context:ident) => {
        impl $crate::Reportable for $context {
            #[track_caller]
            fn report<C: $crate::Context>(ctx: C) -> $crate::Report<Self> {
                $crate::Report::new(ctx).change_context(Self)
            }

            #[track_caller]
            fn attach<A>(value: A) -> $crate::Report<Self>
            where
                A: $crate::attachment::Display,
            {
                $crate::Report::new(Self).attach_printable(value)
            }

            #[track_caller]
            fn attach_dbg<A>(value: A) -> $crate::Report<Self>
            where
                A: $crate::attachment::Debug,
            {
                $crate::Report::new(Self).attach_printable(format!("{value:?}"))
            }

            #[track_caller]
            fn with_kv<K, V>(key: K, value: V) -> $crate::Report<Self>
            where
                K: $crate::attachment::Display,
                V: $crate::attachment::Display,
            {
                use $crate::AttachExt;
                $crate::Report::new(Self).attach_kv(key, value)
            }
            #[track_caller]
            fn with_kv_dbg<K, V>(key: K, value: V) -> $crate::Report<Self>
            where
                K: $crate::attachment::Debug,
                V: $crate::attachment::Debug,
            {
                use $crate::AttachExt;
                $crate::Report::new(Self).attach_kv_dbg(key, value)
            }
            #[track_caller]
            fn with_field_status<K, S>(key: K, status: S) -> $crate::Report<Self>
            where
                K: $crate::attachment::Display,
                S: $crate::attachment::Display,
            {
                $crate::Report::new(Self)
                    .attach_printable($crate::attachment::Field::new(key, status))
            }

            fn value() -> Self {
                $context
            }
        }
    };
}

pub trait AttachExt {
    fn attach_kv<K, V>(self, key: K, value: V) -> Self
    where
        K: Display,
        V: Display;
    fn attach_kv_dbg<K, V>(self, key: K, value: V) -> Self
    where
        K: Debug,
        V: Debug;

    fn attach_field_status<S>(self, name: &'static str, status: S) -> Self
    where
        S: Display;
    fn attach_dbg<A>(self, value: A) -> Self
    where
        A: Debug;
}

impl<C> AttachExt for Report<C> {
    #[track_caller]
    fn attach_kv<K, V>(self, key: K, value: V) -> Self
    where
        K: Display,
        V: Display,
    {
        self.attach_printable(KeyValue(key, value))
    }

    #[track_caller]
    fn attach_kv_dbg<K, V>(self, key: K, value: V) -> Self
    where
        K: Debug,
        V: Debug,
    {
        self.attach_printable(KeyValue::dbg(key, value))
    }

    #[inline]
    #[track_caller]
    fn attach_field_status<S>(self, name: &'static str, status: S) -> Self
    where
        S: Display,
    {
        self.attach_printable(Field::new(name, status))
    }

    #[track_caller]
    fn attach_dbg<A>(self, value: A) -> Self
    where
        A: Debug,
    {
        self.attach_printable(format!("{value:?}"))
    }
}

impl<T, C> AttachExt for Result<T, Report<C>> {
    #[track_caller]
    fn attach_kv<K, V>(self, key: K, value: V) -> Self
    where
        K: Display,
        V: Display,
    {
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.attach_printable(KeyValue(key, value))),
        }
    }

    #[inline]
    #[track_caller]
    fn attach_kv_dbg<K, V>(self, key: K, value: V) -> Self
    where
        K: Debug,
        V: Debug,
    {
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.attach_printable(KeyValue::dbg(key, value))),
        }
    }

    #[inline]
    #[track_caller]
    fn attach_field_status<S>(self, name: &'static str, status: S) -> Self
    where
        S: Display,
    {
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.attach_printable(Field::new(name, status))),
        }
    }

    #[inline]
    #[track_caller]
    fn attach_dbg<A>(self, value: A) -> Self
    where
        A: Debug,
    {
        match self {
            Ok(ok) => Ok(ok),
            Err(report) => Err(report.attach_printable(format!("{value:?}"))),
        }
    }
}

pub trait ResultAttachExt: ResultExt {
    fn attach_kv<K, V>(self, key: K, value: V) -> Result<Self::Ok, Report<Self::Context>>
    where
        K: Display,
        V: Display;
    fn attach_kv_dbg<K, V>(self, key: K, value: V) -> Result<Self::Ok, Report<Self::Context>>
    where
        K: Debug,
        V: Debug;

    fn attach_field_status<S>(
        self,
        name: &'static str,
        status: S,
    ) -> Result<Self::Ok, Report<Self::Context>>
    where
        S: Display;
    fn attach_dbg<A>(self, value: A) -> Result<Self::Ok, Report<Self::Context>>
    where
        A: Debug;
}

impl<T, C> ResultAttachExt for Result<T, C>
where
    C: Context,
{
    #[inline]
    #[track_caller]
    fn attach_kv<K, V>(self, key: K, value: V) -> Result<T, Report<C>>
    where
        K: Display,
        V: Display,
    {
        match self {
            Ok(ok) => Ok(ok),
            Err(e) => Err(Report::from(e).attach_printable(KeyValue(key, value))),
        }
    }
    #[inline]
    #[track_caller]
    fn attach_kv_dbg<K, V>(self, key: K, value: V) -> Result<T, Report<C>>
    where
        K: Debug,
        V: Debug,
    {
        match self {
            Ok(ok) => Ok(ok),
            Err(e) => Err(Report::from(e).attach_printable(KeyValue::dbg(key, value))),
        }
    }

    #[inline]
    #[track_caller]
    fn attach_field_status<S>(self, name: &'static str, status: S) -> Result<T, Report<C>>
    where
        S: Display,
    {
        match self {
            Ok(ok) => Ok(ok),
            Err(e) => Err(Report::from(e).attach_printable(Field::new(name, status))),
        }
    }
    #[inline]
    #[track_caller]
    fn attach_dbg<A>(self, value: A) -> Result<T, Report<C>>
    where
        A: Debug,
    {
        match self {
            Ok(ok) => Ok(ok),
            Err(e) => Err(Report::from(e).attach_printable(format!("{value:?}"))),
        }
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
}

impl<T, E, C> ResultToReport<C> for Result<T, E>
where
    E: ToReport<C>,
{
    type Ok = T;

    #[inline]
    #[track_caller]
    fn to_report(self) -> Result<T, Report<C>> {
        self.map_err(ToReport::to_report)
    }
}

impl<C> ToReport<C> for Report<C> {
    fn to_report(self) -> Report<C> {
        self
    }
}

/// Used to produce [`NotFound`] reports from an [`Option`]
pub trait OptionReport {
    type Some;
    fn ok_or_not_found(self) -> Result<Self::Some, Report<NotFound>>;
    fn ok_or_not_found_kv<K, V>(self, key: K, value: V) -> Result<Self::Some, Report<NotFound>>
    where
        K: Display,
        V: Display;
    fn ok_or_not_found_field(self, field: &'static str) -> Result<Self::Some, Report<NotFound>>;
    fn ok_or_not_found_by<K: Display>(self, key: K) -> Result<Self::Some, Report<NotFound>>;
}

impl<T> OptionReport for Option<T> {
    type Some = T;

    #[track_caller]
    fn ok_or_not_found(self) -> Result<T, Report<NotFound>> {
        // TODO #[track_caller] on closure
        // https://github.com/rust-lang/rust/issues/87417
        // self.ok_or_else(|| Report::new(NotFound))
        match self {
            Some(v) => Ok(v),
            None => Err(NotFound::with_type::<T>()),
        }
    }

    #[track_caller]
    fn ok_or_not_found_kv<K, V>(self, key: K, value: V) -> Result<T, Report<NotFound>>
    where
        K: Display,
        V: Display,
    {
        match self {
            Some(v) => Ok(v),
            None => Err(NotFound::with_kv(key, value)),
        }
    }

    #[track_caller]
    fn ok_or_not_found_field(self, field: &'static str) -> Result<T, Report<NotFound>> {
        match self {
            Some(v) => Ok(v),
            None => Err(NotFound::with_field(field)),
        }
    }

    #[track_caller]
    fn ok_or_not_found_by<K: Display>(self, key: K) -> Result<T, Report<NotFound>> {
        match self {
            Some(v) => Ok(v),
            None => Err(NotFound::with_kv(Index(key), attachment::Type::of::<K>())),
        }
    }
}

#[cfg(test)]
mod test {

    use crate::attachment::Invalid;

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
    #[should_panic]
    fn error_in_error_handling() {
        crate::init_colour();

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

        let _ = output().change_context(MyError).unwrap();
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

    #[test]
    #[should_panic]
    fn with_type_status() {
        fn try_even(num: usize) -> Result<(), Report<MyError>> {
            if num % 2 != 0 {
                return Err(InvalidInput::with_type_status::<usize>(Invalid).into_ctx());
            }
            Ok(())
        }

        try_even(3).unwrap();
    }
}
