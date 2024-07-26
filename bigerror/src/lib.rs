#![cfg_attr(
    not(miri),
    doc(test(attr(deny(warnings, clippy::pedantic, clippy::nursery))))
)]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(
    nightly,
    feature(error_generic_member_access),
    allow(clippy::incompatible_msrv)
)]
#![cfg_attr(all(doc, nightly), feature(doc_auto_cfg))]
#![cfg_attr(all(nightly, feature = "std"), feature(backtrace_frames))]
#![allow(unsafe_code)]
// This is an error handling library producing Results, not Errors
#![allow(clippy::missing_errors_doc)]

extern crate alloc;

pub mod attachment;
pub mod context;
pub mod error_stack;
pub mod fmt;
#[cfg(feature = "grpc")]
pub mod grpc;

pub use attachment::{Expectation, Field, Index, KeyValue, Type};
pub use context::*;

use crate::fmt::ColorMode;
use attachment::{Dbg, Debug, Display};
use tracing::{debug, error, info, trace, warn, Level};

pub use bigerror_derive::ThinContext;
pub use error_stack::{
    iter, AttachmentKind, BigResult, Context, Frame, FrameKind, FutureExt, IntoReportCompat,
    Report, ResultExt,
};
pub use thiserror;

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

/// `ThinContext` behaves as an `error_stack::ContextExt`
/// ideally used for zero sized errors or ones that hold a `'static` ref/value
pub trait ThinContext
where
    Self: Sized + Context,
{
    fn value() -> Self;
    fn from_ctx<C: Context>(ctx: C) -> Report<Self> {
        Report::new(ctx).change_context(Self::value())
    }
    // TODO
    // fn report_dyn_err(err: impl std::error::Error + 'static + Send + Sync)
    // -> Report<Self>;
    #[track_caller]
    fn attach<A>(value: A) -> Report<Self>
    where
        A: Display,
    {
        Report::new(Self::value()).attach_printable(value)
    }
    #[track_caller]
    fn attach_dbg<A>(value: A) -> Report<Self>
    where
        A: Debug,
    {
        Self::attach(Dbg(value))
    }
    #[track_caller]
    fn with_kv<K, V>(key: K, value: V) -> Report<Self>
    where
        K: Display,
        V: Display,
    {
        Self::attach(KeyValue(key, value))
    }
    #[track_caller]
    fn with_kv_dbg<K, V>(key: K, value: V) -> Report<Self>
    where
        K: Display,
        V: Debug,
    {
        Self::attach(KeyValue::dbg(key, value))
    }
    #[track_caller]
    fn with_field_status<S: Display>(key: &'static str, status: S) -> Report<Self> {
        Self::attach(Field::new(key, status))
    }

    #[track_caller]
    fn expected_actual<A: attachment::Display>(expected: A, actual: A) -> Report<Self> {
        Self::attach(Expectation { expected, actual })
    }

    #[track_caller]
    fn with_variant<A: Display>(value: A) -> Report<Self> {
        Self::with_kv(attachment::Type::of::<A>(), value)
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

/// Extends [`error_stack::IntoReport`] to allow an implicit `E -> Report<C>` inference
pub trait ReportAs<T> {
    fn report_as<C: ThinContext>(self) -> Result<T, Report<C>>;
}

impl<T, E: Context> ReportAs<T> for Result<T, E> {
    #[inline]
    #[track_caller]
    fn report_as<C: ThinContext>(self) -> Result<T, Report<C>> {
        // TODO #[track_caller] on closure
        // https://github.com/rust-lang/rust/issues/87417
        // self.map_err(|e| Report::new(C::value()).attach_printable(e))
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(Report::new(C::value()).attach_printable(e)),
        }
    }
}

pub trait IntoContext {
    fn into_ctx<C2: ThinContext>(self) -> Report<C2>;
}

impl<C: Context> IntoContext for Report<C> {
    #[inline]
    #[track_caller]
    fn into_ctx<C2: ThinContext>(self) -> Report<C2> {
        self.change_context(C2::value())
    }
}

pub trait ResultIntoContext: ResultExt {
    fn into_ctx<C2: ThinContext>(self) -> Result<Self::Ok, Report<C2>>;
    // Result::and_then
    fn and_then_ctx<U, F, C2>(self, op: F) -> Result<U, Report<C2>>
    where
        C2: ThinContext,
        F: FnOnce(Self::Ok) -> Result<U, Report<C2>>;
    // Result::map
    fn map_ctx<U, F, C2>(self, op: F) -> Result<U, Report<C2>>
    where
        C2: ThinContext,
        F: FnOnce(Self::Ok) -> U;
}

impl<T, C> ResultIntoContext for Result<T, Report<C>>
where
    C: Context,
{
    #[inline]
    #[track_caller]
    fn into_ctx<C2: ThinContext>(self) -> Result<T, Report<C2>> {
        self.change_context(C2::value())
    }

    #[inline]
    #[track_caller]
    fn and_then_ctx<U, F, C2>(self, op: F) -> Result<U, Report<C2>>
    where
        C2: ThinContext,
        F: FnOnce(T) -> Result<U, Report<C2>>,
    {
        match self {
            Ok(t) => op(t),
            Err(ctx) => Err(ctx.change_context(C2::value())),
        }
    }

    #[inline]
    #[track_caller]
    fn map_ctx<U, F, C2>(self, op: F) -> Result<U, Report<C2>>
    where
        C2: ThinContext,
        F: FnOnce(T) -> U,
    {
        match self {
            Ok(t) => Ok(op(t)),
            Err(ctx) => Err(ctx.change_context(C2::value())),
        }
    }
}

// TODO convert to #[derive(ThinContext)]
#[macro_export]
macro_rules! reportable {
    ($context:ident) => {
        impl $crate::ThinContext for $context {
            fn value() -> Self {
                $context
            }
        }
    };
}

pub trait AttachExt {
    #[must_use]
    fn attach_kv<K, V>(self, key: K, value: V) -> Self
    where
        K: Display,
        V: Display;
    fn attach_kv_dbg<K, V>(self, key: K, value: V) -> Self
    where
        K: Display,
        V: Debug;

    fn attach_field_status<S>(self, name: &'static str, status: S) -> Self
    where
        S: Display;
    fn attach_dbg<A>(self, value: A) -> Self
    where
        A: Debug;
    fn attach_variant<A>(self, value: A) -> Self
    where
        Self: Sized,
        A: Display,
    {
        self.attach_kv(attachment::Type::of::<A>(), value)
    }
}

impl<C> AttachExt for Report<C> {
    #[inline]
    #[track_caller]
    fn attach_kv<K, V>(self, key: K, value: V) -> Self
    where
        K: Display,
        V: Display,
    {
        self.attach_printable(KeyValue(key, value))
    }

    #[inline]
    #[track_caller]
    fn attach_kv_dbg<K, V>(self, key: K, value: V) -> Self
    where
        K: Display,
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

    #[inline]
    #[track_caller]
    fn attach_dbg<A>(self, value: A) -> Self
    where
        A: Debug,
    {
        self.attach_printable(Dbg(value))
    }
}

impl<T, C> AttachExt for Result<T, Report<C>> {
    #[inline]
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
        K: Display,
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
            Err(report) => Err(report.attach_printable(Dbg(value))),
        }
    }
}

// intended to be a quick passthrough for propagating errors to the message log
// in a functional matter
pub trait LogError<T, E>
where
    E: std::fmt::Debug,
{
    // swallows and logs error
    fn log_err(self);
    // swallows and logs error with attachment
    fn log_attached_err<A>(self, attachment: A)
    where
        A: std::fmt::Debug + Send + Sync + 'static;
    // logs error and forwards
    fn and_log_err(self) -> Result<T, E>;
    fn and_log(self, level: Level) -> Result<T, E>;
    // logs error and forwards with attachment
    fn and_attached_err<A>(self, attachment: A) -> Result<T, E>
    where
        A: std::fmt::Debug + Send + Sync + 'static;
    fn on_err(self, op: impl FnOnce()) -> Result<T, E>;
}

impl<T, E> LogError<T, E> for Result<T, E>
where
    E: std::fmt::Debug,
{
    #[inline]
    #[track_caller]
    fn log_err(self) {
        if let Err(e) = self {
            error!(message = ?e);
        }
    }

    #[inline]
    #[track_caller]
    fn log_attached_err<A>(self, attachment: A)
    where
        A: std::fmt::Debug + Send + Sync + 'static,
    {
        if let Err(e) = self {
            error!(err = ?e, "{attachment:?}");
        }
    }
    #[inline]
    #[track_caller]
    fn and_log(self, level: Level) -> Self {
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

    #[inline]
    #[track_caller]
    fn and_log_err(self) -> Self {
        if let Err(e) = &self {
            error!(message = ?e);
        }
        self
    }

    #[inline]
    #[track_caller]
    fn and_attached_err<A>(self, attachment: A) -> Self
    where
        A: std::fmt::Debug + Send + Sync + 'static,
    {
        if let Err(e) = &self {
            error!(err = ?e, "{attachment:?}");
        }
        self
    }

    #[inline]
    #[track_caller]
    fn on_err(self, op: impl FnOnce()) -> Self {
        op();
        self
    }
}

pub trait ClearResult<T, E> {
    #[allow(clippy::result_unit_err)]
    fn clear_err(self) -> Result<T, ()>;

    fn clear_ok(self) -> Result<(), E>;
}

impl<T, E> ClearResult<T, E> for Result<T, E> {
    fn clear_err(self) -> Result<T, ()> {
        self.map_err(|_| ())
    }

    fn clear_ok(self) -> Result<(), E> {
        self.map(|_| ())
    }
}

/// Used to produce [`NotFound`] reports from an [`Option`]
pub trait OptionReport<T>
where
    Self: Sized,
{
    fn expect_or(self) -> Result<T, Report<NotFound>>;
    fn expect_kv<K, V>(self, key: K, value: V) -> Result<T, Report<NotFound>>
    where
        K: Display,
        V: Display;
    fn expect_field(self, field: &'static str) -> Result<T, Report<NotFound>>;

    #[inline]
    #[track_caller]
    fn expect_kv_dbg<K, V>(self, key: K, value: V) -> Result<T, Report<NotFound>>
    where
        K: Display,
        V: Debug,
    {
        self.expect_kv(key, Dbg(value))
    }

    #[inline]
    #[track_caller]
    fn expect_by<K: Display>(self, key: K) -> Result<T, Report<NotFound>> {
        self.expect_kv(Index(key), ty!(T))
    }
}

impl<T> OptionReport<T> for Option<T> {
    #[inline]
    #[track_caller]
    fn expect_or(self) -> Result<T, Report<NotFound>> {
        // TODO #[track_caller] on closure
        // https://github.com/rust-lang/rust/issues/87417
        // self.ok_or_else(|| Report::new(NotFound))
        match self {
            Some(v) => Ok(v),
            None => Err(NotFound::with_type::<T>()),
        }
    }

    #[inline]
    #[track_caller]
    fn expect_kv<K, V>(self, key: K, value: V) -> Result<T, Report<NotFound>>
    where
        K: Display,
        V: Display,
    {
        match self {
            Some(v) => Ok(v),
            None => Err(NotFound::with_kv(key, value)),
        }
    }

    #[inline]
    #[track_caller]
    fn expect_field(self, field: &'static str) -> Result<T, Report<NotFound>> {
        match self {
            Some(v) => Ok(v),
            None => Err(NotFound::with_field(field)),
        }
    }
}

#[macro_export]
macro_rules! __field {
    // === exits ===
    // handle optional method calls: self.x.as_ref()
    ($fn:path, @[$($rf:tt)*] @[$($pre:expr)+], % $field_method:ident() $(.$method:ident())* ) => {
        $fn($($rf)*$($pre.)+ $field_method() $(.$method())*, stringify!($field_method))
    };
    // handle optional method calls: self.x.as_ref()
    ($fn:path, @[$($rf:tt)*] @[$($pre:expr)+], $field:ident $(.$method:ident())* ) => {
        $fn($($rf)*$($pre.)+ $field $(.$method())*, stringify!($field))
    };
    ($fn:path, @[$($rf:tt)*] @[$body:expr], $(.$method:ident())* ) => {
        $fn($($rf)*$body$(.$method())*, stringify!($body))
    };

    // === much TTs ===
    ($fn:path, @[$($rf:tt)*] @[$($pre:expr)+], $field:ident . $($rest:tt)+) => {
        $crate::__field!($fn, $($rf:tt)* @[$($pre)+ $field], $($rest)+)
    };

    // === entries ===
    ($fn:path | &$body:ident . $($rest:tt)+) => {
        $crate::__field!($fn, @[&] @[$body], $($rest)+)
    };
    ($fn:path | $body:ident . $($rest:tt)+) => {
        $crate::__field!($fn, @[] @[$body], $($rest)+)
    };

    // simple cases
    ($fn:path | &$field:ident) => {
        $fn(&$field, stringify!($field))
    };
    ($fn:path | $field:ident) => {
        $fn($field, stringify!($field))
    };
}

#[macro_export]
macro_rules! expect_field {
    ($($body:tt)+) => {
        $crate::__field!(
         $crate::OptionReport::expect_field |
            $($body)+
        )
    };
}

#[cfg(test)]
mod test {

    use crate::attachment::Invalid;

    use super::*;

    #[derive(Debug, thiserror::Error)]
    #[error("MyError")]
    pub struct MyError;
    reportable!(MyError);

    #[derive(Default)]
    struct MyStruct {
        my_field: Option<()>,
        _string: String,
    }

    impl MyStruct {
        fn __field<T>(_t: T, _field: &'static str) {}
        fn my_field(&self) -> Option<()> {
            self.my_field
        }
    }

    macro_rules! assert_err {
        ($result:expr $(,)?) => {
            let result = $result;
            assert!(result.is_err(), "{:?}", result.unwrap());
            if option_env!("PRINTERR").is_some() {
                crate::init_colour();
                println!("\n{:?}", result.unwrap_err());
            }
        };
        ($result:expr, $($arg:tt)+) => {
            let result = $result;
            assert!(result.is_err(), $($arg)+);
            if option_env!("PRINTERR").is_some() {
                crate::init_colour();
                println!("\n{:?}", result.unwrap_err());
            }
        };
    }

    #[test]
    fn implicit_from() {
        fn output() -> Result<usize, Report<MyError>> {
            Ok("NaN".parse::<usize>()?)
        }

        assert_err!(output());
    }
    #[test]
    fn explicit_from() {
        fn output() -> Result<usize, Report<MyError>> {
            "NaN".parse::<usize>().map_err(MyError::from_ctx)
        }

        assert_err!(output());
    }
    #[test]
    fn box_reportable() {
        fn output() -> Result<usize, Box<dyn std::error::Error + Sync + Send>> {
            Ok("NaN".parse::<usize>().map_err(Box::new)?)
        }

        assert_err!(output().map_err(BoxError::from).change_context(MyError));
    }

    #[test]
    fn result_report_macro() {
        fn output() -> Result<usize, Report<ParseError>> {
            report!("NaN".parse::<usize>()).into_ctx()
        }

        assert_err!(output().change_context(MyError));
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
                        .attach_kv("\"More Nan\"", attachment_err),
                })
        }

        assert_err!(output().change_context(MyError));
    }
    #[test]
    fn option_report() {
        assert_err!(None::<&str>
            .expect_or()
            .attach_printable(format!("{:?}", (true, "I wish it wasn't true"))));

        let id: u32 = 0xdead_beef;
        assert_err!(None::<bool>.expect_kv("id", id));
        assert!(Some(true).expect_kv("id", id).unwrap());

        struct OptionField<'a> {
            name: Option<&'a str>,
        }

        let field_none = OptionField { name: None };
        assert_err!(field_none.name.expect_field("name"));

        let field_some = OptionField {
            name: Some("biggy"),
        };
        assert_eq!("biggy", field_some.name.expect_field("name").unwrap());
    }

    #[test]
    fn into_ctx() {
        fn output() -> Result<usize, Report<MyError>> {
            "NaN"
                .parse::<usize>()
                .map_err(|e| ConversionError::from::<&str, usize>(e).into_ctx())
        }

        assert_err!(output());
    }

    #[test]
    fn result_into_ctx() {
        fn output() -> Result<usize, Report<MyError>> {
            "NaN"
                .parse::<usize>()
                .map_err(ConversionError::from::<&str, usize>)
                .into_ctx()
        }

        assert_err!(output());
    }

    #[test]
    fn with_type_status() {
        fn try_even(num: usize) -> Result<(), Report<MyError>> {
            if num % 2 != 0 {
                return Err(InvalidInput::with_type_status::<usize>(Invalid).into_ctx());
            }
            Ok(())
        }

        let my_input = try_even(3);
        assert_err!(my_input);
    }

    #[test]
    fn expect_field() {
        let my_struct = MyStruct::default();

        let my_field = expect_field!(my_struct.my_field);
        assert_err!(my_field);
        // from field method
        let my_field = expect_field!(my_struct.%my_field());
        assert_err!(my_field);
    }

    // this is meant to be a compile time test of the `__field!` macro
    fn __field() {
        let my_struct = MyStruct::default();
        __field!(MyStruct::__field::<&str> | &my_struct._string);
    }

    #[test]
    fn expectation() {
        let my_struct = MyStruct::default();
        let my_field = my_struct
            .my_field
            .ok_or_else(|| InvalidInput::expected_actual("Some", "None"));

        assert_err!(my_field);
    }

    #[test]
    fn attach_variant() {
        fn compare(mine: usize, other: usize) -> Result<(), Report<MyError>> {
            if other != mine {
                bail!(InvalidInput::attach("expected my number!")
                    .attach_variant(other)
                    .into_ctx());
            }
            Ok(())
        }
        let my_number = 2;
        let other_number = 3;
        assert_err!(compare(my_number, other_number));
    }

    // should behave the same as `test::attach_variant`
    // but displays lazy allocation of attachment
    #[test]
    fn attach_kv_macro() {
        fn compare(mine: usize, other: usize) -> Result<(), Report<MyError>> {
            if other != mine {
                return Err(InvalidInput::attach("expected my number!"))
                    .attach_printable_lazy(|| kv!(ty: other)) // <usize>: 3
                    .into_ctx();
            }
            Ok(())
        }
        let my_number = 2;
        let other_number = 3;
        assert_err!(compare(my_number, other_number));
    }

    #[test]
    fn expect_by() {
        let arr = ["a", "b"];
        let get_oob = arr.get(2).expect_by(2);
        assert_err!(get_oob);
    }
}
