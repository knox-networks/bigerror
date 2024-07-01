use std::{fmt::Display, str::FromStr};

use crate::{iter::Frames, AttachmentKind, Context, FrameKind, Report};
use tonic::{Code, Status};
use tonic_types::{ErrorDetails, StatusExt};

use crate::{attachment, AttachExt, ConversionError, OptionReport, ParseError, ReportAs};

/// Dentotes the start of a `google.rpc.DebugInfo` message start
/// https://github.com/googleapis/googleapis/blob/f36c650/google/rpc/error_details.proto#L97-L103
pub const DEBUG_INFO: &str = "details:";
/// Denotes the start of a [`tonic::Status`]
pub const GRPC_ERROR: &str = "GrpcError";

pub trait ErrorStatus<T> {
    fn map_err_to_status(self, op: impl FnOnce(String) -> Status) -> Result<T, Status>;
}

impl<T, E> ErrorStatus<T> for Result<T, E>
where
    E: Display,
{
    fn map_err_to_status(self, op: impl FnOnce(String) -> Status) -> Result<T, Status> {
        match self {
            Ok(t) => Ok(t),
            Err(e) => Err(op(format!("{e}"))),
        }
    }
}

#[macro_export]
macro_rules! required_field {
    ($($body:tt)+) => {
        bigerror::__field!(
            $crate::grpc::missing_field |
            $($body)+
        )
    };
}

#[macro_export]
macro_rules! try_field {
    (Report<$to:ty>,  $($body:tt)+) => {
        bigerror::__field!(
            $crate::grpc::try_report_field::<_, $to> |
            $($body)+
        )
    };
    ($to:ty:  $($body:tt)+) => {
        bigerror::__field!(
            $crate::grpc::try_report_field::<_, $to> |
            $($body)+
        )
    };
    ($to:ty,  $($body:tt)+) => {
        bigerror::__field!(
            $crate::grpc::try_field::<_, $to> |
            $($body)+
        )
    };
}

// currently proto3 is only able to create optional fields from non-scalar values
#[macro_export]
macro_rules! dyn_field {
    ($($body:tt)+) => {
        {
            use $crate::ResultIntoContext;
            $crate::expect_field!($($body)+).and_then_ctx(|v| v.try_into())
        }
    };
}

#[macro_export]
macro_rules! dyn_field_status {
    ($($body:tt)+) => {
        {
            use $crate::grpc::ReportStatus;
            $crate::dyn_field!($($body)+).to_status(tonic::Code::InvalidArgument)
        }
    };
}

#[inline]
#[track_caller]
pub fn parse_field<U: FromStr>(value: &str, name: &'static str) -> Result<U, Status>
where
    U::Err: Context,
{
    U::from_str(value)
        .report_as::<ParseError>()
        .attach_field_status(name, attachment::Invalid)
        .to_status(Code::InvalidArgument)
}

#[inline]
#[track_caller]
pub fn parse_report_field<U: FromStr>(value: &str, name: &'static str) -> Result<U, Status>
where
    U::Err: AttachExt + ReportStatus,
{
    match U::from_str(value) {
        Err(e) => Err(e
            .attach_field_status(name, attachment::Invalid)
            .to_status(Code::InvalidArgument)),
        Ok(v) => Ok(v),
    }
}

#[inline]
#[track_caller]
pub fn try_field<T, U: TryFrom<T>>(value: T, name: &'static str) -> Result<U, Status>
where
    U: Send + Sync,
    U::Error: Context,
{
    match U::try_from(value) {
        Err(e) => Err(ConversionError::from::<T, U>(e)
            .attach_field_status(name, attachment::Invalid)
            .to_status(Code::InvalidArgument)),
        Ok(v) => Ok(v),
    }
}

#[inline]
#[track_caller]
pub fn try_report_field<T, U: TryFrom<T>>(value: T, name: &'static str) -> Result<U, Status>
where
    U: Send + Sync,
    U::Error: AttachExt + ReportStatus,
{
    match U::try_from(value) {
        Err(e) => Err(e
            .attach_field_status(name, attachment::Invalid)
            .to_status(Code::InvalidArgument)),
        Ok(v) => Ok(v),
    }
}

#[inline]
#[track_caller]
pub fn missing_field<T>(value: Option<T>, name: &'static str) -> Result<T, Status> {
    value.expect_field(name).to_status(Code::InvalidArgument)
}

pub trait ReportStatus {
    fn to_status(self, code: Code) -> Status;
    fn to_msg_status(self, code: Code, msg: impl Into<String>) -> Status;
}

impl<C> ReportStatus for Report<C> {
    #[inline]
    #[track_caller]
    fn to_status(self, code: Code) -> Status {
        let msg = self.to_string();
        self.to_msg_status(code, msg)
    }

    #[inline]
    #[track_caller]
    fn to_msg_status(self, code: Code, msg: impl Into<String>) -> Status {
        tracing::error!(err = ?self);
        status_with_details(self, code, msg)
    }
}

pub trait ResultReportStatus<T, C> {
    fn to_status(self, code: Code) -> Result<T, Status>;
    fn to_msg_status(self, code: Code, msg: impl Into<String>) -> Result<T, Status>;
}

impl<T, C> ResultReportStatus<T, C> for Result<T, Report<C>> {
    #[inline]
    #[track_caller]
    fn to_status(self, code: tonic::Code) -> Result<T, Status> {
        self.map_err(|report| report.to_status(code))
    }

    #[inline]
    #[track_caller]
    fn to_msg_status(self, code: tonic::Code, msg: impl Into<String>) -> Result<T, Status> {
        self.map_err(|report| report.to_msg_status(code, msg))
    }
}

fn status_with_details<C>(report: Report<C>, code: Code, msg: impl Into<String>) -> Status {
    let ctx = std::any::type_name::<C>();
    let entries = extract_entries(&report);
    let error_details = ErrorDetails::with_debug_info(entries, ctx);
    Status::with_error_details(code, msg, error_details)
}

// maintain simple wrapper type while the stack trace API is unstable
fn extract_entries<C>(report: &Report<C>) -> Vec<String> {
    frames_to_entries(report.frames())
}

fn status_to_debug_entries(status: &Status) -> Vec<String> {
    let mut entries = Vec::new();
    entries.push(GRPC_ERROR.to_string());
    entries.push(format!("code: {:?}", status.code()));
    entries.push(format!("message: {}", status.message()));
    if let Some(debug_info) = status.get_details_debug_info() {
        entries.push(DEBUG_INFO.to_string());
        for entry in debug_info.stack_entries {
            entries.push(entry);
        }
        if debug_info.detail.is_empty() {
            entries.push(format!("debug_detail: {}", debug_info.detail));
        }
    }
    entries
}

// skips any Frames that cannot implement Debug or Display
fn frames_to_entries(frames: Frames) -> Vec<String> {
    let mut attachment_idx = 0;
    let mut entries = Vec::new();

    for frame in frames {
        if let Some(status) = frame.downcast_ref::<Status>() {
            let status_entries = status_to_debug_entries(status);
            for entry in status_entries {
                entries.push(entry);
            }
            continue;
        }
        match frame.kind() {
            // if we're at the bottom of the stack trace provide a "type_name: display" message
            FrameKind::Context(context) if frame.sources().is_empty() => {
                // strip all characters after a non-alpha match unil `std::any::type_name_of_val`
                // is stabilized
                // TODO https://doc.rust-lang.org/std/any/fn.type_name_of_val.html
                let type_name: String = format!("{context:?}")
                    .chars()
                    .take_while(|c| c.is_alphabetic())
                    .collect();
                // `context` is a `dyn error_stack::Context: Debug + Display + 'static` so we need
                // to compare the `Display` of our context and see if the `Debug` and `Display`
                // impls differ, otherwise we'd be getting stack entries such as:
                // > `"NotFound: NotFound"`
                let context = format!("{context}");
                if type_name == context {
                    entries.push(type_name.to_string());
                } else {
                    entries.push(format!("{type_name}: {context}"));
                }
            }
            FrameKind::Context(context) => {
                entries.push(format!("{context}"));
            }
            FrameKind::Attachment(AttachmentKind::Printable(attachment)) => {
                entries.push(format!("{attachment_idx}: {attachment}"));
                // if we've reached the end of the attachment stack reset attachment_idx
                if let Some(FrameKind::Context(_)) = frame.sources().first().map(|f| f.kind()) {
                    attachment_idx = 0;
                } else {
                    attachment_idx += 1;
                }
            }
            // ignore opaque attachment types that do not have a Display/Debug impl
            FrameKind::Attachment(_) => (),
        }
    }
    entries
}
