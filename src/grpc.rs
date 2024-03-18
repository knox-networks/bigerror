use error_stack::{iter::Frames, AttachmentKind, FrameKind, Report};
use tonic::{Code, Status};
use tonic_types::{ErrorDetails, StatusExt};

/// Dentotes the start of a `google.rpc.DebugInfo` message start
/// https://github.com/googleapis/googleapis/blob/f36c650/google/rpc/error_details.proto#L97-L103
pub const DEBUG_INFO: &str = "details:";
/// Denotes the start of a [`tonic::Status`]
pub const GRPC_ERROR: &str = "GrpcError";

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

// maintain simple wrapper type while the stack trace API is
// unstable
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
                // strip all characters after a non-alpha match
                // unil `std::any::type_name_of_val` is stabilized
                // TODO https://doc.rust-lang.org/std/any/fn.type_name_of_val.html
                let type_name: String = format!("{context:?}")
                    .chars()
                    .take_while(|c| c.is_alphabetic())
                    .collect();
                // `context` is a `dyn error_stack::Context: Debug + Display + 'static`
                // so we need to compare the `Display` of our context and see if the
                // `Debug` and `Display` impls differ,
                // otherwise we'd be getting stack entries such as:
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
                // if we've reached the end of the attachment stack
                // reset attachment_idx
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
