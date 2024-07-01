use core::fmt::{Debug, Display};

use error_stack::Context;

/// Classification of the contents of a [`Frame`], determined by how it was created.
///
/// [`Frame`]: error_stack::Frame
pub enum FrameKind<'f> {
    /// Frame was created through [`Report::new()`] or [`change_context()`].
    ///
    /// [`Report::new()`]: error_stack::Report::new
    /// [`change_context()`]: error_stack::Report::change_context
    Context(&'f dyn Context),
    /// Frame was created through [`attach()`] or [`attach_printable()`].
    ///
    /// [`attach()`]: error_stack::Report::attach
    /// [`attach_printable()`]: error_stack::Report::attach_printable
    Attachment(AttachmentKind<'f>),
}

/// Classification of an attachment which is determined by the method it was created in.
#[non_exhaustive]
pub enum AttachmentKind<'f> {
    /// A generic attachment created through [`attach()`].
    ///
    /// [`attach()`]: error_stack::Report::attach
    Opaque(&'f (dyn Send + Sync + 'static)),
    /// A printable attachment created through [`attach_printable()`].
    ///
    /// [`attach_printable()`]: error_stack::Report::attach_printable
    Printable(&'f dyn Printable),
}

// TODO: Replace `Printable` by trait bounds when trait objects for multiple traits are supported.
//   see https://github.com/rust-lang/rfcs/issues/2035
pub trait Printable: Debug + Display + Send + Sync + 'static {}
impl<T> Printable for T where T: Debug + Display + Send + Sync + 'static {}
