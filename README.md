# bigerror

```sql
-- This error crate is intended to
-- enhance error-stack:
-- https://hash.dev/blog/announcing-error-stack
-- error-stack `is_fantastic && does_things.in(|a| functional(approach))` but
-- `error_stack::Report::change_context` can make code noisy when used often
--
-- ...so why not auto implement a bunch of `From<Error> for MyError`?
-- and provide fundamental building blocks such as `bigerror::NotFound`
-- to express common causes for errors and imply correlation?
CREATE CRATE IF NOT EXISTS
  bigerror (
    error BIGERROR NOT NULL,
);
```


# example

```rust
use bigerror::{
    from_report, reportable, to_report, BoxError, ConversionError, DbError, NetworkError, NotFound,
    ParseError, Report, ReportAs, Reportable, ResultExt,
};
use uuid::Uuid;

#[derive(Debug, thiserror::Error)]
#[error("MyError")]
pub struct MyError;

reportable!(MyError);

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0:?}")]
    Report(Report<MyError>),
    /// we want to preserve this for match arm purposes
    #[error(transparent)]
    TonicStatus(#[from] tonic::Status),
}

from_report!({
    impl From<OtherError as ToReport<_>>

    impl From<std::io::Error>
    impl From<tonic::transport::Error as NetworkError>
    impl From<std::num::ParseIntError as ParseError>
    impl From<sqlx::Error as DbError>

    for Error::Report(MyError)
});

#[derive(Debug, thiserror::Error)]
pub enum OtherError {
    #[error("{0:?}")]
    Report(Report<NotFound>),
}

to_report!(impl ToReport<NotFound> for OtherError::Report);

fn conversion_error() -> Result<usize, Report<ConversionError>> {
    "NaN"
        .parse::<usize>()
        .map_err(ConversionError::from::<&str, usize>)
        .attach(ParseError)
}

fn box_error() -> Result<usize, Report<BoxError>> {
    fn inner() -> Result<usize, Box<dyn std::error::Error + Sync + Send>> {
        Ok("NaN".parse::<usize>().map_err(Box::new)?)
    }

    inner().map_err(BoxError::from)
}

fn main() -> Result<(), Report<MyError>> {
    let conversion = conversion_error().change_context(MyError)?;

    let box_error = box_error().change_context(MyError)?;

    let other_err = Err(OtherError::Report(NotFound::with_kv("id", Uuid::new_v4())));
    other_err.map_err(MyError::report_inner)?;

    "NaN".parse::<usize>().report_as()?;

    Ok(())
}
```
