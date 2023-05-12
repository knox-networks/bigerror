# bigerror
`error BIGERROR NOT NULL`


# example

```rust
use bigerror::{
    to_report, from_report,
    DbErrork, NetworkError,
    NotFound, ParseError, TransactionError,
    InvalidInput, Report
};


#[derive(Debug, thiserror::Error)]
#[error("CliError")]
pub struct CliError;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0:?}")]
    Report(Report<CliError>),

    #[error(transparent)]
    TonicStatus(#[from] tonic::Status),
}

from_report!({
    impl From<other_mod::Error as ToReport<_>>

    impl From<DbError>
    impl From<NetworkError>
    impl From<NotFound>
    impl From<ParseError>
    impl From<TransactionError>

    impl From<Report<InvalidInput>>
    impl From<Report<NetworkError>>
    impl From<Report<NotFound>>
    impl From<Report<ParseError>>
    impl From<Report<TransactionError>>

    impl From<std::io::Error>

    impl From<tonic::transport::Error as NetworkError>

    impl From<std::num::ParseIntError as ParseError>
    impl From<rust_decimal::Error as ParseError>
    impl From<hex::FromHexError as ParseError>
    impl From<std::num::TryFromIntError as ParseError>
    impl From<uuid::Error as ParseError>

    for Error::Report(CliError)
});

to_report!(impl ToReport<CliError> for Error::Report);
```
