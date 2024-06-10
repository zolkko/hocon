use serde::de;
use std::fmt;
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AppendErrorKind {
    EmptyPath,
    InvalidPathType,
    IncompatibleType,
}

/// Hocon format allows to append a value to an array through `+=` operator.
#[derive(Debug, Clone, PartialEq)]
pub struct AppendError {
    pub(crate) kind: AppendErrorKind,
}

impl fmt::Display for AppendError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            AppendErrorKind::EmptyPath => f.write_str("appended path must be non-empty"),
            AppendErrorKind::InvalidPathType => f.write_str(""),
            AppendErrorKind::IncompatibleType => f.write_str("cannot append a value to a non-array field"),
        }
    }
}

impl std::error::Error for AppendError {}

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "line: {}, column: {}", self.line, self.column)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ErrorKind {
    Message(String),
    Grammar(Position, &'static str),
    ParseInt(Position, ParseIntError),
    ParseFloat(Position, ParseFloatError),
    AppendError(Position, AppendError),
    MissingKey,
    DeserializationInvalidType { position: Position, expected: String, unexpected: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn message(s: &str) -> Self {
        Error {
            kind: ErrorKind::Message(s.to_owned()),
        }
    }

    pub fn string(s: String) -> Self {
        Error { kind: ErrorKind::Message(s) }
    }

    pub fn deserialization_invalid_type(position: Position, expected: String, unexpected: String) -> Self {
        Error {
            kind: ErrorKind::DeserializationInvalidType { position, expected, unexpected },
        }
    }

    pub(crate) const fn missing_key() -> Self {
        Self { kind: ErrorKind::MissingKey }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            ErrorKind::Message(ref msg) => f.write_str(msg),
            ErrorKind::Grammar(ref pos, ref descr) => write!(f, "{} {}", pos, descr),
            ErrorKind::ParseInt(ref pos, ref error) => write!(f, "{} {}", pos, error),
            ErrorKind::ParseFloat(ref pos, ref error) => write!(f, "{} {}", pos, error),
            ErrorKind::AppendError(ref pos, ref error) => write!(f, "{} {}", pos, error),
            ErrorKind::MissingKey => write!(f, "key does not exist"),
            ErrorKind::DeserializationInvalidType {
                ref position,
                ref expected,
                ref unexpected,
            } => {
                write!(
                    f,
                    "invalid type: {unexpected}, expected {expected}, at line {line} and column {column}",
                    unexpected = unexpected,
                    expected = expected,
                    line = position.line,
                    column = position.column
                )
            }
        }
    }
}

impl std::error::Error for Error {}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error {
            kind: ErrorKind::Message(msg.to_string()),
        }
    }
}

impl From<((usize, usize), &'static str)> for Error {
    fn from(value: ((usize, usize), &'static str)) -> Self {
        let ((line, column), description) = value;
        Error {
            kind: ErrorKind::Grammar(Position { line, column }, description),
        }
    }
}

impl From<((usize, usize), ParseIntError)> for Error {
    fn from(value: ((usize, usize), ParseIntError)) -> Self {
        let ((line, column), error) = value;
        Error {
            kind: ErrorKind::ParseInt(Position { line, column }, error),
        }
    }
}

impl From<((usize, usize), ParseFloatError)> for Error {
    fn from(value: ((usize, usize), ParseFloatError)) -> Self {
        let ((line, column), error) = value;
        Error {
            kind: ErrorKind::ParseFloat(Position { line, column }, error),
        }
    }
}

impl From<((usize, usize), AppendError)> for Error {
    fn from(value: ((usize, usize), AppendError)) -> Self {
        let ((line, column), error) = value;
        Error {
            kind: ErrorKind::AppendError(Position { line, column }, error),
        }
    }
}
