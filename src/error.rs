use std::fmt;
use std::error::Error;

use pest::RuleType;
use pest::error::{Error as PestError};


#[derive(Debug)]
enum ErrorKind<R> where R: RuleType {
    PestError(PestError<R>)
}

#[derive(Debug)]
pub struct Position {
    line: usize,
    column: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "line: {}, column: {}", self.line, self.column)
    }
}

#[derive(Debug)]
pub struct HoconError<R> where R: RuleType {
    kind: ErrorKind<R>
}

impl<R: RuleType> fmt::Display for HoconError<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            ErrorKind::PestError(ref error) => write!(f, "{}", error),
        }
    }
}

impl<R: RuleType> Error for HoconError<R> { }

/// Converts the pest's grammar parsing error to a hocon-error.
impl<R: RuleType> From<PestError<R>> for HoconError<R> {
    fn from(error: PestError<R>) -> Self {
        HoconError { kind: ErrorKind::PestError(error) }
    }
}
