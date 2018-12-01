use std::fmt;
use std::error::Error;

use pest::RuleType;
use pest::error::{Error as PestError};


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
enum ErrorKind<R> where R: RuleType {
    Grammar(Position, &'static str),
    PestError(PestError<R>)
}

#[derive(Debug)]
pub struct HoconError<R> where R: RuleType {
    kind: ErrorKind<R>
}

impl<R: RuleType> fmt::Display for HoconError<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            ErrorKind::Grammar(ref pos, ref descr) => write!(f, "{} {}", pos, descr),
            ErrorKind::PestError(ref error) => write!(f, "{}", error),
        }
    }
}

impl<R: RuleType> Error for HoconError<R> { }

impl<R: RuleType> From<PestError<R>> for HoconError<R> {
    fn from(error: PestError<R>) -> Self {
        HoconError { kind: ErrorKind::PestError(error) }
    }
}

/// Converts position and error description into Hocon error
impl<R: RuleType> From<((usize, usize), &'static str)> for HoconError<R> {
    fn from(value: ((usize, usize), &'static str)) -> Self {
        let ((line, column), description) = value;
        HoconError { kind: ErrorKind::Grammar( Position { line, column }, description) }
    }
}
