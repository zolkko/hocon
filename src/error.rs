use std::fmt;
use std::error::Error;
use std::num::{ParseIntError, ParseFloatError};

use pest::RuleType;
use pest::error::{Error as PestError};

use serde::de;

use crate::value::AppendError;


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
    Message(String),
    Grammar(Position, &'static str),
    PestError(PestError<R>),
    IntError(Position, ParseIntError),
    FloatError(Position, ParseFloatError),
    AppendError(Position, AppendError)
}

#[derive(Debug)]
pub struct HoconError<R> where R: RuleType {
    kind: ErrorKind<R>
}

impl<R: RuleType> fmt::Display for HoconError<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            ErrorKind::Message(ref msg) => f.write_str(msg),
            ErrorKind::Grammar(ref pos, ref descr) => write!(f, "{} {}", pos, descr),
            ErrorKind::PestError(ref error) => write!(f, "{}", error),
            ErrorKind::IntError(ref pos, ref error) => write!(f, "{} {}", pos, error),
            ErrorKind::FloatError(ref pos, ref error) => write!(f, "{} {}", pos, error),
            ErrorKind::AppendError(ref pos, ref error) => write!(f, "{} {}", pos, error),
        }
    }
}

impl<R: RuleType> Error for HoconError<R> { }

impl<R: RuleType> de::Error for HoconError<R> {

    fn custom<T: fmt::Display>(msg: T) -> Self {
        HoconError { kind: ErrorKind::Message(msg.to_string()) }
    }

}

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

impl<R: RuleType> From<((usize, usize), ParseIntError)> for HoconError<R> {
    fn from(value: ((usize, usize), ParseIntError)) -> Self {
        let ((line, column), error) = value;
        HoconError { kind: ErrorKind::IntError( Position { line, column }, error) }
    }
}

impl<R: RuleType> From<((usize, usize), ParseFloatError)> for HoconError<R> {
    fn from(value: ((usize, usize), ParseFloatError)) -> Self {
        let ((line, column), error) = value;
        HoconError { kind: ErrorKind::FloatError( Position { line, column }, error) }
    }
}

impl<R: RuleType> From<((usize, usize), AppendError)> for HoconError<R> {
    fn from(value: ((usize, usize), AppendError)) -> Self {
        let ((line, column), error) = value;
        HoconError { kind: ErrorKind::AppendError( Position { line, column }, error) }
    }
}
