use crate::value::AppendError;
use serde::de;
use std::fmt;
use std::num::{ParseFloatError, ParseIntError};

pub type Error = HoconError;

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
enum ErrorKind {
    Message(String),
    Grammar(Position, &'static str),
    IntError(Position, ParseIntError),
    FloatError(Position, ParseFloatError),
    AppendError(Position, AppendError),
}

#[derive(Debug)]
pub struct HoconError {
    kind: ErrorKind,
}

impl HoconError {
    pub fn message(s: &str) -> Self {
        HoconError {
            kind: ErrorKind::Message(s.to_owned()),
        }
    }

    pub fn string(s: String) -> Self {
        HoconError { kind: ErrorKind::Message(s) }
    }
}

impl fmt::Display for HoconError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            ErrorKind::Message(ref msg) => f.write_str(msg),
            ErrorKind::Grammar(ref pos, ref descr) => write!(f, "{} {}", pos, descr),
            ErrorKind::IntError(ref pos, ref error) => write!(f, "{} {}", pos, error),
            ErrorKind::FloatError(ref pos, ref error) => write!(f, "{} {}", pos, error),
            ErrorKind::AppendError(ref pos, ref error) => write!(f, "{} {}", pos, error),
        }
    }
}

impl std::error::Error for HoconError {}

impl de::Error for HoconError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        HoconError {
            kind: ErrorKind::Message(msg.to_string()),
        }
    }
}

impl From<((usize, usize), &'static str)> for HoconError {
    fn from(value: ((usize, usize), &'static str)) -> Self {
        let ((line, column), description) = value;
        HoconError {
            kind: ErrorKind::Grammar(Position { line, column }, description),
        }
    }
}

impl From<((usize, usize), ParseIntError)> for HoconError {
    fn from(value: ((usize, usize), ParseIntError)) -> Self {
        let ((line, column), error) = value;
        HoconError {
            kind: ErrorKind::IntError(Position { line, column }, error),
        }
    }
}

impl From<((usize, usize), ParseFloatError)> for HoconError {
    fn from(value: ((usize, usize), ParseFloatError)) -> Self {
        let ((line, column), error) = value;
        HoconError {
            kind: ErrorKind::FloatError(Position { line, column }, error),
        }
    }
}

impl From<((usize, usize), AppendError)> for HoconError {
    fn from(value: ((usize, usize), AppendError)) -> Self {
        let ((line, column), error) = value;
        HoconError {
            kind: ErrorKind::AppendError(Position { line, column }, error),
        }
    }
}
