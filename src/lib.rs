use crate::ast::Span;
pub use crate::error::Error;
use crate::parser::root;
pub use crate::value::Value;
use serde::de::DeserializeOwned;

mod ast;
mod de;
mod duration;
mod error;
mod mem;
mod parser;
mod resolve;
mod value;

pub fn parse(input: &str) -> Result<Value, Error> {
    let (_, ast) = root(Span::new(input)).map_err(|e| Error::string(format!("failed to parse input HOCON: {e}")))?;
    // resolve::resolve(ast)
    todo!()
}

pub fn from_str<T>(input: &str) -> Result<T, Error>
where
    T: DeserializeOwned,
{
    let value = parse(input)?;
    // de::from_value(value)
    todo!()
}
