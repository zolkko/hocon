use std::fmt;
use std::error::Error;
use std::collections::HashMap;

/// Owned hocon object.
pub type Object = HashMap<String, Value>;

/// Owned hocon array.
pub type Array = Vec<Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(String),
    Array(Array),
    Object(Object),
}

#[derive(Debug)]
pub enum AppendError {
    EmptyPath,
    InvalidPathType,
    IncompatibleType,
}

impl fmt::Display for AppendError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            AppendError::EmptyPath => f.write_str("append's path must be non-empty"),
            AppendError::InvalidPathType => f.write_str(""),
            AppendError::IncompatibleType => f.write_str("cannot append a value to a non-array field"),
        }
    }
}

impl Error for AppendError { }

pub trait ObjectOps {
    fn assign_value(&mut self, path: &[String], value: Value);
    fn append_value(&mut self, path: &[String], value: Value) -> Result<(), AppendError>;
}


impl ObjectOps for Object {

    fn assign_value(&mut self, path: &[String], value: Value) {
        if path.len() == 1 {
            let key = &path[0];
            self.insert(key.to_owned(), value);
        } else if path.len() > 1 {
            if let Some((key, tail)) = path.split_first() {
                if !self.contains_key(key.as_str()) {
                    self.insert(key.to_owned(), Value::Object(Object::default()));
                }

                if let Some(Value::Object(sub_obj)) = self.get_mut(key.as_str()) {
                    sub_obj.assign_value(tail, value);
                }
            }
        }
    }

    fn append_value(&mut self, path: &[String], value: Value) -> Result<(), AppendError> {
        if let Some((key, tail)) = path.split_first() {
            // TODO(zolkko): remove tail recursion to prevent stack overflow

            if tail.is_empty() {
                if let Some(maybe_array) = self.get_mut(key) {
                    match maybe_array {
                        Value::Array(array) => {
                            array.push(value);
                            Ok(())
                        }
                        _ => Err(AppendError::IncompatibleType)
                    }
                } else {
                    self.insert(key.to_owned(), Value::Array(vec![value]));
                    Ok(())
                }
            } else {

                if !self.contains_key(key.as_str()) {
                    self.insert(key.to_owned(), Value::Object(Object::default()));
                }

                if let Some(Value::Object(sub_obj)) = self.get_mut(key.as_str()) {
                    sub_obj.append_value(tail, value)
                } else {
                    Err(AppendError::InvalidPathType)
                }
            }
        } else {
            Err(AppendError::EmptyPath)
        }
    }
}
