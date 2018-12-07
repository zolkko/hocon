use std::fmt;
use std::error::Error;
use std::collections::HashMap;

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

/// Owned hocon array.
pub type Array = Vec<Value>;

/// Owned hocon object.
pub type Object = HashMap<String, Value>;

/// Hocon format allows a user to either assign (`=`, `:`) a value to a field or
/// append (`+=`) a value to an array.
pub trait ObjectOps {

    /// Assigns a value to a field, defined by the `path`.
    ///
    /// If a field or any intermediate objects do not exist, they will be created.
    /// An existing field will be overwritten.
    fn assign_value(&mut self, path: &[String], value: Value);

    /// Appends a value to and array, defined by the `path`.
    ///
    /// If an array does not exist, it will be created.
    /// The function will return an error, if the target is not an array or
    /// intermediate fields are not objects.
    fn append_value(&mut self, path: &[String], value: Value) -> Result<(), AppendError>;
}

impl ObjectOps for Object {

    fn assign_value(&mut self, path: &[String], value: Value) {
        if let Some((key, tail)) = path.split_first() {
            if tail.is_empty() {
                self.insert(key.to_owned(), value);
            } else {
                if let Some(Value::Object(sub_obj)) = self.get_mut(key.as_str()) {
                    sub_obj.assign_value(tail, value);
                } else {
                    let mut sub_obj = Object::default();
                    sub_obj.assign_value(tail, value);
                    self.insert(key.to_owned(), Value::Object(sub_obj));
                }
            }
        }
    }

    fn append_value(&mut self, path: &[String], value: Value) -> Result<(), AppendError> {
        if let Some((key, tail)) = path.split_first() {

            if tail.is_empty() {
                if let Some(maybe_array) = self.get_mut(key) {
                    match maybe_array {
                        Value::Array(array) => {
                            array.push(value);
                            Ok(())
                        }
                        _ => Err(AppendError { kind: AppendErrorKind::IncompatibleType })
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
                    Err(AppendError { kind: AppendErrorKind::InvalidPathType })
                }
            }
        } else {
            Err(AppendError { kind: AppendErrorKind::EmptyPath })
        }
    }
}

#[derive(Debug)]
enum AppendErrorKind {
    EmptyPath,
    InvalidPathType,
    IncompatibleType
}

/// Hocon format allows to append a value to an array through `+=` operator.
#[derive(Debug)]
pub struct AppendError {
    kind: AppendErrorKind
}

impl fmt::Display for AppendError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            AppendErrorKind::EmptyPath => f.write_str("append's path must be non-empty"),
            AppendErrorKind::InvalidPathType => f.write_str(""),
            AppendErrorKind::IncompatibleType => f.write_str("cannot append a value to a non-array field"),
        }
    }
}

impl Error for AppendError { }


#[cfg(test)]
mod tests {

    use super::*;

    macro_rules! object {
        ( $( $x:expr => $y:expr ),* ) => {
            {
                let mut temp_obj = $crate::value::Object::default();
                $(
                    temp_obj.insert($x.to_owned(), $y);
                )*
                temp_obj
            }
        };
    }

    #[test]
    fn test_assign_non_existing() {
        let mut obj = Object::default();
        obj.assign_value(&vec!["field".to_owned(), "subfield".to_owned()], Value::Integer(123));

        let expected = object![
            "field" => Value::Object(object![
                "subfield" => Value::Integer(123)
            ])
        ];

        assert_eq!(obj, expected);
    }

    #[test]
    fn test_assign_override() {
        let mut obj = object![
            "field" => Value::Object(object![
                "subfield" => Value::Integer(123)
            ])
        ];
        obj.assign_value(&vec!["field".to_owned(), "subfield".to_owned()], Value::Integer(321));

        let expected = object![
            "field" => Value::Object(object![
                "subfield" => Value::Integer(321)
            ])
        ];

        assert_eq!(obj, expected);
    }

    #[test]
    fn test_assign_change_types() {
        let mut obj = object![
            "field" => Value::Object(object![
                "subfield" => Value::Integer(123)
            ])
        ];
        obj.assign_value(&vec!["field".to_owned(), "subfield".to_owned(), "subsubfield".to_owned()], Value::Integer(321));

        let expected = object![
            "field" => Value::Object(object![
                "subfield" => Value::Object(object![
                    "subsubfield" => Value::Integer(321)
                ])
            ])
        ];

        assert_eq!(obj, expected);
    }
}