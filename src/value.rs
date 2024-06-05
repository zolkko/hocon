use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

static MISSING_KEY: ValueKind = ValueKind::BadValue(crate::error::Error::missing_key());

#[derive(Default, Clone, PartialEq, Debug)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Value(pub ValueKind, pub Position);

impl Value {
    pub fn new(kind: ValueKind, position: Position) -> Self {
        Self(kind, position)
    }
}

impl From<ValueKind> for Value {
    fn from(value: ValueKind) -> Self {
        Value(value, Position::new(0, 0))
    }
}

/// Represents any HOCON value.
#[derive(Debug, Clone, PartialEq)]
pub enum ValueKind {
    Null,
    Boolean(bool),
    Integer(isize),
    Real(f64),
    String(String),
    Array(Array),
    Object(Object),
    BadValue(crate::error::Error),
}

impl<T> std::ops::Index<T> for ValueKind
where
    T: AsRef<str>,
{
    type Output = ValueKind;

    fn index(&self, index: T) -> &Self::Output {
        if let ValueKind::Object(ref object) = self {
            let key = index.as_ref();
            let Some(Value(value, _)) = object.get(key) else {
                return &MISSING_KEY;
            };
            value
        } else {
            &MISSING_KEY
        }
    }
}

/// The default value is `Value::Null`.
impl Default for ValueKind {
    fn default() -> Self {
        ValueKind::Null
    }
}

pub type ArrayItems = Vec<Value>;

/// Owned hocon array.
#[derive(Clone, PartialEq, Default, Debug)]
pub struct Array {
    pub items: ArrayItems,
    pub position: Position,
}

impl Array {
    pub fn new(items: ArrayItems, position: Position) -> Self {
        Self { items, position }
    }

    pub fn push(&mut self, value: Value) {
        self.items.push(value)
    }
}

pub type Fields = HashMap<String, Value>;

/// Owned hocon object.
#[derive(Default, Clone, PartialEq, Debug)]
pub struct Object {
    pub fields: Fields,
    pub position: Position,
}

impl Object {
    pub fn new(fields: Fields, position: Position) -> Self {
        Self { fields, position }
    }

    #[inline(always)]
    pub fn get<Q>(&self, k: &Q) -> Option<&Value>
    where
        String: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.fields.get(k)
    }

    #[inline(always)]
    pub fn insert(&mut self, k: String, v: Value) -> Option<Value> {
        self.fields.insert(k, v)
    }

    #[inline(always)]
    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut Value>
        where
            String: Borrow<Q>,
            Q: Hash + Eq + ?Sized,
    {
        self.fields.get_mut(k)
    }

    #[inline(always)]
    pub fn contains_key<Q>(&self, k: &Q) -> bool
        where
            String: Borrow<Q>,
            Q: Hash + Eq + ?Sized,
    {
        self.fields.contains_key(k)
    }
}

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
    fn append_value(&mut self, path: &[String], value: Value) -> Result<(), crate::error::AppendError>;

    /// According to the hocon documentation, the merge must be performed only on object.
    /// - keys and values of the second object must be added to the first object;
    /// - if a key exists in both objects and their values are sub-objects, then they must be
    ///   merged recursively.
    fn merge_object(&mut self, second: &Object);
}

impl ObjectOps for Object {
    fn assign_value(&mut self, path: &[String], value: Value) {
        if let Some((key, tail)) = path.split_first() {
            if tail.is_empty() {
                self.insert(key.to_owned(), value);
            } else if let Some(Value(ValueKind::Object(sub_obj), _)) = self.get_mut(key.as_str()) {
                sub_obj.assign_value(tail, value);
            } else {
                let mut sub_obj = Object::default();
                sub_obj.assign_value(tail, value);
                // FIXME: use real coordinates
                let pos = Position::new(0, 0);
                self.insert(key.to_owned(), Value::new(ValueKind::Object(sub_obj), pos));
            }
        }
    }

    fn append_value(&mut self, path: &[String], value: Value) -> Result<(), crate::error::AppendError> {
        if let Some((key, tail)) = path.split_first() {
            if tail.is_empty() {
                if let Some(Value(maybe_array, _)) = self.get_mut(key) {
                    match maybe_array {
                        ValueKind::Array(array) => {
                            array.push(value);
                            Ok(())
                        }
                        _ => Err(crate::error::AppendError {
                            kind: crate::error::AppendErrorKind::IncompatibleType,
                        }),
                    }
                } else {
                    let pos = value.1.clone();
                    self.insert(key.to_owned(), Value::new(ValueKind::Array(Array::new(vec![value], pos.clone())), pos));
                    Ok(())
                }
            } else {
                if !self.contains_key(key.as_str()) {
                    // FIXME: use real coordinates
                    let pos = Position::new(0, 0);
                    self.insert(key.to_owned(), Value::new(ValueKind::Object(Object::default()), pos));
                }

                if let Some(Value(ValueKind::Object(sub_obj), _)) = self.get_mut(key.as_str()) {
                    sub_obj.append_value(tail, value)
                } else {
                    Err(crate::error::AppendError {
                        kind: crate::error::AppendErrorKind::InvalidPathType,
                    })
                }
            }
        } else {
            Err(crate::error::AppendError {
                kind: crate::error::AppendErrorKind::EmptyPath,
            })
        }
    }

    fn merge_object(&mut self, second: &Object) {
        for (k, v) in second.fields.iter() {
            if let Value(ValueKind::Object(from), _) = v {
                if let Some(Value(ValueKind::Object(to), _)) = self.get_mut(k) {
                    to.merge_object(from);
                } else {
                    self.insert(k.to_owned(), v.clone());
                }
            } else {
                self.insert(k.to_owned(), v.clone());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    /*
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
       fn assign_non_existing() {
           let mut obj = Object::default();
           obj.assign_value(&vec!["field".to_owned(), "subfield".to_owned()], ValueKind::Integer(123));

           let expected = object![
               "field" => ValueKind::Object(object![
                   "subfield" => ValueKind::Integer(123)
               ])
           ];

           assert_eq!(obj, expected);
       }

       #[test]
       fn assign_override() {
           let mut obj = object![
               "field" => ValueKind::Object(object![
                   "subfield" => ValueKind::Integer(123)
               ])
           ];
           obj.assign_value(&vec!["field".to_owned(), "subfield".to_owned()], ValueKind::Integer(321));

           let expected = object![
               "field" => ValueKind::Object(object![
                   "subfield" => ValueKind::Integer(321)
               ])
           ];

           assert_eq!(obj, expected);
       }

       #[test]
       fn assign_change_types() {
           let mut obj = object![
               "field" => ValueKind::Object(object![
                   "subfield" => ValueKind::Integer(123)
               ])
           ];
           obj.assign_value(&vec!["field".to_owned(), "subfield".to_owned(), "subsubfield".to_owned()], ValueKind::Integer(321));

           let expected = object![
               "field" => ValueKind::Object(object![
                   "subfield" => ValueKind::Object(object![
                       "subsubfield" => ValueKind::Integer(321)
                   ])
               ])
           ];

           assert_eq!(obj, expected);
       }

       #[test]
       fn merge_object() {
           let mut to_obj = object![
               "field1" => ValueKind::Object(object![
                   "subfield1" => ValueKind::Integer(1)
               ])
           ];

           let from_obj = object![
               "field1" => ValueKind::Object(object![
                   "subfield2" => ValueKind::Integer(2)
               ]),
               "field2" => ValueKind::Object(object![
                   "subfield3" => ValueKind::Integer(3)
               ])
           ];

           let expected = object![
               "field1" => ValueKind::Object(object![
                   "subfield1" => ValueKind::Integer(1),
                   "subfield2" => ValueKind::Integer(2)
               ]),
               "field2" => ValueKind::Object(object![
                   "subfield3" => ValueKind::Integer(3)
               ])
           ];

           to_obj.merge_object(&from_obj);

           assert_eq!(to_obj, expected);
       }
    */
}
