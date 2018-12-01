use std::collections::HashMap;


pub type Object = HashMap<String, Value>;

pub type Array = Vec<Value>;

#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(String),
    Array(Array),
    Object(Object),
}
