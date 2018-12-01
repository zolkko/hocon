use std::collections::HashMap;


#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}
