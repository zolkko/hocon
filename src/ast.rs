#[derive(PartialEq, Clone, Debug)]
pub(crate) struct Path(Vec<String>);

#[derive(PartialEq, Clone, Debug)]
pub(crate) enum Value {
    Null,
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(String),
    Array(Array),
    Object(Object),
    Substitution(Path),
    OptionalSubstitution(Path),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Object(Vec<(Path, Value)>);

impl Object {
    fn append(&mut self, key: Path, value: Value) {
        self.0.push((key, value));
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Array(Vec<Value>);

impl Array {
    fn append(&mut self, value: Value) {
        self.0.push(value)
    }
}
