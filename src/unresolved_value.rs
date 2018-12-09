pub enum ValueChunks {
    Resolved(ValueChunk),
    Unresolved(Vec<ValueChunk>)
}

pub enum Array {
    Resolved(Vec<ValueChunk>),
    Unresolved(Vec<ValueChunk>)
}

type UnresolvedArray = Vec<ValueChunks>;

type UnresolvedObject = HashMap<String, ValueChunks>;

pub enum Substitution {
    Required(Vec<String>),
    Optional(Vec<String>),
}

/// Represents any valid HOCON value.
#[derive(Debug, Clone, PartialEq)]
pub enum ValueChunk {
    Null,
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(String),
    Array(UnresolvedArray),
    Object(UnresolvedObject),
    Substitution(Substitution)
}
