//! Hocon allows  appending values to existing elements, substitutions, external inclusions.
//! Therefor firstly it is parsed into an AST and then AST is folded into Values.

pub(crate) type Path<'a> = Vec<&'a str>;

pub(crate) type PathRef<'a, 'b> = &'a [&'b str];

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Substitution<'a> {
    Required(Path<'a>),
    Optional(Path<'a>),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum IncludePath<'a> {
    Quoted(&'a str),
    Url(&'a str),
    File(&'a str),
    Classpath(&'a str),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Include<'a> {
    Required(IncludePath<'a>),
    NonRequired(IncludePath<'a>),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum FieldOp<'a> {
    Assign(Vec<Value<'a>>),
    Append(Vec<Value<'a>>),
    Object(Object<'a>),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Field<'a> {
    pub path: Path<'a>,
    pub op: FieldOp<'a>,
}

impl<'a> From<(Path<'a>, FieldOp<'a>)> for Field<'a> {
    fn from((path, op): (Path<'a>, FieldOp<'a>)) -> Self {
        Self { path, op }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum FieldOrInclude<'a> {
    Field(Field<'a>),
    Include(Include<'a>),
}

pub(crate) type Object<'a> = Vec<FieldOrInclude<'a>>;

pub(crate) type ArrayItem<'a> = Vec<Value<'a>>;

pub(crate) type Array<'a> = Vec<ArrayItem<'a>>;

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Value<'a> {
    Null,
    Boolean(bool),
    Integer(isize),
    Real(f64),
    String(&'a str),
    Array(Array<'a>),
    Object(Object<'a>),
    Substitution(Substitution<'a>),
}
