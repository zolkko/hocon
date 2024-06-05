//! Hocon allows  appending values to existing elements, substitutions, external inclusions.
//! Therefor firstly it is parsed into an AST and then AST is folded into Values.
use nom_locate::LocatedSpan;

pub(crate) type Span<'a> = LocatedSpan<&'a str>;

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
pub(crate) enum FieldOp<'a, S> {
    Assign(Vec<Value<'a, S>>),
    Append(Vec<Value<'a, S>>),
    Object(Object<'a, S>),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Field<'a, S> {
    pub path: Path<'a>,
    pub op: FieldOp<'a, S>,
}

impl<'a, S> From<(Path<'a>, FieldOp<'a, S>)> for Field<'a, S> {
    fn from((path, op): (Path<'a>, FieldOp<'a, S>)) -> Self {
        Self { path, op }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum FieldOrInclude<'a, S> {
    Field(Field<'a, S>),
    Include(Include<'a>),
}

pub(crate) type Fields<'a, S> = Vec<FieldOrInclude<'a, S>>;

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Object<'a, S> {
    pub(crate) fields: Fields<'a, S>,
    pub(crate) span: S,
}

impl<'a, S> Object<'a, S> {
    pub(crate) fn new(fields: Vec<FieldOrInclude<'a, S>>, span: S) -> Self {
        Object { fields, span }
    }
}

pub(crate) type ArrayItem<'a, S> = Vec<Value<'a, S>>;

pub(crate) type Array<'a, S> = Vec<ArrayItem<'a, S>>;

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum ValueKind<'a, S> {
    Null,
    Boolean(bool),
    Integer(isize),
    Real(f64),
    String(&'a str),
    Array(Array<'a, S>),
    Object(Object<'a, S>),
    Substitution(Substitution<'a>),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Value<'a, S = Span<'a>> {
    pub kind: ValueKind<'a, S>,
    pub span: S,
}

impl<'a, S> Value<'a, S> {
    pub(crate) fn new(kind: ValueKind<'a, S>, span: S) -> Self {
        Self { kind, span }
    }
}

#[cfg(test)]
impl<'a> Value<'a, Span<'a>> {
    pub(crate) fn span(&self) -> &Span {
        &self.span
    }

    pub(crate) fn kind(self) -> ValueKind<'a, Span<'a>> {
        self.kind
    }
}
