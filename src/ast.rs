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
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(&'a str),
    Array(Array<'a>),
    Object(Object<'a>),
    Substitution(Substitution<'a>),
}

#[cfg(test)]
mod tests {
    /*
    use super::*;

    #[test]
    fn test_parse_array() {
        let array_ast = AstParser::parse(Rule::array, r#"[1,2,3]"#).unwrap().next().unwrap();
        let array = parse_array(None, array_ast).expect("must parse array");
        assert_eq!(array, Array(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]));
    }

    #[test]
    fn test_parse_string_value() {
        let string_value_examples = [
            (
                r#"string value  "sub-string" second"#,
                Value::String(vec![
                    StringPart::String("string value".to_owned()),
                    StringPart::String("sub-string".to_owned()),
                    StringPart::String("second".to_owned()),
                ]),
            ),
            (
                r#""quoted string value""#,
                Value::String(vec![StringPart::String("quoted string value".to_owned())]),
            ),
            (
                r#""first string" " second string""#,
                Value::String(vec![
                    StringPart::String("first string".to_owned()),
                    StringPart::String(" second string".to_owned()),
                ]),
            ),
            (
                r#""first string " 123 " second string""#,
                Value::String(vec![
                    StringPart::String("first string ".to_owned()),
                    StringPart::String("123".to_owned()),
                    StringPart::String(" second string".to_owned()),
                ]),
            ),
            (
                r#""1 string " ${variable} " 2 string""#,
                Value::String(vec![
                    StringPart::String("1 string ".to_owned()),
                    StringPart::Substitution(Substitution::Required(vec!["variable".to_owned()])),
                    StringPart::String(" 2 string".to_owned()),
                ]),
            ),
            (
                r#"123 " first string " ${variable1} " second string""#,
                Value::String(vec![
                    StringPart::String("123".to_owned()),
                    StringPart::String(" first string ".to_owned()),
                    StringPart::Substitution(Substitution::Required(vec!["variable1".to_owned()])),
                    StringPart::String(" second string".to_owned()),
                ]),
            ),
            (
                r#"some string  " first string " ${variable2} " second string""#,
                Value::String(vec![
                    StringPart::String("some string".to_owned()),
                    StringPart::String(" first string ".to_owned()),
                    StringPart::Substitution(Substitution::Required(vec!["variable2".to_owned()])),
                    StringPart::String(" second string".to_owned()),
                ]),
            ),
            (
                r#"${variable1} " first string " ${variable2} " second string""#,
                Value::String(vec![
                    StringPart::Substitution(Substitution::Required(vec!["variable1".to_owned()])),
                    StringPart::String(" first string ".to_owned()),
                    StringPart::Substitution(Substitution::Required(vec!["variable2".to_owned()])),
                    StringPart::String(" second string".to_owned()),
                ]),
            ),
            (
                r#"${variable1} ${variable2}  first string " second string " ${variable3}  third string"#,
                Value::String(vec![
                    StringPart::Substitution(Substitution::Required(vec!["variable1".to_owned()])),
                    StringPart::Substitution(Substitution::Required(vec!["variable2".to_owned()])),
                    StringPart::String("first string".to_owned()),
                    StringPart::String(" second string ".to_owned()),
                    StringPart::Substitution(Substitution::Required(vec!["variable3".to_owned()])),
                    StringPart::String("third string".to_owned()),
                ]),
            ),
        ];

        for (example_input, expected_str_value) in string_value_examples.iter() {
            let value_ast = AstParser::parse(Rule::value, example_input).unwrap().next().unwrap();
            let string_value = parse_value(value_ast).expect("cannot parse the string");
            assert_eq!(&string_value, expected_str_value);
        }
    }

    #[test]
    fn test_parse_array_value() {
        let examples = [
            (
                r#"[4, 5, 6]"#,
                Value::Array(vec![
                    ArrayPart::Array(Array(vec![Value::Integer(4), Value::Integer(5), Value::Integer(6)])),
                ]),
            ),
            (
                r#"[1, 2, 3] [4, 5, 6]"#,
                Value::Array(vec![
                    ArrayPart::Array(Array(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)])),
                    ArrayPart::Array(Array(vec![Value::Integer(4), Value::Integer(5), Value::Integer(6)])),
                ]),
            ),
            (
                r#"[1, 2, 3] ${subs} [4, 5, 6]"#,
                Value::Array(vec![
                    ArrayPart::Array(Array(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)])),
                    ArrayPart::Substitution(Substitution::Required(vec!["subs".to_owned()])),
                    ArrayPart::Array(Array(vec![Value::Integer(4), Value::Integer(5), Value::Integer(6)])),
                ]),
            ),
            (
                r#"[10, 11, 12] ${var}"#,
                Value::Array(vec![
                    ArrayPart::Array(Array(vec![Value::Integer(10), Value::Integer(11), Value::Integer(12)])),
                    ArrayPart::Substitution(Substitution::Required(vec!["var".to_owned()])),
                ]),
            ),
            (
                r#"${variabe} [7, 8, 9]"#,
                Value::Array(vec![
                    ArrayPart::Substitution(Substitution::Required(vec!["variabe".to_owned()])),
                    ArrayPart::Array(Array(vec![Value::Integer(7), Value::Integer(8), Value::Integer(9)])),
                ]),
            ),
            (
                r#"${variable1} ${variable2} [1, 2, 3]"#,
                Value::Array(vec![
                    ArrayPart::Substitution(Substitution::Required(vec!["variable1".to_owned()])),
                    ArrayPart::Substitution(Substitution::Required(vec!["variable2".to_owned()])),
                    ArrayPart::Array(Array(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)])),
                ]),
            ),
        ];

        for (example, expected) in examples.iter() {
            let array_ast = AstParser::parse(Rule::value, example).unwrap().next().unwrap();
            let array_value = parse_value(array_ast).expect("cannot parse array");
            assert_eq!(&array_value, expected);
        }
    }

    macro_rules! object {
        ( $($x:expr => $y:expr),* ) => {
            {
                let mut fields = std::collections::HashMap::new();
                $(
                    fields.insert($x, $y);
                )*
                Object { fields, path: None }
            }
        };
    }

    #[test]
    fn test_object_value() {
        let examples = vec![
            (
                r#"{ field1: 1 }"#,
                Value::Object(vec![
                    ObjectPart::Object(object![
                        "field1".to_owned() => Value::Integer(1)
                    ])
                ]),
            ),
            (
                r#"{ field1: 1 } { field2: 2 }"#,
                Value::Object(vec![
                    ObjectPart::Object(object![
                        "field1".to_owned() => Value::Integer(1)
                    ]),
                    ObjectPart::Object(object![
                        "field2".to_owned() => Value::Integer(2)
                    ]),
                ]),
            ),
            (
                r#"{ field1: 1 } ${variable1} { field2: 2 } ${variable2}"#,
                Value::Object(vec![
                    ObjectPart::Object(object![
                        "field1".to_owned() => Value::Integer(1)
                    ]),
                    ObjectPart::Substitution(Substitution::Required(vec!["variable1".to_owned()])),
                    ObjectPart::Object(object![
                        "field2".to_owned() => Value::Integer(2)
                    ]),
                    ObjectPart::Substitution(Substitution::Required(vec!["variable2".to_owned()])),
                ]),
            ),
            (
                r#"{ field1 = ${variable1} ${variable2} { field2: 1 } }"#,
                Value::Object(vec![
                    ObjectPart::Object(object![
                        "field1".to_owned() => Value::Object(vec![
                            ObjectPart::Substitution(Substitution::Required(vec!["variable1".to_owned()])),
                            ObjectPart::Substitution(Substitution::Required(vec!["variable2".to_owned()])),
                            ObjectPart::Object(object![
                                "field2".to_owned() => Value::Integer(1)
                            ]),
                        ])
                    ])
                ]),
            ),
            (
                r#"{ field1 += 1, field1 += 2, field1 += 3 }"#,
                Value::Object(vec![
                    ObjectPart::Object(object![
                        "field1".to_owned() => Value::Array(vec![
                            ArrayPart::Array(Array(vec![Value::Integer(1)])),
                            ArrayPart::Array(Array(vec![Value::Integer(2)])),
                            ArrayPart::Array(Array(vec![Value::Integer(3)])),
                        ])
                    ])
                ]),
            ),
            (
                r#"{ field1 = ${variable}, field1 += 2, field1 += 3 }"#,
                Value::Object(vec![
                    ObjectPart::Object(object![
                        "field1".to_owned() => Value::Array(vec![
                            ArrayPart::Substitution(Substitution::Required(vec!["variable".to_owned()])),
                            ArrayPart::Array(Array(vec![Value::Integer(2)])),
                            ArrayPart::Array(Array(vec![Value::Integer(3)])),
                        ])
                    ])
                ]),
            ),
            (
                r#"{ field1 = ${variable1} ${variable2}, field1 += 2, field1 += 3 }"#,
                Value::Object(vec![
                    ObjectPart::Object(object![
                        "field1".to_owned() => Value::Array(vec![
                            ArrayPart::Substitution(Substitution::Required(vec!["variable1".to_owned()])),
                            ArrayPart::Substitution(Substitution::Required(vec!["variable2".to_owned()])),
                            ArrayPart::Array(Array(vec![Value::Integer(2)])),
                            ArrayPart::Array(Array(vec![Value::Integer(3)])),
                        ])
                    ])
                ]),
            ),
            (
                r#"{ field1.field2 += 1, field1.field2 += 2, field1.field2 += 3 }"#,
                Value::Object(vec![
                    ObjectPart::Object(object![
                        "field1".to_owned() => Value::Object(vec![
                            ObjectPart::Object(object![
                                "field2".to_owned() => Value::Array(vec![
                                    ArrayPart::Array(Array(vec![Value::Integer(1)])),
                                    ArrayPart::Array(Array(vec![Value::Integer(2)])),
                                    ArrayPart::Array(Array(vec![Value::Integer(3)])),
                                ])
                            ])
                        ])
                    ])
                ]),
            ),
        ];

        for (example, expected) in examples {
            let object_ast = AstParser::parse(Rule::value, example).unwrap().next().unwrap();
            let object_value = parse_value(object_ast).expect("cannot parse object");
            assert_eq!(object_value, expected);
        }
    }

    #[test]
    fn test_include() {
        let examples = [
            (
                r#"include "some value""#,
                Include::NonRequired(IncludePath::SingleQuoted("some value".to_owned()))
            ),
            (
                r#"include url("http://example.com")"#,
                Include::NonRequired(IncludePath::Url("http://example.com".to_owned()))
            ),
            (
                r#"include file("test.txt")"#,
                Include::NonRequired(IncludePath::File("test.txt".to_owned()))
            ),
            (
                r#"include classpath("classpath")"#,
                Include::NonRequired(IncludePath::Classpath("classpath".to_owned()))
            ),

            (
                r#"include required("some value")"#,
                Include::Required(IncludePath::SingleQuoted("some value".to_owned()))
            ),
            (
                r#"include required(url("http://example.com"))"#,
                Include::Required(IncludePath::Url("http://example.com".to_owned()))
            ),
            (
                r#"include required(file("test.txt"))"#,
                Include::Required(IncludePath::File("test.txt".to_owned()))
            ),
            (
                r#"include required(classpath("classpath"))"#,
                Include::Required(IncludePath::Classpath("classpath".to_owned()))
            ),
        ];

        for (input, expected) in examples.iter() {
            let include_pair = AstParser::parse(Rule::include, input).unwrap().next().unwrap();
            let res = parse_include(include_pair).expect(format!("failed to parse {}", input).as_ref());
            assert_eq!(&res, expected);
        }
    }

    #[test]
    fn test_process_field_path() {
        let examples = [
            (r#"part"#, vec!["part".to_owned()]),
            (r#""some part""#, vec!["some part".to_owned()]),
            (r#"part1.part2"#, vec!["part1".to_owned(), "part2".to_owned()]),
            (r#""part 1"."part 2""#, vec!["part 1".to_owned(), "part 2".to_owned()]),
        ];

        for (input, expected) in examples.iter() {
            let field_path_pair = AstParser::parse(Rule::field_path, input).unwrap().next().unwrap();
            let res = parse_field_path(field_path_pair).expect(format!("failed to parse {}", input).as_ref());
            assert_eq!(&res, expected);
        }
    }
    */
}
