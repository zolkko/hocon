use std::default::Default;
use std::error::Error;
use std::collections::HashMap;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;


pub(crate) type BoxError = Box<dyn Error>;

pub(crate) type Path = Vec<String>;

/// When an "include" directive is encountered in a hocon file,
/// a user is asked to provide it's raw content to the parser.
///
/// All the files are resolved
#[derive(PartialEq, Clone, Debug)]
pub(crate) enum IncludePath {
    SingleQuoted(String),
    Url(String),
    File(String),
    Classpath(String)
}

/// An include directive can be either required or non required.
#[derive(PartialEq, Clone, Debug)]
pub(crate) enum Include {
    Required(IncludePath),
    NonRequired(IncludePath),
}

#[derive(PartialEq, Clone, Debug)]
pub(crate) enum Substitution {
    Required(Path),
    Optional(Path),
}

/// Unquoted hocon string may contain a substitution inside it.
/// Thus a complete unresolved Hocon string consists of multiple parts.
#[derive(PartialEq, Clone, Debug)]
pub(crate) enum StringPart {
    String(String),
    Substitution(Substitution),
}

/// An array may consists of one or more arrays and substitution expressions
/// which must be merged into a single array value when the container object
/// gets resolved.
#[derive(PartialEq, Clone, Debug)]
pub(crate) enum ArrayPart {
    Array(Array),
    Substitution(Substitution),
}

#[derive(PartialEq, Clone, Debug)]
pub(crate) enum Value {
    Null,
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(Vec<StringPart>),
    Array(Vec<ArrayPart>),
    Object(Object),
    Substitution(Substitution),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum FieldOp {
    Assign(Path, Value),
    Append(Path, Value),
    Incl(Include),
}

#[derive(Default, PartialEq, Clone, Debug)]
pub(crate) struct Object(HashMap<String, Value>);

impl Object {
    fn append(&mut self, field: FieldOp) {
        match field {
            FieldOp::Assign(path, value) => {

            },
            FieldOp::Append(path, value) => {

            },
            FieldOp::Incl(include) => {

            },
        }
    }

    fn assign_value(&mut self, path: &[String], value: Value) -> Result<(), Box<dyn Error>> {
        if let Some((key, tail)) = path.split_first() {
            if tail.is_empty() {
                self.0.insert(key.clone(), value);
                Ok(())
            } else {
                if let Some(Value::Object(sub_obj)) = self.0.get_mut(key.as_str()) {
                    sub_obj.assign_value(tail, value)
                } else {
                    let mut sub_obj = Object::default();
                    let () = sub_obj.assign_value(tail, value)?;
                    self.0.insert(key.clone(), Value::Object(sub_obj));
                    Ok(())
                }
            }
        } else {
            Err("empty path".into())
        }
    }

    fn append_value(&mut self, path: &[String], value: Value) -> Result<(), Box<dyn Error>> {
        if let Some((key, tail)) = path.split_first() {
            if tail.is_empty() {
                if let Some(existing_value) = self.0.get_mut(key) {
                    match existing_value {
                        Value::Array(array) => {
                            if let Some((ArrayPart::Array(last), _)) = array.split_last_mut() {
                                last.0.push(value);
                            } else {
                                array.push(ArrayPart::Array(Array(vec![value])));
                            }
                            Ok(())
                        },
                        Value::Substitution(sub) => {
                            let parts = vec![
                                ArrayPart::Substitution(sub.clone()),
                                ArrayPart::Array(Array(vec![value])),
                            ];
                            self.0.insert(key.clone(), Value::Array(parts));
                            Ok(())
                        },
                        _ => Err("incompatible type".into()),
                    }
                } else {
                    self.0.insert(key.clone(), value);
                    Ok(())
                }
            } else {
                if !self.0.contains_key(key.as_str()) {
                    self.0.insert(key.clone(), Value::Object(Object::default()));
                }
                if let Some(Value::Object(sub_obj)) = self.0.get_mut(key.as_str()) {
                    sub_obj.append_value(tail, value)
                } else {
                    Err("invalid path type".into())
                }
            }
        } else {
            Err("empty path".into())
        }
    }

    fn merge_object(&mut self, second: &Object) {
        for (k, v) in second.0.iter() {
            if let Value::Object(ref from) = v {
                if let Some(Value::Object(to)) = self.0.get_mut(k.as_str()) {
                    to.merge_object(from);
                } else {
                    self.0.insert(k.clone(), v.clone());
                }
            } else {
                self.0.insert(k.clone(), v.clone());
            }
        }
    }
}

#[derive(Default, PartialEq, Clone, Debug)]
pub(crate) struct Array(Vec<Value>);

impl Array {
    fn append(&mut self, value: Value) {
        self.0.push(value)
    }
}

/// Parser object
#[derive(Parser)]
#[grammar = "hocon.pest"]
pub(crate) struct AstParser {
}

impl Default for AstParser {
    fn default() -> Self {
        AstParser::new()
    }
}

impl AstParser {
    pub fn new() -> Self {
        AstParser { }
    }

    /// Parses a string slice into owned hocon object.
    /// Similar to JSON format, Hocon allows arrays and objects to be defined on the top level.
    pub fn parse_str(&self, input: &str) -> Result<Value, BoxError> {
        let mut parsed = AstParser::parse(Rule::root, input)?;

        let root_pair = parsed.next().unwrap();
        let mut pairs = root_pair.into_inner();
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            Rule::array => {
                parse_arrays(pair, pairs).map(|v| Value::Array(v))
            },
            Rule::object => {
                parse_object(pair).map(|v| Value::Object(v))
            },
            Rule::object_body => {
                parse_object_body(pair).map(|v| Value::Object(v))
            },
            _ => {
                unreachable!("root grammar rule does not correspond to processing logic")
            },
        }
    }
}

fn parse_value(value: Pair<Rule>) -> Result<(Value, Option<Vec<Path>>), BoxError> {
    let value_chunks = value.into_inner();
    let mut input = value_chunks.clone();

    if let Some(pair) = input.next() {
        let position = pair.as_span().start_pos().line_col();

        match pair.as_rule() {
            Rule::null => {
                Ok((Value::Null, None))
            },
            Rule::bool_true => {
                Ok((Value::Bool(true), None))
            },
            Rule::bool_false => {
                Ok((Value::Bool(false), None))
            },
            Rule::float => {
                match pair.as_str().parse() {
                    Ok(v) => Ok((Value::Float(v), None)),
                    Err(error) => Err(format!("{:?} {:?}", error, position).into()),
                }
            },
            Rule::int => {
                match pair.as_str().parse() {
                    Ok(v) => Ok((Value::Integer(v), None)),
                    Err(error) => Err(format!("{:?} {:?}", error, position).into()),
                }
            },
            Rule::unquoted_string => {
                let value = process_string_value(pair, input, value_chunks.as_str())?;
                // TODO: deps
                Ok((value, None))
            },
            Rule::string | Rule::mstring => {
                let value = process_string_value(pair, input, value_chunks.as_str())?;
                // TODO: deps
                Ok((value, None))
            },
            Rule::array => {
                parse_arrays(pair, input).map(|v| {
                    // TODO: deps
                    (Value::Array(v), None)
                })
            },
            Rule::object => {
                parse_object(pair).map(|obj| {
                    // TODO: deps
                    (Value::Object(obj), None)
                })
            },
            Rule::substitution => {
                parse_substitutions(pair, input).map(|v| {
                    (v, None)
                })
            },
            _ => {
                unreachable!("grammar rule definitions do not correspond to the source code")
            },
        }
    } else {
        Ok((Value::Null, None))
    }
}

/// An array value may consists of
fn parse_arrays(current_pair: Pair<Rule>, mut input: Pairs<Rule>) -> Result<Vec<ArrayPart>, BoxError> {
    let mut array_parts = vec![ArrayPart::Array(parse_array(current_pair)?)];
    for pair in input {
        let part = match pair.as_rule() {
            Rule::array => ArrayPart::Array(parse_array(pair)?),
            Rule::substitution => ArrayPart::Substitution(parse_substitution(pair)?),
            _ => unreachable!("grammar rule definitions do not correspond to the source code"),
        };
        array_parts.push(part);
    }
    Ok(array_parts)
}

fn parse_array(array: Pair<Rule>) -> Result<Array, BoxError> {
    let mut result: Array = Array::default();
    let array_values = array.into_inner();
    for array_item in array_values {
        let (value, _deps) = parse_value(array_item)?;
        result.append(value);
    }
    Ok(result)
}

fn parse_substitutions(current_pair: Pair<Rule>, mut input: Pairs<Rule>)  -> Result<Value, BoxError> {
    let subs = parse_substitution(current_pair)?;
    if let Some(next) = input.next() {
        match next.as_rule() {
            Rule::array => {
                let mut array_value = parse_arrays(next, input)?;
                array_value.insert(0, ArrayPart::Substitution(subs));
                Ok(Value::Array(array_value))
            },
            _ => {
                unimplemented!("not yet implemented")
            },
        }
    } else {
        Ok(Value::Substitution(subs))
    }
}

fn parse_substitution(pair: Pair<Rule>) -> Result<Substitution, BoxError> {
    let position = pair.as_span().start_pos().line_col();
    if let Some(inner) = pair.into_inner().next() {
        match inner.as_rule() {
            Rule::required_substitution => {
                let key_path = process_field_paths(inner)?;
                Ok(Substitution::Required(key_path))

            },
            Rule::optional_substitution => {
                let key_path = process_field_paths(inner)?;
                Ok(Substitution::Optional(key_path))
            },
            _ => {
                Err(format!("expected optional or required substitution, pos {:?}", position).into())
            },
        }
    } else {
        Err(format!("expected a substitution, pos {:?}", position).into())
    }
}

fn parse_object(object: Pair<Rule>) -> Result<Object, BoxError> {
    match object.into_inner().next() {
        Some(body) => parse_object_body(body),
        None => Ok(Object::default())
    }
}

fn parse_field(field: Pair<Rule>) -> Result<FieldOp, BoxError> {
    let position = field.as_span().start_pos().line_col();
    let mut content = field.into_inner();

    let path = if let Some(field_path) = content.next() {
        process_field_path(field_path)?
    } else {
        return Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into());
    };

    if let Some(pair) = content.next() {
        let position = pair.as_span().start_pos().line_col();
        match pair.as_rule() {
            Rule::field_append => {
                if let Some(value_pair) = content.next() {
                    parse_value(value_pair).map(|(value, _deps)| FieldOp::Append(path, value))
                } else {
                    Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
                }
            }
            Rule::field_assign => {
                if let Some(value_pair) = content.next() {
                    parse_value(value_pair).map(|(value, _deps)| FieldOp::Assign(path, value))
                } else {
                    Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
                }
            }
            Rule::object => {
                let obj = parse_object(pair)?;
                Ok(FieldOp::Assign(path, Value::Object(obj)))
            }
            _ => unreachable!()
        }
    } else {
        Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
    }
}

/// If an object has a body it must contain at least one field or an include directive.
fn parse_object_body(body: Pair<Rule>) -> Result<Object, BoxError> {
    let mut result = Object::default();
    let content = body.into_inner();
    for pair in content {
        let position = pair.as_span().start_pos().line_col();
        match pair.as_rule() {
            Rule::field => {
                let field = parse_field(pair)?;
                result.append(field);
            },
            Rule::include => {
                let incl = parse_include(pair)?;
                result.append(FieldOp::Incl(incl));
            },
            _ => unreachable!()
        }
    }
    Ok(result)
}

fn process_string(string: Pair<Rule>) -> Result<String, BoxError> {
    let position = string.as_span().start_pos().line_col();
    let mut inners = string.into_inner();
    if let Some(inner) = inners.next() {
        // TODO(zolkko): unescape the value
        Ok(inner.as_str().to_owned())
    } else {
        Err(format!("a string must have a content, pos {:?}", position).into())
    }
}

fn process_field_paths(pair: Pair<Rule>) -> Result<Vec<String>, BoxError> {
    let position = pair.as_span().start_pos().line_col();
    let mut content = pair.into_inner();

    if let Some(field_path) = content.next() {
        process_field_path(field_path)
    } else {
        Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
    }
}

fn process_field_path(pair: Pair<Rule>) -> Result<Vec<String>, BoxError> {
    let mut path = Vec::new();
    for key in pair.into_inner() {
        match key.as_rule() {
            Rule::field_name => path.push(key.as_str().to_owned()),
            Rule::string => path.push(process_string(key)?),
            _ => unreachable!()
        }
    }
    Ok(path)
}

/// A hocon string may contain a substitution, so we have to split the string into
/// parts.
fn process_string_value(current_pair: Pair<Rule>, mut input: Pairs<Rule>, string_repr: &str) -> Result<Value, BoxError> {
    let offset = current_pair.as_span().start();

    let mut string_parts: Vec<StringPart> = Vec::new();
    let mut result = String::new();

    // There could be multiple unquoted strings followed one another,
    // this variable holds the position of the first unquoted string in the sequence.
    let mut unquoted_seq: Option<(usize, usize)> = None;
    let mut last_substitution_end: Option<usize> = None;

    {
        let position = current_pair.as_span().start_pos().line_col();

        match current_pair.as_rule() {
            Rule::unquoted_string => {
                let span = current_pair.as_span();
                unquoted_seq = Some((span.start(), span.end()))
            },
            Rule::string | Rule::mstring => {
                result += &process_string(current_pair)?;
            },
            _ => {
                return Err(format!("quoted or unquoted string value is expected, pos {:?}", position).into());
            },
        }
    }

    while let Some(pair) = input.next() {
        let position = pair.as_span().start_pos().line_col();

        match pair.as_rule() {
            Rule::unquoted_string => {
                unquoted_seq = match unquoted_seq {
                    Some((s, e)) => Some((s, pair.as_span().end())),
                    None => {
                        let span = pair.as_span();
                        Some((span.start(), span.end()))
                    }
                };
            },
            Rule::string | Rule::mstring => {
                last_substitution_end = None;
                if let Some((s, e)) = unquoted_seq {
                    result += &string_repr[(s - offset)..(e - offset)];
                    unquoted_seq = None;
                }
                result += &process_string(pair)?;
            },
            Rule::substitution => {
                if let Some(inner) = pair.into_inner().next() {

                    if let Some((s, e)) = unquoted_seq {
                        let current_start = inner.as_span().start();
                        result += &string_repr[(s - offset)..(current_start - offset)];
                        unquoted_seq = None;
                    }

                    let span = inner.as_span();
                    last_substitution_end = Some(span.end());

                    let position = span.start_pos().line_col();

                    string_parts.push(StringPart::String(result));
                    string_parts.push(match inner.as_rule() {
                        Rule::required_substitution => {
                            let key_path = process_field_paths(inner)?;
                            StringPart::Substitution(Substitution::Required(key_path))
                        },
                        Rule::optional_substitution => {
                            let key_path = process_field_paths(inner)?;
                            StringPart::Substitution(Substitution::Optional(key_path))
                        },
                        _ => {
                            return Err(format!("expected optional or required substitution, pos {:?}", position).into())
                        },
                    });
                    result = String::new();
                } else {
                    return Err(format!("expected optional or required substitution, pos {:?}", position).into())
                }
            },
            _ => {
                return Err(format!("cannot concatenate the value, expected string, pos {:?}", position).into());
            },
        }
    }

    if let Some((s, e)) = unquoted_seq {
        let part = string_repr[(s - offset)..(e - offset)].to_owned();
        string_parts.push(StringPart::String(part));
    }

    if !result.is_empty() {
        string_parts.push(StringPart::String(result));
    }

    Ok(Value::String(string_parts))
}

fn parse_include(include: Pair<Rule>) -> Result<Include, BoxError> {
    let position = include.as_span().start_pos().line_col();
    let pair = include.into_inner().next().expect("regular include or required include, malformed grammar");
    match pair.as_rule() {
        Rule::required_include => {
            let pair = pair.into_inner().next().expect("regular_include");
            parse_include_path(pair).map(|i| Include::Required(i))
        },
        Rule::regular_include => {
            parse_include_path(pair).map(|i| Include::NonRequired(i))
        },
        _ => {
            Err(format!("expected required include or regular include, pos {:?}", position).into())
        },
    }
}

fn parse_include_path(pair: Pair<Rule>) -> Result<IncludePath, BoxError> {
    let position = pair.as_span().start_pos().line_col();
    let include_kind = if let Some(ik) = pair.into_inner().next() {
        ik
    } else {
        return Err(format!("include directive must be followed by either file(), url(), classpath() or single-quoted string, pos {:?}", position).into());
    };

    let position = include_kind.as_span().start_pos().line_col();
    match include_kind.as_rule() {
        Rule::include_file => {
            let maybe_string = include_kind.into_inner().next();
            if let Some(string) = maybe_string {
                let value = process_string(string)?;
                Ok(IncludePath::File(value))
            } else {
                Err(format!("include file() directive must contain a single-quoted string, pos {:?}", position).into())
            }
        },
        Rule::include_url => {
            let maybe_string = include_kind.into_inner().next();
            if let Some(string) = maybe_string {
                let value = process_string(string)?;
                Ok(IncludePath::Url(value))
            } else {
                Err(format!("include url() directive must contain a single-quoted string, pos {:?}", position).into())
            }
        },
        Rule::include_classpath => {
            let maybe_string = include_kind.into_inner().next();
            if let Some(string) = maybe_string {
                let value = process_string(string)?;
                Ok(IncludePath::Classpath(value))
            } else {
                Err(format!("include classpath() directive must contain a single-quoted string, pos {:?}", position).into())
            }
        },
        Rule::include_string => {
            let maybe_string = include_kind.into_inner().next();
            if let Some(string) = maybe_string {
                let value = process_string(string)?;
                Ok(IncludePath::SingleQuoted(value))
            } else {
                Err(format!("single-quoted include directive must contain a single-quoted string, pos {:?}", position).into())
            }
        },
        _ => {
            Err(format!("include directive must be followed by either file(), url(), classpath() or single-quoted string, pos {:?}", position).into())
        },
    }
}

fn is_self_reference(a: &[String], b: &[String]) -> bool {
    if a.len() <= b.len() {
        a.iter().zip(b).all(|(x, y)| x == y)
    } else {
        false
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse_array() {
        let array_ast = AstParser::parse(Rule::array, r#"[1,2,3]"#).unwrap().next().unwrap();
        let array = parse_array(array_ast).expect("must parse array");
        assert_eq!(array, Array(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]));
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
        ];

        for (example, expected) in examples.iter() {
            let value_ast = AstParser::parse(Rule::value, example).unwrap().next().unwrap();
            let (array_value, _) = parse_value(value_ast).expect("must parse array");
            assert_eq!(&array_value, expected);
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
            let res = process_field_path(field_path_pair).expect(format!("failed to parse {}", input).as_ref());
            assert_eq!(&res, expected);
        }
    }
}