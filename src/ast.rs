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

/// An object may consists of one or more objects and substitution expressions,
/// that later are merged into a resolved object.
#[derive(PartialEq, Clone, Debug)]
pub(crate) enum ObjectPart {
    Object(Object),
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
    Object(Vec<ObjectPart>),
    Substitution(Vec<Substitution>),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum FieldOp {
    Assign(Path, Value),
    Append(Path, Value),
    Incl(Include),
}

#[derive(Default, PartialEq, Clone, Debug)]
pub(crate) struct Object {
    fields: HashMap<String, Value>,
}

impl Object {
    fn append(&mut self, field: FieldOp) -> Result<(), BoxError> {
        match field {
            FieldOp::Assign(path, value) => self.assign_value(&path, value),
            FieldOp::Append(path, value) => self.append_value(&path, value),
            FieldOp::Incl(include) => {
                match include {
                    Include::Required(path) => {
                    },
                    Include::NonRequired(path) => {

                    },
                }
            },
        }
    }

    fn assign_value(&mut self, path: &[String], value: Value) -> Result<(), BoxError> {
        if let Some((key, tail)) = path.split_first() {
            if tail.is_empty() {
                self.fields.insert(key.clone(), value);
                Ok(())
            } else {
                if let Some(Value::Object(object_parts)) = self.fields.get_mut(key.as_str()) {
                    if let Some((ObjectPart::Object(last_object), _)) = object_parts.split_last_mut() {
                        last_object.assign_value(tail, value)
                    } else {
                        let mut last_object = Object::default();
                        let _ = last_object.assign_value(tail, value)?;
                        object_parts.push(ObjectPart::Object(last_object));
                        Ok(())
                    }
                } else {
                    let mut sub_obj = Object::default();
                    let _ = sub_obj.assign_value(tail, value)?;
                    self.fields.insert(key.clone(), Value::Object(vec![ObjectPart::Object(sub_obj)]));
                    Ok(())
                }
            }
        } else {
            Err("empty path".into())
        }
    }

    fn append_value(&mut self, path: &[String], value: Value) -> Result<(), BoxError> {
        if let Some((key, tail)) = path.split_first() {
            if tail.is_empty() {
                if let Some(existing_value) = self.fields.get_mut(key) {
                    match existing_value {
                        Value::Array(array) => {
                            array.push(ArrayPart::Array(Array(vec![value])));
                            Ok(())
                        },
                        Value::Substitution(subsitutions) => {
                            let mut parts: Vec<_> = subsitutions.iter().cloned().map(|v| ArrayPart::Substitution(v)).collect();
                            parts.push(ArrayPart::Array(Array(vec![value])));
                            self.fields.insert(key.clone(), Value::Array(parts));
                            Ok(())
                        },
                        _ => Err("incompatible type".into()),
                    }
                } else {
                    // if an element is not exist then we create a new array which
                    // consists of a single element
                    self.fields.insert(key.clone(), Value::Array(vec![ArrayPart::Array(Array(vec![value]))]));
                    Ok(())
                }
            } else {
                match self.fields.get_mut(key.as_str()) {
                    Some(Value::Object(object_parts)) => {
                        if let Some((ObjectPart::Object(last_object), _)) = object_parts.split_last_mut() {
                            last_object.append_value(tail, value)
                        } else {
                            let mut object = Object::default();
                            let _ = object.append_value(tail, value)?;
                            object_parts.push(ObjectPart::Object(object));
                            Ok(())
                        }
                    },
                    None => {
                        let mut object = Object::default();
                        let _ = object.append_value(tail, value)?;
                        self.fields.insert(key.clone(), Value::Object(vec![ObjectPart::Object(object)]));
                        Ok(())
                    },
                    _ => Err("invalid path type".into()),
                }
            }
        } else {
            Err("empty path".into())
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

/// Type of handler function which must return content of a file/url to include.
/// The first argument is a type of a resource to include and the second is a path or url.
pub(crate) type IncludeHandler = for<'a> fn (Include) -> Result<Option<Object>, BoxError>;

/// Parser object
#[derive(Parser)]
#[grammar = "hocon.pest"]
pub(crate) struct AstParser {
    include_handler: Option<IncludeHandler>
}

impl Default for AstParser {
    fn default() -> Self {
        AstParser::new()
    }
}

impl AstParser {
    pub fn new() -> Self {
        AstParser { include_handler: None }
    }

    pub fn with_handler(f: IncludeHandler) -> Self {
        AstParser { include_handler: Some(f) }
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
                parse_array(pair).map(|v| Value::Array(vec![ArrayPart::Array(v)]))
            },
            Rule::object => {
                parse_object(pair).map(|v| Value::Object(vec![ObjectPart::Object(v)]))
            },
            Rule::object_body => {
                parse_object_body(pair).map(|v| Value::Object(vec![ObjectPart::Object(v)]))
            },
            _ => {
                unreachable!("root grammar rule does not correspond to processing logic")
            },
        }
    }
}

fn parse_value(value: Pair<Rule>) -> Result<Value, BoxError> {
    let value_chunks = value.into_inner();
    let mut input = value_chunks.clone();

    if let Some(pair) = input.next() {
        let position = pair.as_span().start_pos().line_col();

        match pair.as_rule() {
            Rule::null => {
                Ok(Value::Null)
            },
            Rule::bool_true => {
                Ok(Value::Bool(true))
            },
            Rule::bool_false => {
                Ok(Value::Bool(false))
            },
            Rule::float => {
                match pair.as_str().parse() {
                    Ok(v) => Ok(Value::Float(v)),
                    Err(error) => Err(format!("{:?} {:?}", error, position).into()),
                }
            },
            Rule::int => {
                match pair.as_str().parse() {
                    Ok(v) => Ok(Value::Integer(v)),
                    Err(error) => Err(format!("{:?} {:?}", error, position).into()),
                }
            },
            Rule::unquoted_string | Rule::string | Rule::mstring => {
                parse_string_parts(pair, input).map(|v| Value::String(v))
            },
            Rule::object => {
                parse_object_parts(pair, input).map(|v| Value::Object(v))
            },
            Rule::array => {
                parse_arrays(pair, input).map(|v| Value::Array(v))
            },
            Rule::substitution => {
                parse_substitutions(pair, input)
            },
            _ => {
                unreachable!("grammar rule definitions do not correspond to the source code")
            },
        }
    } else {
        Ok(Value::Null)
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
        result.append(parse_value(array_item)?);
    }
    Ok(result)
}

/// If a string contain an escape sequence, the sequence must be decoded by the parser.
fn unescape_string(string: &str) -> String {
    // TODO: unescape string
    string.to_owned()
}

/// The function handles a single or multi quoted string
fn parse_string(string: Pair<Rule>) -> Result<String, BoxError> {
    let position = string.as_span().start_pos().line_col();
    let mut inners = string.into_inner();
    if let Some(inner) = inners.next() {
        Ok(unescape_string(inner.as_str()))
    } else {
        // TODO: otherwise this is a error in the parser grammar, so this is unreachable
        Err(format!("a string must have a content, pos {:?}", position).into())
    }
}

/// A string value may consists of multiple sub-strings and substitutions.
fn parse_string_parts(pair: Pair<Rule>, pairs: Pairs<Rule>) -> Result<Vec<StringPart>, BoxError> {
    let mut result = Vec::new();
    let mut last: Option<String> = None;

    match pair.as_rule() {
        Rule::substitution => {
            result.push(StringPart::Substitution(parse_substitution(pair)?));
        },
        Rule::string | Rule::mstring => {
            result.push(StringPart::String(parse_string(pair)?));
        },
        Rule::unquoted_string => {
            last = Some(pair.as_str().to_owned());
        },
        _ => {
            unreachable!("string, unquoted_string or mstring expected");
        },
    }

    for pair in pairs {
        match pair.as_rule() {
            Rule::substitution => {
                if let Some(l) = last.take() {
                    result.push(StringPart::String(unescape_string(l.trim_end())));
                }
                result.push(StringPart::Substitution(parse_substitution(pair)?))
            },
            Rule::string | Rule::mstring => {
                if let Some(l) = last.take() {
                    result.push(StringPart::String(unescape_string(l.trim_end())));
                }
                result.push(StringPart::String(parse_string(pair)?))
            },
            Rule::unquoted_string => {
                if let Some(l) = last.take() {
                    result.push(StringPart::String(unescape_string(l.as_ref())));
                }
                last = Some(pair.as_str().to_owned());
            },
            _ => {
                unreachable!("string, unquoted_string or mstring expected");
            },
        }
    }

    if let Some(l) = last.take() {
        result.push(StringPart::String(unescape_string(l.trim_end())));
    }

    Ok(result)
}

fn parse_substitutions(current_pair: Pair<Rule>, mut input: Pairs<Rule>) -> Result<Value, BoxError> {
    let substitution = parse_substitution(current_pair)?;
    let mut substitutions = vec![substitution];
    loop {
        if let Some(next) = input.next() {
            match next.as_rule() {
                Rule::substitution => {
                    substitutions.push(parse_substitution(next)?);
                },
                Rule::string | Rule::unquoted_string | Rule::mstring => {
                    let mut result_string: Vec<_> = substitutions.into_iter().map(|s| StringPart::Substitution(s)).collect();
                    result_string.extend(parse_string_parts(next, input)?);
                    return Ok(Value::String(result_string));
                },
                Rule::array => {
                    let mut result_array: Vec<_> = substitutions.into_iter().map(|s| ArrayPart::Substitution(s)).collect();
                    result_array.extend(parse_arrays(next, input)?);
                    return Ok(Value::Array(result_array));
                },
                Rule::object => {
                    let mut result_object: Vec<_> = substitutions.into_iter().map(|s| ObjectPart::Substitution(s)).collect();
                    result_object.extend(parse_object_parts(next, input)?);
                    return Ok(Value::Object(result_object));
                },
                _ => {
                    unreachable!("substitution, string, array or object expected");
                },
            }
        } else {
            return Ok(Value::Substitution(substitutions));
        }
    }
}

fn parse_substitution(pair: Pair<Rule>) -> Result<Substitution, BoxError> {
    let position = pair.as_span().start_pos().line_col();
    if let Some(inner) = pair.into_inner().next() {
        match inner.as_rule() {
            Rule::required_substitution => {
                let key_path = parse_field_paths(inner)?;
                Ok(Substitution::Required(key_path))

            },
            Rule::optional_substitution => {
                let key_path = parse_field_paths(inner)?;
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

fn parse_object_parts(current_pair: Pair<Rule>, mut input: Pairs<Rule>) -> Result<Vec<ObjectPart>, BoxError> {
    let mut object_parts = vec![ObjectPart::Object(parse_object(current_pair)?)];
    for pair in input {
        let part = match pair.as_rule() {
            Rule::object => ObjectPart::Object(parse_object(pair)?),
            Rule::substitution => ObjectPart::Substitution(parse_substitution(pair)?),
            _ => unreachable!("grammar rule definitions do not correspond to the source code"),
        };
        object_parts.push(part);
    }
    Ok(object_parts)
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
        parse_field_path(field_path)?
    } else {
        return Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into());
    };

    if let Some(pair) = content.next() {
        let position = pair.as_span().start_pos().line_col();
        match pair.as_rule() {
            Rule::field_append => {
                if let Some(value_pair) = content.next() {
                    parse_value(value_pair).map(|value| FieldOp::Append(path, value))
                } else {
                    Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
                }
            },
            Rule::field_assign => {
                if let Some(value_pair) = content.next() {
                    parse_value(value_pair).map(|value| FieldOp::Assign(path, value))
                } else {
                    Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
                }
            },
            Rule::object => {
                let object = parse_object(pair)?;
                Ok(FieldOp::Assign(path, Value::Object(vec![ObjectPart::Object(object)])))
            },
            _ => unreachable!(),
        }
    } else {
        Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
    }
}

/// If an object has a body it must contain at least one field or an include directive.
fn parse_object_body(body: Pair<Rule>) -> Result<Object, BoxError> {
    let mut result = Object::default();
    for pair in body.into_inner() {
        let position = pair.as_span().start_pos().line_col();
        match pair.as_rule() {
            Rule::field => {
                let field = parse_field(pair)?;
                let _ = result.append(field)?;
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

fn parse_field_paths(pair: Pair<Rule>) -> Result<Vec<String>, BoxError> {
    let position = pair.as_span().start_pos().line_col();
    let mut content = pair.into_inner();

    if let Some(field_path) = content.next() {
        parse_field_path(field_path)
    } else {
        Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
    }
}

fn parse_field_path(pair: Pair<Rule>) -> Result<Vec<String>, BoxError> {
    let mut path = Vec::new();
    for key in pair.into_inner() {
        match key.as_rule() {
            Rule::field_name => path.push(key.as_str().to_owned()),
            Rule::string => path.push(parse_string(key)?),
            _ => unreachable!()
        }
    }
    Ok(path)
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
                let value = parse_string(string)?;
                Ok(IncludePath::File(value))
            } else {
                Err(format!("include file() directive must contain a single-quoted string, pos {:?}", position).into())
            }
        },
        Rule::include_url => {
            let maybe_string = include_kind.into_inner().next();
            if let Some(string) = maybe_string {
                let value = parse_string(string)?;
                Ok(IncludePath::Url(value))
            } else {
                Err(format!("include url() directive must contain a single-quoted string, pos {:?}", position).into())
            }
        },
        Rule::include_classpath => {
            let maybe_string = include_kind.into_inner().next();
            if let Some(string) = maybe_string {
                let value = parse_string(string)?;
                Ok(IncludePath::Classpath(value))
            } else {
                Err(format!("include classpath() directive must contain a single-quoted string, pos {:?}", position).into())
            }
        },
        Rule::include_string => {
            let maybe_string = include_kind.into_inner().next();
            if let Some(string) = maybe_string {
                let value = parse_string(string)?;
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
                Object { fields }
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
}