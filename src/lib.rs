use std::collections::HashMap;
use std::error::Error;


use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;


mod error;
mod parser;
mod value;
pub mod de;

mod ast;

mod tsort;

use self::value::{Array, Object, Value, ObjectOps};
use self::error::HoconError;
use std::collections::hash_map::Values;


type KeyPath = Vec<String>;

#[derive(Debug, PartialEq)]
enum SubstitutionEntry {
    Resolved(Value),
    Required(KeyPath),
    Optional(KeyPath)
}

#[derive(Debug, PartialEq)]
enum ValueT {
    Resolved(Value),
    Substitution(Vec<SubstitutionEntry>)
}

/// When an "include" directive is encountered in a hocon file,
/// a user is asked to provide it's raw content to the parser.
///
/// All the files are resolved
pub enum IncludePath {
    SingleQuoted(String),
    Url(String),
    File(String),
    Classpath(String)
}

/// Type of handler function which must return content of a file/url to include.
/// The first argument is a type of a resource to include and the second is a path or url.
type IncludeHandler = for<'a> fn (IncludePath) -> Result<String, Box<dyn Error>>;


/// Parser object
#[derive(Parser)]
#[grammar = "hocon.pest"]
pub struct HoconParser {
    include_handler: Option<IncludeHandler>
}

impl Default for HoconParser {
    fn default() -> Self {
        HoconParser::new()
    }
}

impl HoconParser {

    pub fn new() -> Self {
        HoconParser { include_handler: None }
    }

    pub fn with_include_handler(handler: IncludeHandler) -> Self {
        HoconParser { include_handler: Some(handler) }
    }

    /// Parses a string slice into owned hocon object.
    /// Similar to JSON format, Hocon allows arrays and objects to be defined on the top level.
    pub fn parse_str(&self, input: &str) -> Result<Value, HoconError<Rule>> {
        let mut parsed = HoconParser::parse(Rule::root, input)?;

        let root_pair = parsed.next().unwrap();
        let mut pairs = root_pair.into_inner();
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            Rule::array => {
                let array = extract_array(pair)?;
                Ok(Value::Array(array))
            },
            Rule::object => {
                let object = extract_object(pair)?.unwrap_or_else(|| Object::default());
                Ok(Value::Object(object))
            },
            Rule::object_body => {
                let object = extract_object_body(pair)?;
                Ok(Value::Object(object))
            },
            _ => unreachable!("root grammar rule does not correspond to processing logic")
        }
    }

    fn parse_include_root(&self, input: &str) -> Result<Object, HoconError<Rule>> {
        let mut parsed = HoconParser::parse(Rule::root, input)?;

        let root_pair = parsed.next().unwrap();
        let mut pairs = root_pair.into_inner();
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            Rule::object => {
                Ok(extract_object(pair)?.unwrap_or_else(|| Object::default()))
            }
            Rule::object_body => {
                Ok(extract_object_body(pair)?)
            }
            _ => {
                Err(((0, 0), "root element of an included file must be an object").into())
            }
        }
    }

    fn process_include(&self, include: Pair<Rule>) -> Option<HashMap<String, Value>> {
        let result = HashMap::default();
        let pair = include.into_inner().next().expect("regular include or required include, malformed grammar");

        match pair.as_rule() {
            Rule::required_include => {
                if let Some(ref handler) = self.include_handler {
                    let pair = pair.into_inner().next().expect("regular_include");
                    self.process_regular_include(pair, handler)
                } else {
                    panic!("required include");
                }
            }
            Rule::regular_include => {
                if let Some(ref handler) = self.include_handler {
                    self.process_regular_include(pair, handler)
                }
            }
            _ => {
                panic!("expected required include or regular include")
            }
        }

        Some(result)
    }

    fn process_regular_include(&self, regular_include: Pair<Rule>, handler: &IncludeHandler) {
        let include_kind = self.extract_include_path(regular_include).expect("propagate the error");
        let obj = handler(include_kind);
    }

    /// Takes tokens recognized by the `regular_include` grammar rule and extracts an include path.
    fn extract_include_path(&self, pair: Pair<Rule>) -> Result<IncludePath, HoconError<Rule>> {
        let position = pair.as_span().start_pos().line_col();
        let include_kind = if let Some(ik) = pair.into_inner().next() {
            ik
        } else {
            return Err((position, "include directive must be followed by either file(), url(), classpath() or single-quoted string").into())
        };

        let position = include_kind.as_span().start_pos().line_col();
        match include_kind.as_rule() {
            Rule::include_file => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = process_string(string)?;
                    Ok(IncludePath::File(value))
                } else {
                    Err((position, "include file() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_url => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = process_string(string)?;
                    Ok(IncludePath::Url(value))
                } else {
                    Err((position, "include url() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_classpath => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = process_string(string)?;
                    Ok(IncludePath::Classpath(value))
                } else {
                    Err((position, "include classpath() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_string => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = process_string(string)?;
                    Ok(IncludePath::SingleQuoted(value))
                } else {
                    Err((position, "single-quoted include directive must contain a single-quoted string").into())
                }
            },
            _ => {
                Err((position, "include directive must be followed by either file(), url(), classpath() or single-quoted string").into())
            }
        }
    }

}

/// Extracts a string from a `string` or a `multi-line string` rules.
fn process_string(string: Pair<Rule>) -> Result<String, HoconError<Rule>> {
    let position = string.as_span().start_pos().line_col();
    let mut inners = string.into_inner();
    if let Some(inner) = inners.next() {
        // TODO(zolkko): unescape the value
        Ok(inner.as_str().to_owned())
    } else {
        Err((position, "a string must have a content").into())
    }
}

/// Process `value` grammar rule by concatenating a list of item into a single value.
///
/// A value can be either fully resolved or unresolved value.
fn extract_value(value: Pair<Rule>) -> Result<Value, HoconError<Rule>> {

    let value_chunks = value.into_inner();
    let mut input = value_chunks.clone();

    if let Some(pair) = input.next() {
        let position = pair.as_span().start_pos().line_col();

        match pair.as_rule() {
            Rule::null => {
                return Ok(Value::Null)
            }
            Rule::bool_true => {
                return Ok(Value::Bool(true))
            }
            Rule::bool_false => {
                return Ok(Value::Bool(false))
            }
            Rule::float => {
                let value = match pair.as_str().parse() {
                    Ok(v) => v,
                    Err(error) => return Err((position, error).into())
                };
                return Ok(Value::Float(value))
            }
            Rule::int => {
                let value = match pair.as_str().parse() {
                    Ok(v) => v,
                    Err(error) => return Err((position, error).into())
                };
                return Ok(Value::Integer(value))
            }
            Rule::unquoted_string => {
                return process_string_value(pair, input, value_chunks.as_str());
            }
            Rule::string | Rule::mstring => {
                return process_string_value(pair, input, value_chunks.as_str());
            }
            Rule::array => {
                let array = extract_array(pair)?;
                return concatenate_array(array, input);
            }
            Rule::object => {
                let object = extract_object(pair)?.unwrap_or_else(|| Object::default());
                return concatenate_object(object, input);
            }
            Rule::substitution => {
                Ok(Value::String("not yet implemented".to_owned()))
            }
            _ => {
                unreachable!("grammar rule definitions do not correspond to the source code")
            }
        }
    } else {
        Ok(Value::Null)
    }
}

///
fn process_substitution(current_pair: Pair<Rule>, mut input: Pairs<Rule>) -> Result<Value, HoconError<Rule>> {

    let position = current_pair.as_span().start_pos().line_col();

    match current_pair.as_rule() {
        Rule::substitution => {
            unimplemented!()
        },
        _ => {
            return Err((position, "expected optional or required substitution").into())
        }
    }

}

fn concatenate_object(mut result: Object, input: Pairs<Rule>) -> Result<Value, HoconError<Rule>> {
    for pair in input {
        let position = pair.as_span().start_pos().line_col();
        match pair.as_rule() {
            Rule::object => {
                if let Some(object) = extract_object(pair)? {
                    result.merge_object(&object);
                }
            }
            Rule::substitution => {
                unimplemented!()
            }
            _ => {
                return Err((position, "cannot concatenate the value, expected an object").into());
            }
        }
    }

    Ok(Value::Object(result))
}

fn concatenate_array(mut result: Array, input: Pairs<Rule>) -> Result<Value, HoconError<Rule>> {
    for pair in input {
        let position = pair.as_span().start_pos().line_col();
        match pair.as_rule() {
            Rule::array => {
                result.extend(extract_array(pair)?);
            }
            Rule::substitution => {
                unimplemented!()
            }
            _ => {
                return Err((position, "cannot concatenate the value, expected an array").into());
            }
        }
    }
    Ok(Value::Array(result))
}


fn process_field_path(pair: Pair<Rule>) -> Result<KeyPath, HoconError<Rule>> {

    let position = pair.as_span().start_pos().line_col();
    let mut content = pair.into_inner();

    if let Some(field_path) = content.next() {
        let paths = field_path.into_inner();
        let mut path = Vec::new();
        for key in paths {
            match key.as_rule() {
                Rule::field_name => path.push(key.as_str().to_owned()),
                Rule::string => path.push(process_string(key)?),
                _ => unreachable!()
            }
        }
        return Ok(path);
    } else {
        return Err((position, "field grammar rule does not correspond to extraction logic").into());
    }
}

/// Process a sequence of one or more strings.
///
/// Because `span` start and end positions are relative to entire input, here I also
/// pass an offset argument.
fn process_string_value(current_pair: Pair<Rule>, mut input: Pairs<Rule>, string_repr: &str) -> Result<Value, HoconError<Rule>> {

    let offset = current_pair.as_span().start();

    let mut result_with_subs: Vec<SubstitutionEntry> = Vec::new();
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
                return Err((position, "quoted or unquoted string value is expected").into());
            }
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

                    result_with_subs.push(SubstitutionEntry::Resolved(Value::String(result)));
                    result_with_subs.push(match inner.as_rule() {
                        Rule::required_substitution => SubstitutionEntry::Required(process_field_path(inner)?),
                        Rule::optional_substitution => SubstitutionEntry::Optional(process_field_path(inner)?),
                        _ => {
                            return Err((position, "expected optional or required substitution").into())
                        }
                    });

                    result = String::new();

                } else {
                    return Err((position, "expected optional or required substitution").into())
                }
            },
            _ => {
                return Err((position, "cannot concatenate the value, expected string").into());
            }
        }
    }

    if let Some((s, e)) = unquoted_seq {
        result += &string_repr[(s - offset)..(e - offset)];
    }

    if result_with_subs.is_empty() {
        Ok(Value::String(result))
    } else {

        let mut r = String::new();
        for i in result_with_subs {
            match i {
                SubstitutionEntry::Resolved(Value::String(s)) => {
                    r += &s;
                },
                SubstitutionEntry::Optional(ref key_path) => {
                    r += "%";
                    for (i, k) in key_path.iter().enumerate() {
                        if i > 0 {
                            r += ".";
                        }
                        r += k;
                    }
                    r += "%";
                },
                SubstitutionEntry::Required(ref key_path) => {
                    r += "%";
                    for (i, k) in key_path.iter().enumerate() {
                        if i > 0 {
                            r += ".";
                        }
                        r += k;
                    }
                    r += "%";
                },
                _ => {
                    r += "%";
                    r += "field";
                    r += "%";
                }
            }
        }

        if !result.is_empty() {
            r += &result;
        }

        Ok(Value::String(r))
    }
}

fn extract_array(array: Pair<Rule>) -> Result<Array, HoconError<Rule>> {
    let mut result: Array = Array::default();
    let array_values = array.into_inner();
    for array_item in array_values {
        let value = extract_value(array_item)?;
        result.push(value);
    }
    Ok(result)
}

/// An object may have an empty body, in this case the function returns `None`.
fn extract_object(object: Pair<Rule>) -> Result<Option<Object>, HoconError<Rule>> {
    match object.into_inner().next() {
        Some(body) => extract_object_body(body).map(|obj| Some(obj)),
        None => Ok(None)
    }
}

/// A value of a field can be either assigned or appended to an existing value.
#[derive(Debug, PartialEq)]
enum FieldOp {
    Assign(Vec<String>, Value),
    Append(Vec<String>, Value)
}

/// If an object has a body it must contain at least one field or an include directive.
fn extract_object_body(body: Pair<Rule>) -> Result<Object, HoconError<Rule>> {

    let mut result = Object::default();

    let content = body.into_inner();
    for pair in content {
        let position = pair.as_span().start_pos().line_col();
        match pair.as_rule() {
            Rule::field => {
                match extract_field(pair)? {
                    FieldOp::Assign(field_path, value) => {
                        result.assign_value(&field_path[..], value);
                    },
                    FieldOp::Append(field_path, value) => {
                        if let Err(error) = result.append_value(&field_path[..], value) {
                            return Err((position, error).into());
                        }
                    }
                }
            }
            Rule::include => {
                // TODO: merge included object into result
                unimplemented!("include operation is not implemented")
            }
            _ => unreachable!()
        }
    }

    Ok(result)
}

fn extract_field(field: Pair<Rule>) -> Result<FieldOp, HoconError<Rule>> {

    let position = field.as_span().start_pos().line_col();
    let mut content = field.into_inner();

    let path = if let Some(field_path) = content.next() {
        let paths = field_path.into_inner();
        let mut path = Vec::new();
        for key in paths {
            match key.as_rule() {
                Rule::field_name => path.push(key.as_str().to_owned()),
                Rule::string => path.push(process_string(key)?),
                _ => unreachable!()
            }
        }
        path
    } else {
        return Err((position, "field grammar rule does not correspond to extraction logic").into());
    };

    if let Some(pair) = content.next() {
        let position = pair.as_span().start_pos().line_col();
        match pair.as_rule() {
            Rule::field_append => {
                if let Some(value_pair) = content.next() {
                    extract_value(value_pair).map(|value| FieldOp::Append(path, value))
                } else {
                    Err((position, "field grammar rule does not correspond to extraction logic").into())
                }
            }
            Rule::field_assign => {
                if let Some(value_pair) = content.next() {
                    extract_value(value_pair).map(|value| FieldOp::Assign(path, value))
                } else {
                    Err((position, "field grammar rule does not correspond to extraction logic").into())
                }
            }
            Rule::object => {
                if let Some(obj) = extract_object(pair)? {
                    Ok(FieldOp::Assign(path, Value::Object(obj)))
                } else {
                    // TODO: assign nothing
                    Ok(FieldOp::Assign(path, Value::Object(Object::default())))
                }
            }
            _ => unreachable!()
        }
    } else {
        Err((position, "field grammar rule does not correspond to extraction logic").into())
    }
}

#[cfg(test)]
mod tests {

    use pest::{parses_to, consumes_to};
    use super::*;

    #[test]
    fn test_processing_quoted_strings() {
        let input = r#"field: "the string" " value""#;

        let parser = HoconParser::new();
        let result = parser.parse_str(input).expect("must parse the input");

        let expected_object = Value::Object({
            let mut obj = Object::new();
            obj.insert("field".to_owned(), Value::String("the string value".to_owned()));
            obj
        });

        assert_eq!(expected_object, result);
    }

    #[test]
    fn test_processing_unquoted_strings() {
        let input = r#"
            field: the string value
        "#;

        let parser = HoconParser::new();
        let result = parser.parse_str(input).expect("must parse the input");

        let expected_object = Value::Object({
            let mut obj = Object::new();
            obj.insert("field".to_owned(), Value::String("the string value".to_owned()));
            obj
        });

        assert_eq!(expected_object, result);
    }

    #[test]
    fn test_processing_mixed_strings() {
        let input = r#"
            field: the " string " value
        "#;

        let parser = HoconParser::new();
        let result = parser.parse_str(input).expect("must parse the input");

        let expected_object = Value::Object({
            let mut obj = Object::new();
            obj.insert("field".to_owned(), Value::String("the string value".to_owned()));
            obj
        });

        assert_eq!(expected_object, result);
    }

    /*#[test]
    fn test_processing_substitution_inside_string() {
        let input = r#"
            field: this   is   the    ${?subfield.name} value another
        "#;

        let parser = HoconParser::new();
        let result = parser.parse_str(input).expect("must parse the input");

        let expected_object = Value::Object({
            let mut obj = Object::new();
            obj.insert("field".to_owned(), Value::String("the string value another".to_owned()));
            obj
        });

        assert_eq!(expected_object, result);
    }*/

    #[test]
    fn test_extract_object() {
        let input = r#"requirements {
        field1 =
        akka.actor.mailbox.unbounded-queue-based   asdasd
        field2 = akka.actor.mailbox.bounded-queue-based
      }"#;

        let obj_pair = HoconParser::parse(Rule::root, input).unwrap().next().unwrap();

        let obj = extract_object(obj_pair);
        println!("{:#?}", obj);
    }

    #[test]
    fn test_extract_field() {
        let mut field_pairs = HoconParser::parse(Rule::field, "path: value").expect("must parse");
        let field_pair = field_pairs.next().expect("must parse the field");
        let res = extract_field(field_pair).expect("must extract");

        assert_eq!(res, FieldOp::Assign(vec!["path".to_owned()], Value::String("value".to_owned())));
    }

    #[test]
    fn test_parse_array() {
        parses_to! {
            parser: HoconParser,
            input: r#"[1, 2, 3]"#,
            rule:   Rule::array,
            tokens: [
                array(0, 9, [
                    value(1, 2, [int(1, 2)]),
                    value(4, 5, [int(4, 5)]),
                    value(7, 8, [int(7, 8)])
                ])
            ]
        };
    }

    #[test]
    fn test_extract_simple_array() {

        let mut array_pair = HoconParser::parse(Rule::array, r#"[1, foo   bar ": hello all "  quo "ted", 3]"#)
            .expect("must parse the array");

        let array = extract_array(array_pair.next().unwrap())
            .expect("must extract an array");

        assert_eq!(array, vec![
            Value::Integer(1),
            Value::String("foo   bar: hello all quoted".to_owned()),
            Value::Integer(3)
        ]);
    }

    #[test]
    fn test_extract_array_value() {
        let mut value_pairs = HoconParser::parse(Rule::value, r#"[1, 2, 3] [4, 5, 6]"#)
            .expect("must parse the value");

        let value_pair = value_pairs.next().expect("value expected");

        let value = extract_value(value_pair).expect("must concatenate");

        assert_eq!(value, Value::Array(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4),
            Value::Integer(5),
            Value::Integer(6)
        ]));
    }
}
