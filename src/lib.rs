use std::collections::HashMap;
use std::error::Error;


use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;


mod error;
mod value;

use self::value::{Array, Object, Value, ObjectOps};
use self::error::HoconError;


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
        HoconParser { include_handler: None }
    }
}

impl HoconParser {

    /// Parses a string slice into owned hocon object.
    /// Similar to JSON format, Hocon allows arrays and objects to be defined on the top level.
    pub fn parse_str(&self, input: &str) -> Result<Value, HoconError<Rule>> {
        let mut parsed = HoconParser::parse(Rule::root, input)?;

        let mut root_pair = parsed.next().unwrap();
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

        let mut root_pair = parsed.next().unwrap();
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
                    let value = extract_string(string)?;
                    Ok(IncludePath::File(value))
                } else {
                    Err((position, "include file() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_url => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = extract_string(string)?;
                    Ok(IncludePath::Url(value))
                } else {
                    Err((position, "include url() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_classpath => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = extract_string(string)?;
                    Ok(IncludePath::Classpath(value))
                } else {
                    Err((position, "include classpath() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_string => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = extract_string(string)?;
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
fn extract_string(string: Pair<Rule>) -> Result<String, HoconError<Rule>> {
    let position = string.as_span().start_pos().line_col();
    let mut inners = string.into_inner();
    if let Some(inner) = inners.next() {
        // TODO(zolkko): unescape the value
        Ok(inner.as_str().to_owned())
    } else {
        Err((position, "a string must have a content").into())
    }
}

/// Process `value` gramma rule by concatenating a list of item into a single value.
/// This function also resolves substitutions.
fn concatenate_value(value: Pair<Rule>) -> Result<Value, HoconError<Rule>> {
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
                let span = pair.as_span();
                return concatenate_str(String::new(), Some((span.start(), span.end())), input, value_chunks.as_str(), span.start());
            }
            Rule::string | Rule::mstring => {
                let span = pair.as_span();
                let mut result = extract_string(pair)?.to_owned();
                return concatenate_str(result, None, input, value_chunks.as_str(), span.start());
            }
            Rule::array => {
                let mut array = extract_array(pair)?;
                return concatenate_array(array, input);
            }
            Rule::object => {
                let mut object = extract_object(pair)?.unwrap_or_else(|| Object::default());
                return concatenate_object(object, input);
            }
            Rule::substitution => {
                unimplemented!()
            }
            _ => {
                unreachable!("grammar rule definitions do not correspond to the source code")
            }
        }
    } else {
        Ok(Value::Null)
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

/// Because `span` start and end positions are relative to entire input, here I also
/// pass an offset argument.
fn concatenate_str(mut result: String, mut unquoted_seq: Option<(usize, usize)>, input: Pairs<Rule>, str_value: &str, offset: usize) -> Result<Value, HoconError<Rule>> {

    for pair in input {

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
            }
            Rule::string | Rule::mstring => {

                if let Some((s, e)) = unquoted_seq {
                    result += &str_value[(s - offset)..(e - offset)];
                    unquoted_seq = None;
                }

                result += &extract_string(pair)?;
            }
            Rule::substitution => {
                unimplemented!()
            }
            _ => {
                return Err((position, "cannot concatenate the value, expected s string").into());
            }
        }
    }

    if let Some((s, e)) = unquoted_seq {
        result += &str_value[(s - offset)..(e - offset)];
    }

    Ok(Value::String(result))
}

fn extract_array(array: Pair<Rule>) -> Result<Array, HoconError<Rule>> {
    let mut result: Array = Array::default();
    let array_values = array.into_inner();
    for array_item in array_values {
        let value = concatenate_value(array_item)?;
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


#[derive(Debug, PartialEq)]
enum FieldOp {
    Assign(Vec<String>, Value),
    Append(Vec<String>, Value)
}

/// If an object has a body it must contain at least one field or include directive.
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
                unimplemented!()
//                if let Some(included_object) = self.process_include(pair) {
//                    // TODO: merge included object into result
//                }
            }
            _ => unreachable!("object_body grammar rule does not correspond to extraction logic")
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
                Rule::string => path.push(extract_string(key)?),
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
                    concatenate_value(value_pair).map(|value| FieldOp::Append(path, value))
                } else {
                    Err((position, "field grammar rule does not correspond to extraction logic").into())
                }
            }
            Rule::field_assign => {
                if let Some(value_pair) = content.next() {
                    concatenate_value(value_pair).map(|value| FieldOp::Assign(path, value))
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

    static AKKA_CONF: &'static str = include_str!("resources/akka.conf");

    #[test]
    fn test_akka() {
        let pairs = HoconParser::parse(Rule::root, AKKA_CONF);

        println!("{:#?}", pairs);
        //assert!(false);
    }


    #[test]
    fn test_extract_object() {
        let input = r#"requirements {
        field1 =
        akka.actor.mailbox.unbounded-queue-based   asdasd
        field2 = akka.actor.mailbox.bounded-queue-based
      }"#;

        let mut obj_pair = HoconParser::parse(Rule::root, input).unwrap().next().unwrap();

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

        let mut array = Array::default();
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

        let value = concatenate_value(value_pair).expect("must concatenate");

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
