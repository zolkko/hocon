use std::default::Default;
use std::error::Error;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;


pub(crate) type BoxError = Box<dyn Error>;

pub(crate) type Path = Vec<String>;

/// Unquoted hocon string may contain a substitution inside it.
/// Thus a complete unresolved hocon string consists of multiple parts.
#[derive(PartialEq, Clone, Debug)]
pub(crate) enum StringPart {
    String(String),
    Substitution(Path),
    OptionalSubstitution(Path),
}

#[derive(PartialEq, Clone, Debug)]
pub(crate) enum Value {
    Null,
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(Vec<StringPart>),
    Array(Array),
    Object(Object),
    Substitution(Path),
    OptionalSubstitution(Path),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum FieldOp {
    Assign(Path, Value),
    Append(Path, Value)
}

#[derive(Default, PartialEq, Clone, Debug)]
pub(crate) struct Object(Vec<FieldOp>);

impl Object {
    fn append(&mut self, field: FieldOp) {
        self.0.push(field);
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
                let array = parse_array(pair)?;
                Ok(Value::Array(array))
            },
            Rule::object => {
                let object = parse_object(pair)?.unwrap_or_else(|| Object::default());
                Ok(Value::Object(object))
            },
            Rule::object_body => {
                let object = parse_object_body(pair)?;
                Ok(Value::Object(object))
            },
            _ => unreachable!("root grammar rule does not correspond to processing logic")
        }
    }

    /*
    fn parse_include_root(&self, input: &str) -> Result<Object, HoconError<Rule>> {
        let mut parsed = AstParser::parse(Rule::root, input)?;

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
    */
}

fn parse_array(array: Pair<Rule>) -> Result<Array, BoxError> {
    let mut result: Array = Array::default();
    let array_values = array.into_inner();
    for array_item in array_values {
        let value = parse_value(array_item)?;
        result.append(value);
    }
    Ok(result)
}

fn parse_value(value: Pair<Rule>) -> Result<Value, BoxError> {
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
                    Err(error) => {
                        return Err(format!("{:?} {:?}", error, position).into());
                    }
                };
                return Ok(Value::Float(value))
            }
            Rule::int => {
                let value = match pair.as_str().parse() {
                    Ok(v) => v,
                    Err(error) => {
                        return Err(format!("{:?} {:?}", error, position).into());
                    }
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
                return parse_array(pair).map(|arr| Value::Array(arr));
            }
            Rule::object => {
                parse_object(pair).map(|obj| {
                    Value::Object(obj.unwrap_or_else(|| Object::default()))
                })
            }
            Rule::substitution => {
                Ok(Value::String(vec![StringPart::String("not yet implemented".to_owned())]))
            }
            _ => {
                unreachable!("grammar rule definitions do not correspond to the source code")
            }
        }
    } else {
        Ok(Value::Null)
    }
}

fn parse_object(object: Pair<Rule>) -> Result<Option<Object>, BoxError> {
    match object.into_inner().next() {
        Some(body) => parse_object_body(body).map(|obj| Some(obj)),
        None => Ok(None)
    }
}

fn parse_field(field: Pair<Rule>) -> Result<FieldOp, BoxError> {
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
            }
            Rule::field_assign => {
                if let Some(value_pair) = content.next() {
                    parse_value(value_pair).map(|value| FieldOp::Assign(path, value))
                } else {
                    Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into())
                }
            }
            Rule::object => {
                if let Some(obj) = parse_object(pair)? {
                    Ok(FieldOp::Assign(path, Value::Object(obj)))
                } else {
                    // TODO: assign nothing
                    Ok(FieldOp::Assign(path, Value::Object(Object::default())))
                }
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

fn process_field_path(pair: Pair<Rule>) -> Result<Vec<String>, BoxError> {
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
        return Err(format!("field grammar rule does not correspond to extraction logic, pos {:?}", position).into());
    }
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
                            let key_path = process_field_path(inner)?;
                            StringPart::Substitution(key_path)
                        },
                        Rule::optional_substitution => {
                            let key_path = process_field_path(inner)?;
                            StringPart::OptionalSubstitution(key_path)
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
