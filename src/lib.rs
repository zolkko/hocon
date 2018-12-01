use std::collections::HashMap;
use std::error::Error;


use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;


mod error;
mod value;

use self::value::Value;
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

impl HoconParser {
    pub fn parse_str(&self, input: &str) -> Result<Value, HoconError<Rule>> {
        let mut parsed = HoconParser::parse(Rule::root, input)?;

        let mut root_pair = parsed.next().unwrap(); // TODO(zolkko): HoconError
        let mut pairs = root_pair.into_inner();
        let pair = pairs.next().unwrap();

        match pair.as_rule() {
            Rule::array => Ok(self.parse_array(pair)),
            Rule::object => unimplemented!(),
            Rule::root_body => unimplemented!(),
            _ => unimplemented!()
        }
    }

    fn parse_array(&self, rule: Pair<Rule>) -> Value {

        let mut array: Vec<Value> = Vec::new();
        let inner = rule.into_inner();

        for pair in inner {

            let value: Value = match pair.as_rule() {
                Rule::null => {
                    Value::Null
                }
                Rule::bool_true => {
                    Value::Bool(true)
                }
                Rule::bool_false => {
                    Value::Bool(false)
                }
                Rule::float => {
                    let value = pair.as_str().parse().expect("cannot parse float");
                    Value::Float(value)
                }
                Rule::int => {
                    let value = pair.as_str().parse().expect("cannot parse integer");
                    Value::Integer(value)
                }
                Rule::string => {
                    let content= pair.into_inner();
                    Value::String(content.as_str().to_owned())
                }
                Rule::mstring => {
                    let content = pair.into_inner();
                    Value::String(content.as_str().to_owned())
                }
                Rule::array_raw_string => {
                    Value::String(pair.as_str().to_owned())
                }
                Rule::array => {
                    self.parse_array(pair)
                }
                Rule::object => {
                    self.parse_object(pair)
                }
                _ => {
                    unimplemented!()
                }
            };

            array.push(value);
        }

        Value::Array(array)
    }

    /// An object may not have a body.
    /// In this case an empty object is returned to the caller.
    fn parse_object(&self, rule: Pair<Rule>) -> Value {
        let maybe_body = rule.into_inner().next();
        if let Some(body) = maybe_body {
            self.parse_object_body(body)
        } else {
            Value::Object(HashMap::default())
        }
    }

    /// An object's body is a list of field with their corresponding values.
    fn parse_object_body(&self, rule: Pair<Rule>) -> Value {

        let result = HashMap::default();

        let inner = rule.into_inner();
        println!("detected result: {:#?}", inner);

        for pair in inner {

            match pair.as_rule() {
                Rule::field => {
                    println!("processing a field")
                },
                Rule::include => {
                    println!("processing an include");
                    if let Some(included_object) = self.process_include(pair) {
                        // TODO: merge included object into result
                    }
                }
                _ => unreachable!()
            }

            /*
            let mut content = field.into_inner();
            let field_name = content.next().unwrap();
            let nxt = content.next().unwrap();

            match nxt.as_rule() {
                Rule::field_assign => {
                    println!("assigning a value")
                }
                Rule::field_append => {
                    println!("appending a value")
                }
                Rule::object => {
                    println!("inplace definition")
                }
                _ => {
                    unreachable!()
                }
            }
            */
        }

        Value::Object(result)
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

    /// Takes a tokens recognized by the `regular_include` grammar rule
    /// and extracts an include path.
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
                    let value = self.extract_string(string)?;
                    Ok(IncludePath::File(value))
                } else {
                    Err((position, "include file() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_url => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = self.extract_string(string)?;
                    Ok(IncludePath::Url(value))
                } else {
                    Err((position, "include url() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_classpath => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = self.extract_string(string)?;
                    Ok(IncludePath::Classpath(value))
                } else {
                    Err((position, "include classpath() directive must contain a single-quoted string").into())
                }
            },
            Rule::include_string => {
                let maybe_string = include_kind.into_inner().next();
                if let Some(string) = maybe_string {
                    let value = self.extract_string(string)?;
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

    /// Extracts a string from a `string` or a multi-line string rules.
    fn extract_string(&self, string: Pair<Rule>) -> Result<String, HoconError<Rule>> {
        let position = string.as_span().start_pos().line_col();
        let mut inners = string.into_inner();
        if let Some(inner) = inners.next() {
            // TODO(zolkko): unescape the value
            Ok(inner.as_str().to_owned())
        } else {
            Err((position, "string must have a content").into())
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;


    #[test]
    fn runme() {
        let parser = HoconParser { include_handler: Some(|_k| Ok(" empty-string ".to_owned())) };
        let val = parser.parse_str(r###"

[1, 2, 3.14, {

// the body of the object
first_field = 1
second_field = 2
include file("some/file.hocon")
third_field += 3
four_field : 4
with_subobject {
  // this is sub object
}

}, "", true, [3, 2, 1], null
// this is a comment line
 """multi-line string""", raw string , 4


, ]

"###);
        println!("{:?}", val);
        assert!(false);
    }
}
