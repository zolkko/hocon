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
pub enum IncludeKind {
    SingleQuoted,
    Url,
    File,
    Classpath
}

/// Type of handler function which must return content of a file/url to include.
/// The first argument is a type of a resource to include and the second is a path or url.
type IncludeHandler = for<'a> fn (IncludeKind, &'a str) -> Result<String, Box<dyn Error>>;

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

        let mut result = HashMap::default();

        let inner = rule.into_inner();
        println!("detected result: {:#?}", inner);

        for pair in inner {

            match pair.as_rule() {
                Rule::field => {
                    println!("processing a field")
                },
                Rule::include => {
                    println!("processing an include");
                    if let Some(included_object) = self.parse_include(pair) {
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

    fn parse_include(&self, include: Pair<Rule>) -> Option<HashMap<String, Value>> {
        let result = HashMap::default();
        let pair = include.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::required_include => {
                println!("required include");
                if let Some(ref handler) = self.include_handler {
                } else {
                    panic!("include is required");
                }
            }
            Rule::normal_include => {
                println!("normal include");
                if let Some(ref handler) = self.include_handler {

                    let inc = pair.into_inner().next().expect(r#"malformed "normal_include" grammar rule"#);

                    let kind = match inc.as_rule() {
                        Rule::include_file => IncludeKind::File,
                        Rule::include_url => IncludeKind::Url,
                        Rule::include_classpath => IncludeKind::Classpath,
                        Rule::include_string => IncludeKind::SingleQuoted,
                        _ => unreachable!()
                    };

                    let resource_path = inc.into_inner().next().expect("malformed grammar").into_inner().as_str();
                    let res = handler(kind, resource_path);
                }
            }
            _ => unreachable!()
        }

        Some(result)
    }
}


#[cfg(test)]
mod tests {

    use super::*;


    #[test]
    fn runme() {
        let parser = HoconParser { include_handler: Some(|k, s| Ok(" empty-string ".to_owned())) };
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
