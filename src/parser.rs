use crate::ast::{Field, FieldOp, FieldOrInclude, Include, IncludePath, Object, Path, Substitution, Value};
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_while};
use nom::character::complete::{anychar, char, hex_digit1, line_ending, multispace0, multispace1, space0};
use nom::combinator::{eof, map, map_res, not, opt, recognize, value};
use nom::multi::{many0, many1, many_m_n};
use nom::number::complete::recognize_float;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;

fn comment(input: &str) -> IResult<&str, &str> {
    let start = alt((tag("//"), tag("#")));
    let comment = take_while(|x| x != '\n' && x != '\r');
    let end = alt((line_ending, eof));
    delimited(start, comment, end)(input)
}

fn empty_lines(input: &str) -> IResult<&str, ()> {
    let empty_line = alt((comment, multispace1));
    value((), many0(empty_line))(input)
}

/// ```peg
/// optional_subs_start = _{ "${?" }
/// ```
fn optional_substitution_start(input: &str) -> IResult<&str, &str> {
    tag("${?")(input)
}

/// ```peg
/// required_subs_start = _{ "${" }
/// ```
fn required_substitution_start(input: &str) -> IResult<&str, &str> {
    tag("${")(input)
}

/// ```peg
/// subs_end = _{ "}" }
/// ```
fn required_substitution_end(input: &str) -> IResult<&str, &str> {
    tag("}")(input)
}

/// ```peg
/// optional_substitution = { optional_subs_start ~ field_path ~ subs_end }
/// ```
fn optional_substitution(input: &str) -> IResult<&str, Substitution> {
    map(
        delimited(pair(optional_substitution_start, multispace0), field_path, pair(multispace0, required_substitution_end)),
        Substitution::Optional,
    )(input)
}

/// ```peg
/// required_substitution = { required_subs_start ~ field_path ~ subs_end }
/// ```
fn required_substitution(input: &str) -> IResult<&str, Substitution> {
    map(
        delimited(pair(required_substitution_start, multispace0), field_path, pair(multispace0, required_substitution_end)),
        Substitution::Required,
    )(input)
}

/// ```peg
/// substitution = { optional_substitution | required_substitution }
/// ```
fn substitution(input: &str) -> IResult<&str, Value> {
    map(alt((required_substitution, optional_substitution)), Value::Substitution)(input)
}

fn field_name_or_string(input: &str) -> IResult<&str, &str> {
    alt((field_name, string_content))(input)
}

fn field_name(input: &str) -> IResult<&str, &str> {
    recognize(many1(preceded(
        not(alt((
            tag("\""),
            tag(":"),
            tag("="),
            tag("+="),
            tag("{"),
            tag("}"),
            tag("["),
            tag("]"),
            tag(","),
            tag("."),
            tag("#"),
            tag("//"),
            multispace1,
        ))),
        anychar,
    )))(input)
}

fn path_delimiter(input: &str) -> IResult<&str, char> {
    char('.')(input)
}

/// Parse separators for arrays and objects.
///
/// Initial variant of the rule used to be:
/// ```ebnf
/// separator  = _{ ("," ~ NEWLINE*) | (NEWLINE+ ~ ","? ~ NEWLINE*) }
/// ```
fn separator(input: &str) -> IResult<&str, ()> {
    let comma = value((), pair(char(','), empty_lines));
    let prefix_comma = value((), tuple((line_ending, empty_lines, opt(char(',')), empty_lines)));
    preceded(space0, alt((comma, prefix_comma)))(input)
}

/// Parse array
/// ```ebnf
/// array = { "[" ~ NEWLINE* ~ value ~ (separator ~ value)* ~ (separator)? ~ "]" | "[" ~ NEWLINE* ~ "]" }
/// ```
fn array_value(input: &str) -> IResult<&str, Value> {
    let elements = opt(pair(value_chunks, terminated(many0(preceded(separator, value_chunks)), opt(separator))));
    let elements_value = map(elements, |maybe_x| if let Some(x) = maybe_x { Value::Array(combine_vec(x)) } else { Value::Array(vec![]) });
    delimited(pair(char('['), empty_lines), terminated(elements_value, empty_lines), pair(char(']'), space0))(input)
}

/// ```peg
/// field_assign = @{ "=" | ":" }
/// ```
fn field_assign(input: &str) -> IResult<&str, &str> {
    alt((tag("="), tag(":")))(input)
}

/// ```peg
/// field_append = @{ "+=" }
/// ```
fn field_append(input: &str) -> IResult<&str, &str> {
    tag("+=")(input)
}

/// ```peg
/// path_delimiter = _{ "." }
/// field_name = @{ (!("\"" | ":" | "=" | "+=" | "{" | "}" | "[" | "]" | "," | "." | NEWLINE | WHITESPACE | "#" | "//") ~ ANY)+ }
/// field_path = { (string | field_name) ~ (path_delimiter ~ (string | field_name))* }
/// ```
fn field_path(input: &str) -> IResult<&str, Path> {
    let parser = pair(field_name_or_string, many0(preceded(path_delimiter, field_name_or_string)));
    map(parser, combine_vec)(input)
}

/// ```ebnf
/// field = { field_path ~ NEWLINE* ~ (((field_assign | field_append) ~ NEWLINE* ~ value) | object) }
/// ```
fn field(input: &str) -> IResult<&str, Field> {
    #[derive(Clone)]
    enum Op {
        Assign,
        Append,
    }

    let assign_or_append = alt((value(Op::Assign, field_assign), value(Op::Append, field_append)));
    let field_value = map(separated_pair(assign_or_append, multispace0, value_chunks), |(op, val)| match op {
        Op::Assign => FieldOp::Assign(val),
        Op::Append => FieldOp::Append(val),
    });
    let pval = alt((field_value, map(object, FieldOp::Object)));
    let ppath = terminated(field_path, space0);

    map(pair(ppath, pval), Field::from)(input)
}

/// ```ebnf
/// field_or_include = _{ include | field }
/// ```
fn field_or_include(input: &str) -> IResult<&str, FieldOrInclude> {
    let include_parser = map(include, FieldOrInclude::Include);
    let field_parser = map(field, FieldOrInclude::Field);
    alt((include_parser, field_parser))(input)
}

/// ```peg
/// object_body = { field_or_include ~ (separator ~ field_or_include)* ~ separator? }
/// ```
fn object_body(input: &str) -> IResult<&str, Object> {
    // TODO ... comments and empty lines
    let parser = terminated(pair(field_or_include, many0(preceded(separator, field_or_include))), opt(separator));
    map(parser, combine_vec)(input)
}

/// ```peg
/// object = { "{" ~ NEWLINE* ~ object_body? ~ NEWLINE* ~ "}" }
/// ```
fn object(input: &str) -> IResult<&str, Object> {
    let object_body_parser = map(opt(object_body), |mo| match mo {
        Some(o) => o,
        None => vec![],
    });
    delimited(pair(char('{'), empty_lines), terminated(object_body_parser, empty_lines), pair(char('}'), space0))(input)
}

fn object_value(input: &str) -> IResult<&str, Value> {
    map(object, Value::Object)(input)
}

/// Parse value of an array or an object.
fn value_chunks(input: &str) -> IResult<&str, Vec<Value<'_>>> {
    let mut parser = many1(terminated(value_chunk, space0));
    parser(input)
}

/// ```peg
/// value_chunk = _{ array | object | substitution | unquoted_string }
/// ```
fn value_chunk(input: &str) -> IResult<&str, Value> {
    let number = map_res(recognize_float::<&str, _>, |x| {
        if x.contains('.') {
            str::parse(x).map(Value::Float).map_err(|err| format!("failed to parse double: {err}"))
        } else {
            str::parse(x).map(Value::Integer).map_err(|err| format!("failed to parse int: {err}"))
        }
    });

    let mut elems = alt((number, boolean, null, multi_string, string, object_value, array_value, substitution, unquoted_string));
    // terminated(elems, peek(spaced_element_end))(input)
    // terminated(elems, pair(space0, peek(element_end)))(input)
    elems(input)
}

/*
/// ```peg
/// element_end = _{ NEWLINE | EOI | "//" | "#" | "," | "]" | "}" }
/// ```
fn element_end(input: &str) -> IResult<&str, ()> {
    value((), alt((eof, line_ending, tag("//"), tag("#"), tag(","), tag("]"), tag("}"))))(input)
}
*/

/// ```peg
/// include_file = { ^"file(" ~ string ~ ")" }
/// ```
fn include_file(input: &str) -> IResult<&str, IncludePath> {
    let left = tuple((tag_no_case("file"), space0, char('('), multispace0));
    let right = pair(multispace0, tag(")"));
    map(delimited(left, string_content, right), IncludePath::File)(input)
}

/// ```peg
/// include_url = { ^"url(" ~ string ~ ")" }
/// ```
fn include_url(input: &str) -> IResult<&str, IncludePath> {
    let left = tuple((tag_no_case("url"), space0, tag("("), multispace0));
    let right = pair(multispace0, tag(")"));
    map(delimited(left, string_content, right), IncludePath::Url)(input)
}

/// ```peg
/// include_classpath = { ^"classpath(" ~ string ~ ")" }
/// ```
fn include_classpath(input: &str) -> IResult<&str, IncludePath> {
    map(
        delimited(
            tuple((tag_no_case("classpath"), space0, tag("("), multispace0)),
            string_content,
            pair(multispace0, tag(")")),
        ),
        IncludePath::Classpath,
    )(input)
}

/// ```peg
/// regular_include = { include_file | include_url | include_classpath | include_string }
/// ```
fn regular_include(input: &str) -> IResult<&str, Include> {
    let quoted = map(string_content, IncludePath::Quoted);
    map(alt((include_file, include_url, include_classpath, quoted)), Include::NonRequired)(input)
}

/// ```peg
/// required_include = { ^"required(" ~ regular_include ~ ")" }
/// ```
fn required_include(input: &str) -> IResult<&str, Include> {
    map(
        delimited(
            tuple((tag_no_case("required"), space0, tag("("), multispace0)),
            regular_include,
            pair(multispace0, tag(")")),
        ),
        |s| if let Include::NonRequired(x) = s { Include::Required(x) } else { s },
    )(input)
}

/// ```peg
/// include = { ^"include" ~ NEWLINE* ~ (required_include | regular_include) }
/// ```
fn include(input: &str) -> IResult<&str, Include> {
    preceded(pair(tag_no_case("include"), multispace1), alt((required_include, regular_include)))(input)
}

/// ```peg
/// unquoted_string_end = _{ "//" | NEWLINE | "$" | "\"" | "{" | "}" | "[" | "]" | ":" | "=" | "," | "+" | "#" | "`" | "^" | "?" | "!" | "@" | "*" | "&" | "\\" }
/// unquoted_string = @{ (!unquoted_string_end ~ ANY)+ }
/// ```
fn unquoted_string(input: &str) -> IResult<&str, Value> {
    let unquoted_string_end = alt((
        tag("//"),
        line_ending::<&str, _>,
        tag(" "),
        tag("\t"),
        tag("$"),
        tag("\""),
        tag("{"),
        tag("}"),
        tag("["),
        tag("]"),
        tag(","),
        tag("#"),
        // tag(":"),
        // tag("="),
        // tag("+"),
        // tag("`"),
        // tag("^"),
        // tag("?"),
        // tag("!"),
        // tag("@"),
        // tag("*"),
        // tag("&"),
        // tag("\\"),
    ));
    let parser = recognize(many1(preceded(not(unquoted_string_end), anychar)));
    map(parser, Value::String)(input)
}

/// ```peg
/// multi_string = ${ "\"\"\"" ~ minner ~ "\"\"\"" }
/// ms_inner  = @{ (!("\"\"\"") ~ ANY)* }
/// ```
fn multi_string(input: &str) -> IResult<&str, Value> {
    let multi_string_content = recognize(many0(preceded(not(tag::<_, &str, _>(r#"""""#)), anychar)));
    let parser_fn = delimited(tag(r#"""""#), multi_string_content, tag(r#"""""#));
    map(parser_fn, Value::String)(input)
}

/// ```peg
/// string = ${ "\"" ~ raw_string ~ "\"" }
/// ```
fn string(input: &str) -> IResult<&str, Value> {
    map(string_content, Value::String)(input)
}

fn string_content(input: &str) -> IResult<&str, &str> {
    delimited(char('"'), raw_string, char('"'))(input)
}

/// ```peg
/// raw_string = @{ (!("\"" | "\\") ~ ANY)* ~ (escape ~ raw_string)? }
/// ```
fn raw_string(input: &str) -> IResult<&str, &str> {
    let body = take_while(|ch| ch != '"' && ch != '\\');
    let rec = many0(pair(escape, raw_string));
    recognize(pair(body, rec))(input)
}

/// ```peg
/// escape = @{ "\\" ~ ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" | unicode) }
/// ```
fn escape(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        tag("\\"),
        alt((tag("\""), tag("\\"), tag("/"), tag("b"), tag("f"), tag("n"), tag("r"), tag("t"), unicode)),
    ))(input)
}

/// ```peg
/// unicode = @{ "u" ~ ASCII_HEX_DIGIT{4} }
/// ```
fn unicode(input: &str) -> IResult<&str, &str> {
    let ascii_hex_digit = recognize(many_m_n(1, 4, hex_digit1));
    preceded(char('u'), ascii_hex_digit)(input)
}

/// ```peg
/// true  = @{ ^"true" | ^"yes" | ^"y" | ^"t" }
/// false = @{ ^"false" | ^"no" | ^"n" | ^"f" }
/// bool  = _{ true | false }
/// ```
fn boolean(input: &str) -> IResult<&str, Value> {
    alt((
        value(
            Value::Bool(true),
            alt((tag_no_case("true"), tag_no_case("yes"), tag_no_case("t"), tag_no_case("y"), tag("1"))),
        ),
        value(
            Value::Bool(false),
            alt((tag_no_case("false"), tag_no_case("no"), tag_no_case("n"), tag_no_case("f"), tag("0"))),
        ),
    ))(input)
}

/// ```peg
/// null = @{ ^"null" }
/// ```
fn null(input: &str) -> IResult<&str, Value> {
    value(Value::Null, tag_no_case("null"))(input)
}

fn combine_vec<T>((head, mut rest): (T, Vec<T>)) -> Vec<T> {
    rest.reserve_exact(1);
    rest.insert(0, head);
    rest
}

pub(crate) fn root(input: &str) -> IResult<&str, Value> {
    let object_or_body = alt((object, object_body));
    let value = map(opt(object_or_body), |maybe_body| match maybe_body {
        Some(body) => Value::Object(body),
        None => Value::Object(vec![]),
    });
    let parser = delimited(empty_lines, value, empty_lines);
    terminated(parser, eof)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty_lines() {
        let res = empty_lines(
            r#"

        # comment 1


        # comment 2
        # comment 3


        // comment 4 and 5


        "#,
        );

        assert_eq!(res, Ok(("", ())));
    }

    #[test]
    fn test_parse_root() {
        let expected = Value::Object(vec![FieldOrInclude::Field(Field {
            path: vec!["name"],
            op: FieldOp::Assign(vec![Value::String("value")]),
        })]);
        assert_eq!(root("{ name: value }"), Ok(("", expected)));
    }

    #[test]
    fn test_parse_array() {
        assert_eq!(array_value("[]"), Ok(("", Value::Array(vec![]))));
        assert_eq!(array_value("[1]"), Ok(("", Value::Array(vec![vec![Value::Integer(1)]]))));
        assert_eq!(array_value("[1,2]"), Ok(("", Value::Array(vec![vec![Value::Integer(1)], vec![Value::Integer(2)]]))));
        assert_eq!(array_value("[1,2,]"), Ok(("", Value::Array(vec![vec![Value::Integer(1)], vec![Value::Integer(2)]]))));
    }

    #[test]
    fn test_parse_value_chunk() {
        assert_eq!(value_chunk("true"), Ok(("", Value::Bool(true))));
        assert_eq!(value_chunk("123"), Ok(("", Value::Integer(123))));
        assert_eq!(value_chunk("1.23"), Ok(("", Value::Float(1.23))));
        assert_eq!(value_chunk("[]"), Ok(("", Value::Array(vec![]))));
        assert_eq!(value_chunk("{}"), Ok(("", Value::Object(vec![]))));
        assert_eq!(value_chunk(r#""string1""#), Ok(("", Value::String("string1"))));
        assert_eq!(value_chunk(r#""""string2""""#), Ok(("", Value::String("string2"))));
        assert_eq!(value_chunk("string"), Ok(("", Value::String("string"))));
    }

    #[test]
    fn test_parse_value_chunks() {
        assert_eq!(
            value_chunks(r#""value1" "value2" "value3""#),
            Ok(("", vec![Value::String("value1"), Value::String("value2"), Value::String("value3"),]))
        );
        assert_eq!(
            value_chunks(r#""value1" "value2" "value3" "#),
            Ok(("", vec![Value::String("value1"), Value::String("value2"), Value::String("value3"),]))
        );
        assert_eq!(
            value_chunks(r#""value1" "value2" "value3","#),
            Ok((",", vec![Value::String("value1"), Value::String("value2"), Value::String("value3"),]))
        );
    }

    #[test]
    fn test_parse_field() {
        assert_eq!(
            field("name: value"),
            Ok((
                "",
                Field {
                    path: vec!["name"],
                    op: FieldOp::Assign(vec![Value::String("value")]),
                }
            ))
        );

        assert_eq!(
            field("name1.name2: value"),
            Ok((
                "",
                Field {
                    path: vec!["name1", "name2"],
                    op: FieldOp::Assign(vec![Value::String("value")]),
                }
            ))
        );
    }

    #[test]
    fn test_parse_object() {
        assert_eq!(object(r#"{ }"#), Ok(("", Object::default())));
        assert_eq!(
            object(r#"{ field1: 123 }"#),
            Ok((
                "",
                vec![FieldOrInclude::Field(Field {
                    path: vec!["field1"],
                    op: FieldOp::Assign(vec![Value::Integer(123)]),
                })]
            ))
        );
        assert_eq!(
            object("{ field1: 123 \n field2: 321 }"),
            Ok((
                "",
                vec![
                    FieldOrInclude::Field(Field {
                        path: vec!["field1"],
                        op: FieldOp::Assign(vec![Value::Integer(123)]),
                    }),
                    FieldOrInclude::Field(Field {
                        path: vec!["field2"],
                        op: FieldOp::Assign(vec![Value::Integer(321)]),
                    })
                ]
            ))
        );
    }

    #[test]
    fn test_parse_separator() {
        assert_eq!(separator(",next"), Ok(("next", ())));
        assert_eq!(separator(",   next"), Ok(("next", ())));
        assert_eq!(separator(", \nnext"), Ok(("next", ())));
        assert_eq!(separator("\n,\nnext"), Ok(("next", ())));
        assert_eq!(separator("\n \n, \nnext"), Ok(("next", ())));
        assert_eq!(separator("\n \n , \nnext"), Ok(("next", ())));
    }

    #[test]
    fn test_parse_field_path() {
        assert_eq!(field_path("field"), Ok(("", vec!["field"])));
        assert_eq!(field_path("field1.field2"), Ok(("", vec!["field1", "field2"])));
    }

    #[test]
    fn test_parse_include() {
        let res = include(r#"include required( file(  "file"   ) )"#);
        assert_eq!(res, Ok(("", Include::Required(IncludePath::File("file")))));

        let res = include(r#"include required  ( file(  "file"   ) )"#);
        assert_eq!(res, Ok(("", Include::Required(IncludePath::File("file")))));

        let res = include(r#"include required(url("url"))"#);
        assert_eq!(res, Ok(("", Include::Required(IncludePath::Url("url")))));

        let res = include(r#"include classpath("classpath")"#);
        assert_eq!(res, Ok(("", Include::NonRequired(IncludePath::Classpath("classpath")))));
    }

    #[test]
    fn test_parse_multi_string() {
        let res = multi_string(r#""""qwerty""""#);
        assert_eq!(res, Ok(("", Value::String("qwerty"))));

        let res = multi_string(r#""""qwerty"123""""#);
        assert_eq!(res, Ok(("", Value::String(r#"qwerty"123"#))));

        let res = multi_string(r#""""qwerty""123""""#);
        assert_eq!(res, Ok(("", Value::String(r#"qwerty""123"#))));

        let res = multi_string(r#""""qwerty"""123""""#);
        assert_eq!(res, Ok((r#"123""""#, Value::String("qwerty"))));

        let res = multi_string(r#""""""""#);
        assert_eq!(res, Ok(("", Value::String(""))));

        let res = multi_string(r#""""x""""#);
        assert_eq!(res, Ok(("", Value::String("x"))));

        let res = multi_string(r#""""xx""""#);
        assert_eq!(res, Ok(("", Value::String("xx"))));
    }

    #[test]
    fn test_parse_raw_string() {
        assert_eq!(raw_string(r#"\u1234\nend"#), Ok(("", r#"\u1234\nend"#)));
        assert_eq!(raw_string(r#"start\u1234\nend"#), Ok(("", r#"start\u1234\nend"#)));
        assert_eq!(raw_string(r#"start\u1234\nend""#), Ok((r#"""#, r#"start\u1234\nend"#)));
    }

    #[test]
    fn test_parse_escape() {
        assert_eq!(escape(r#"\n"#), Ok(("", "\\n")));
        assert_eq!(escape(r#"\b"#), Ok(("", "\\b")));
        assert_eq!(escape(r#"\f"#), Ok(("", "\\f")));
        assert_eq!(escape(r#"\t"#), Ok(("", "\\t")));
        assert_eq!(escape(r#"\r"#), Ok(("", "\\r")));
        assert_eq!(escape(r#"\u1234"#), Ok(("", "\\u1234")));
    }

    #[test]
    fn test_parse_unicode() {
        assert_eq!(unicode("u0faa"), Ok(("", "0faa")))
    }

    #[test]
    fn test_parse_comment() {
        assert_eq!(comment("# This is a comment"), Ok(("", " This is a comment")));
        assert_eq!(comment("// This is a comment"), Ok(("", " This is a comment")));
        assert_eq!(comment("// This is a new-line comment\n"), Ok(("", " This is a new-line comment")));

        let (_, res) = many0(comment)("// Comment 1\n# Comment 2").unwrap();
        assert_eq!(res, &[" Comment 1", " Comment 2"]);
    }

    #[test]
    fn test_parse_boolean() {
        assert_eq!(boolean("True"), Ok(("", Value::Bool(true))));
        assert_eq!(boolean("t"), Ok(("", Value::Bool(true))));
        assert_eq!(boolean("T"), Ok(("", Value::Bool(true))));
        assert_eq!(boolean("Yes"), Ok(("", Value::Bool(true))));

        assert_eq!(boolean("False"), Ok(("", Value::Bool(false))));
        assert_eq!(boolean("F"), Ok(("", Value::Bool(false))));
        assert_eq!(boolean("No"), Ok(("", Value::Bool(false))));
    }

    #[test]
    fn test_parse_null() {
        assert_eq!(null("Null"), Ok(("", Value::Null)));
    }

    #[test]
    fn test_parse_object_with_linebreaks() {
        let input = r#"
      requirements {
        "akka.dispatch.UnboundedMessageQueueSemantics" =
          akka.actor.mailbox.unbounded-queue-based
        "akka.dispatch.BoundedMessageQueueSemantics" =
          akka.actor.mailbox.bounded-queue-based
      }"#;
        let (_, result) = root(input).expect("line breaks are allowed");
        assert_eq!(
            result,
            Value::Object(vec![FieldOrInclude::Field(Field {
                path: vec!["requirements"],
                op: FieldOp::Object(vec![
                    FieldOrInclude::Field(Field {
                        path: vec!["akka.dispatch.UnboundedMessageQueueSemantics"],
                        op: FieldOp::Assign(vec![Value::String("akka.actor.mailbox.unbounded-queue-based")])
                    }),
                    FieldOrInclude::Field(Field {
                        path: vec!["akka.dispatch.BoundedMessageQueueSemantics"],
                        op: FieldOp::Assign(vec![Value::String("akka.actor.mailbox.bounded-queue-based")])
                    })
                ])
            })])
        );
    }
}
