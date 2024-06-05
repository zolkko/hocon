use crate::ast::{Array, Field, FieldOp, FieldOrInclude, Fields, Include, IncludePath, Object, Path, Span, Substitution, Value, ValueKind};
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_while};
use nom::character::complete::{anychar, char, hex_digit1, line_ending, multispace0, multispace1, space0};
use nom::combinator::{eof, map, map_res, not, opt, recognize, value};
use nom::multi::{many0, many1, many_m_n};
use nom::number::complete::recognize_float;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;

fn make_value<'a>(kind: ValueKind<'a, Span<'a>>, input: Span<'a>, reminder: Span<'a>) -> IResult<Span<'a>, Value<'a>> {
    Ok((reminder, Value::new(kind, input)))
}

fn comment(input: Span) -> IResult<Span, Span> {
    let start = alt((tag("//"), tag("#")));
    let comment = take_while(|x| x != '\n' && x != '\r');
    let end = alt((line_ending, eof));
    delimited(start, comment, end)(input)
}

fn empty_lines(input: Span) -> IResult<Span, ()> {
    let empty_line = alt((comment, multispace1));
    value((), many0(empty_line))(input)
}

/// ```peg
/// optional_subs_start = _{ "${?" }
/// ```
fn optional_substitution_start(input: Span) -> IResult<Span, Span> {
    tag("${?")(input)
}

/// ```peg
/// required_subs_start = _{ "${" }
/// ```
fn required_substitution_start(input: Span) -> IResult<Span, Span> {
    tag("${")(input)
}

/// ```peg
/// subs_end = _{ "}" }
/// ```
fn required_substitution_end(input: Span) -> IResult<Span, Span> {
    tag("}")(input)
}

/// ```peg
/// optional_substitution = { optional_subs_start ~ field_path ~ subs_end }
/// ```
fn optional_substitution(input: Span) -> IResult<Span, Substitution> {
    map(
        delimited(pair(optional_substitution_start, multispace0), field_path, pair(multispace0, required_substitution_end)),
        Substitution::Optional,
    )(input)
}

/// ```peg
/// required_substitution = { required_subs_start ~ field_path ~ subs_end }
/// ```
fn required_substitution(input: Span) -> IResult<Span, Substitution> {
    map(
        delimited(pair(required_substitution_start, multispace0), field_path, pair(multispace0, required_substitution_end)),
        Substitution::Required,
    )(input)
}

/// ```peg
/// substitution = { optional_substitution | required_substitution }
/// ```
fn substitution(input: Span) -> IResult<Span, Value> {
    let (reminder, kind) = map(alt((required_substitution, optional_substitution)), ValueKind::Substitution)(input)?;
    make_value(kind, input, reminder)
}

fn field_name_or_string(input: Span) -> IResult<Span, Span> {
    alt((field_name, string_content))(input)
}

fn field_name(input: Span) -> IResult<Span, Span> {
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

fn path_delimiter(input: Span) -> IResult<Span, char> {
    char('.')(input)
}

/// Parse separators for arrays and objects.
///
/// Initial variant of the rule used to be:
/// ```ebnf
/// separator  = _{ ("," ~ NEWLINE*) | (NEWLINE+ ~ ","? ~ NEWLINE*) }
/// ```
fn separator(input: Span) -> IResult<Span, ()> {
    let comma = value((), pair(char(','), empty_lines));
    let prefix_comma = value((), tuple((line_ending, empty_lines, opt(char(',')), empty_lines)));
    preceded(space0, alt((comma, prefix_comma)))(input)
}

/// Parse array
/// ```ebnf
/// array = { "[" ~ NEWLINE* ~ value ~ (separator ~ value)* ~ (separator)? ~ "]" | "[" ~ NEWLINE* ~ "]" }
/// ```
fn array_value(input: Span) -> IResult<Span, Value> {
    let begin = pair(char('['), empty_lines);
    let elements = terminated(opt(pair(value_chunks, terminated(many0(preceded(separator, value_chunks)), opt(separator)))), empty_lines);
    let elements_value = map(elements, |maybe_x| {
        if let Some(x) = maybe_x {
            ValueKind::Array(Array::new(combine_vec(x), input))
        } else {
            ValueKind::Array(Array::new(vec![], input))
        }
    });
    let end = pair(char(']'), space0);

    let (reminder, kind) = delimited(begin, elements_value, end)(input)?;

    make_value(kind, input, reminder)
}

/// ```peg
/// field_assign = @{ "=" | ":" }
/// ```
fn field_assign(input: Span) -> IResult<Span, Span> {
    alt((tag("="), tag(":")))(input)
}

/// ```peg
/// field_append = @{ "+=" }
/// ```
fn field_append(input: Span) -> IResult<Span, Span> {
    tag("+=")(input)
}

/// ```peg
/// path_delimiter = _{ "." }
/// field_name = @{ (!("\"" | ":" | "=" | "+=" | "{" | "}" | "[" | "]" | "," | "." | NEWLINE | WHITESPACE | "#" | "//") ~ ANY)+ }
/// field_path = { (string | field_name) ~ (path_delimiter ~ (string | field_name))* }
/// ```
fn field_path(input: Span) -> IResult<Span, Path> {
    map(pair(field_name_or_string, many0(preceded(path_delimiter, field_name_or_string))), |(h, r)| {
        let mut result: Path = Vec::with_capacity(1 + r.len());
        result.push(h.fragment());
        result.extend(r.into_iter().map(Span::into_fragment));
        result
    })(input)
}

/// ```ebnf
/// field = { field_path ~ NEWLINE* ~ (((field_assign | field_append) ~ NEWLINE* ~ value) | object) }
/// ```
fn field(input: Span) -> IResult<Span, Field<Span>> {
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
fn field_or_include(input: Span) -> IResult<Span, FieldOrInclude<Span>> {
    let include_parser = map(include, FieldOrInclude::Include);
    let field_parser = map(field, FieldOrInclude::Field);
    alt((include_parser, field_parser))(input)
}

/// ```peg
/// object_body = { field_or_include ~ (separator ~ field_or_include)* ~ separator? }
/// ```
fn object_body(input: Span) -> IResult<Span, Fields<Span>> {
    // TODO ... comments and empty lines
    let parser = terminated(pair(field_or_include, many0(preceded(separator, field_or_include))), opt(separator));
    map(parser, combine_vec)(input)
}

/// ```peg
/// object = { "{" ~ NEWLINE* ~ object_body? ~ NEWLINE* ~ "}" }
/// ```
fn object(input: Span) -> IResult<Span, Object<Span>> {
    let object_body_parser = map(opt(object_body), |mo| match mo {
        Some(o) => Object::new(o, input),
        None => Object::new(vec![], input),
    });
    delimited(pair(char('{'), empty_lines), terminated(object_body_parser, empty_lines), pair(char('}'), space0))(input)
}

fn object_value(input: Span) -> IResult<Span, Value> {
    let (reminder, kind) = map(object, ValueKind::Object)(input)?;
    make_value(kind, input, reminder)
}

/// Parse value of an array or an object.
fn value_chunks(input: Span) -> IResult<Span, Vec<Value<'_>>> {
    let mut parser = many1(terminated(value_chunk, space0));
    parser(input)
}

fn number_value(input: Span) -> IResult<Span, Value> {
    let number = map_res(recognize_float::<Span, _>, |x| {
        let x = x.into_fragment();
        if x.contains('.') {
            str::parse(x).map(ValueKind::Real).map_err(|err| format!("failed to parse double: {err}"))
        } else {
            str::parse(x).map(ValueKind::Integer).map_err(|err| format!("failed to parse int: {err}"))
        }
    });
    let nan = value(ValueKind::Real(f64::NAN), tag_no_case("nan"));
    let inf = value(ValueKind::Real(f64::INFINITY), tag_no_case("inf"));
    let neg_inf = value(ValueKind::Real(f64::NEG_INFINITY), tag_no_case("-inf"));
    let (reminder, kind) = alt((neg_inf, inf, nan, number))(input)?;
    make_value(kind, input, reminder)
}

/// ```peg
/// value_chunk = _{ number, boolean, null, string, array | object | substitution | unquoted_string }
/// ```
fn value_chunk(input: Span) -> IResult<Span, Value> {
    let mut elems = alt((number_value, null, boolean, multi_string, string, object_value, array_value, substitution, unquoted_string));
    elems(input)
}

/// ```peg
/// include_file = { ^"file(" ~ string ~ ")" }
/// ```
fn include_file(input: Span) -> IResult<Span, IncludePath> {
    let left = tuple((tag_no_case("file"), space0, char('('), multispace0));
    let right = pair(multispace0, tag(")"));
    map(delimited(left, string_content, right), |x| IncludePath::File(x.into_fragment()))(input)
}

/// ```peg
/// include_url = { ^"url(" ~ string ~ ")" }
/// ```
fn include_url(input: Span) -> IResult<Span, IncludePath> {
    let left = tuple((tag_no_case("url"), space0, tag("("), multispace0));
    let right = pair(multispace0, tag(")"));
    map(delimited(left, string_content, right), |x| IncludePath::Url(x.into_fragment()))(input)
}

/// ```peg
/// include_classpath = { ^"classpath(" ~ string ~ ")" }
/// ```
fn include_classpath(input: Span) -> IResult<Span, IncludePath> {
    map(
        delimited(
            tuple((tag_no_case("classpath"), space0, tag("("), multispace0)),
            string_content,
            pair(multispace0, tag(")")),
        ),
        |x| IncludePath::Classpath(x.into_fragment()),
    )(input)
}

/// ```peg
/// regular_include = { include_file | include_url | include_classpath | include_string }
/// ```
fn regular_include(input: Span) -> IResult<Span, Include> {
    let quoted = map(string_content, |x| IncludePath::Quoted(x.into_fragment()));
    map(alt((include_file, include_url, include_classpath, quoted)), Include::NonRequired)(input)
}

/// ```peg
/// required_include = { ^"required(" ~ regular_include ~ ")" }
/// ```
fn required_include(input: Span) -> IResult<Span, Include> {
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
fn include(input: Span) -> IResult<Span, Include> {
    preceded(pair(tag_no_case("include"), multispace1), alt((required_include, regular_include)))(input)
}

/// ```peg
/// unquoted_string_end = _{ "//" | NEWLINE | "$" | "\"" | "{" | "}" | "[" | "]" | ":" | "=" | "," | "+" | "#" | "`" | "^" | "?" | "!" | "@" | "*" | "&" | "\\" }
/// unquoted_string = @{ (!unquoted_string_end ~ ANY)+ }
/// ```
fn unquoted_string(input: Span) -> IResult<Span, Value> {
    let unquoted_string_end = alt((
        tag("//"),
        line_ending::<Span, _>,
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
    let (reminder, kind) = map(parser, |x| ValueKind::String(x.into_fragment()))(input)?;
    make_value(kind, input, reminder)
}

/// ```peg
/// multi_string = ${ "\"\"\"" ~ minner ~ "\"\"\"" }
/// ms_inner  = @{ (!("\"\"\"") ~ ANY)* }
/// ```
fn multi_string(input: Span) -> IResult<Span, Value> {
    let multi_string_content = recognize(many0(preceded(not(tag::<_, Span, _>(r#"""""#)), anychar)));
    let parser_fn = delimited(tag(r#"""""#), multi_string_content, tag(r#"""""#));
    let (reminder, kind) = map(parser_fn, |x| ValueKind::String(x.into_fragment()))(input)?;
    make_value(kind, input, reminder)
}

/// ```peg
/// string = ${ "\"" ~ raw_string ~ "\"" }
/// ```
fn string(input: Span) -> IResult<Span, Value> {
    let (reminder, kind) = map(string_content, |x| ValueKind::String(x.into_fragment()))(input)?;
    make_value(kind, input, reminder)
}

fn string_content(input: Span) -> IResult<Span, Span> {
    delimited(char('"'), raw_string, char('"'))(input)
}

/// ```peg
/// raw_string = @{ (!("\"" | "\\") ~ ANY)* ~ (escape ~ raw_string)? }
/// ```
fn raw_string(input: Span) -> IResult<Span, Span> {
    let body = take_while(|ch| ch != '"' && ch != '\\');
    let rec = many0(pair(escape, raw_string));
    recognize(pair(body, rec))(input)
}

/// ```peg
/// escape = @{ "\\" ~ ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t" | unicode) }
/// ```
fn escape(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        tag("\\"),
        alt((tag("\""), tag("\\"), tag("/"), tag("b"), tag("f"), tag("n"), tag("r"), tag("t"), unicode)),
    ))(input)
}

/// ```peg
/// unicode = @{ "u" ~ ASCII_HEX_DIGIT{4} }
/// ```
fn unicode(input: Span) -> IResult<Span, Span> {
    let ascii_hex_digit = recognize(many_m_n(1, 4, hex_digit1));
    preceded(char('u'), ascii_hex_digit)(input)
}

/// ```peg
/// true  = @{ ^"true" | ^"yes" | ^"y" | ^"t" }
/// false = @{ ^"false" | ^"no" | ^"n" | ^"f" }
/// bool  = _{ true | false }
/// ```
fn boolean(input: Span) -> IResult<Span, Value> {
    let (reminder, kind) = alt((
        value(
            ValueKind::Boolean(true),
            alt((tag_no_case("true"), tag_no_case("yes"), tag_no_case("on"), tag_no_case("t"), tag_no_case("y"), tag("1"))),
        ),
        value(
            ValueKind::Boolean(false),
            alt((tag_no_case("false"), tag_no_case("no"), tag_no_case("off"), tag_no_case("n"), tag_no_case("f"), tag("0"))),
        ),
    ))(input)?;
    make_value(kind, input, reminder)
}

/// ```peg
/// null = @{ ^"null" }
/// ```
fn null(input: Span) -> IResult<Span, Value> {
    let (reminder, kind) = value(ValueKind::Null, tag_no_case("null"))(input)?;
    make_value(kind, input, reminder)
}

fn combine_vec<T>((head, mut rest): (T, Vec<T>)) -> Vec<T> {
    rest.reserve_exact(1);
    rest.insert(0, head);
    rest
}

pub(crate) fn root(input: Span) -> IResult<Span, Value> {
    let obj_body = map(object_body, |x| Object::new(x, input));
    let object_or_body = alt((object, obj_body));
    let value = map(opt(object_or_body), |maybe_body| match maybe_body {
        Some(body) => ValueKind::Object(body),
        None => ValueKind::Object(Object::new(vec![], input)),
    });
    let parser = delimited(empty_lines, value, empty_lines);
    let (reminder, kind) = terminated(parser, eof)(input)?;
    make_value(kind, input, reminder)
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


        "#
            .into(),
        );

        assert_eq!(res.map(|x| x.1), Ok(()));
    }

    #[test]
    fn test_parse_root() {
        let input = "{ name: value }";

        let expected = Value::<Position>::new(
            ValueKind::Object(Object::new(
                vec![FieldOrInclude::Field(Field {
                    path: vec!["name"],
                    op: FieldOp::Assign(vec![Value::<Position>::new(ValueKind::String("value"), (1, 9))]),
                })],
                (1, 1),
            )),
            (1, 1),
        );

        let result = root(input.into()).map(pair_span_to_position);

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn test_parse_array() {
        assert_eq!(
            array_value("[]".into()).map(pair_span_to_position),
            Ok(Value::new(ValueKind::Array(Array::new(vec![], (1, 1))), (1, 1)))
        );
        assert_eq!(
            array_value("[1]".into()).map(pair_span_to_position),
            Ok(Value::new(
                ValueKind::Array(Array::new(vec![vec![Value::new(ValueKind::Integer(1), (1, 2))]], (1, 1))),
                (1, 1)
            ))
        );
        assert_eq!(
            array_value("[1,2]".into()).map(pair_span_to_position),
            Ok(Value::new(
                ValueKind::Array(Array::new(
                    vec![vec![Value::new(ValueKind::Integer(1), (1, 2))], vec![Value::new(ValueKind::Integer(2), (1, 4))]],
                    (1, 1)
                )),
                (1, 1)
            ))
        );
        assert_eq!(
            array_value("[1,2,]".into()).map(pair_span_to_position),
            Ok(Value::new(
                ValueKind::Array(Array::new(
                    vec![vec![Value::new(ValueKind::Integer(1), (1, 2))], vec![Value::new(ValueKind::Integer(2), (1, 4))]],
                    (1, 1)
                )),
                (1, 1)
            ))
        );
    }

    #[test]
    fn test_parse_value_chunk() {
        assert_eq!(value_chunk("true".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Boolean(true), (1, 1))));
        assert_eq!(value_chunk("123".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Integer(123), (1, 1))));
        assert_eq!(value_chunk("1.23".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Real(1.23), (1, 1))));
        assert_eq!(
            value_chunk("[]".into()).map(pair_span_to_position),
            Ok(Value::new(ValueKind::Array(Array::new(vec![], (1, 1))), (1, 1)))
        );
        assert_eq!(
            value_chunk("{}".into()).map(pair_span_to_position),
            Ok(Value::new(ValueKind::Object(Object::new(vec![], (1, 1))), (1, 1)))
        );
        assert_eq!(
            value_chunk(r#""string1""#.into()).map(pair_span_to_position),
            Ok(Value::new(ValueKind::String("string1"), (1, 1)))
        );
        assert_eq!(
            value_chunk(r#""""string2""""#.into()).map(pair_span_to_position),
            Ok(Value::new(ValueKind::String("string2"), (1, 1)))
        );
        assert_eq!(value_chunk("string".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::String("string"), (1, 1))));
    }

    #[test]
    fn test_parse_value_chunks() {
        fn convert<'a>((_, x): (Span<'a>, Vec<Value<'a, Span<'a>>>)) -> Vec<Value<'a, Position>> {
            x.into_iter().map(replace_span).collect()
        }

        assert_eq!(
            value_chunks(r#""value1" "value2" "value3""#.into()).map(convert),
            Ok(vec![
                Value::new(ValueKind::String("value1"), (1, 1)),
                Value::new(ValueKind::String("value2"), (1, 10)),
                Value::new(ValueKind::String("value3"), (1, 19))
            ])
        );
        assert_eq!(
            value_chunks(r#""value1" "value2" "value3" "#.into()).map(convert),
            Ok(vec![
                Value::new(ValueKind::String("value1"), (1, 1)),
                Value::new(ValueKind::String("value2"), (1, 10)),
                Value::new(ValueKind::String("value3"), (1, 19)),
            ])
        );
        assert_eq!(
            value_chunks(r#""value1" "value2" "value3","#.into()).map(convert),
            Ok(vec![
                Value::new(ValueKind::String("value1"), (1, 1)),
                Value::new(ValueKind::String("value2"), (1, 10)),
                Value::new(ValueKind::String("value3"), (1, 19)),
            ])
        );
    }

    #[test]
    fn test_parse_field() {
        assert_eq!(
            field("name: value".into()).map(|x| replace_field(x.1)),
            Ok(Field {
                path: vec!["name"],
                op: FieldOp::Assign(vec![Value::new(ValueKind::String("value"), (1, 7)),]),
            })
        );

        assert_eq!(
            field("name1.name2: value".into()).map(|x| replace_field(x.1)),
            Ok(Field {
                path: vec!["name1", "name2"],
                op: FieldOp::Assign(vec![Value::new(ValueKind::String("value"), (1, 14))]),
            })
        );
    }

    #[test]
    fn test_parse_object() {
        assert_eq!(object(r#"{ }"#.into()).map(|x| replace_object(x.1)), Ok(Object::new(vec![], (1, 1))));
        assert_eq!(
            object(r#"{ field1: 123 }"#.into()).map(|x| replace_object(x.1)),
            Ok(Object::new(
                vec![FieldOrInclude::Field(Field {
                    path: vec!["field1"],
                    op: FieldOp::Assign(vec![Value::new(ValueKind::Integer(123), (1, 11))]),
                })],
                (1, 1)
            ))
        );
        assert_eq!(
            object("{ field1: 123 \n field2: 321 }".into()).map(|x| replace_object(x.1)),
            Ok(Object::new(
                vec![
                    FieldOrInclude::Field(Field {
                        path: vec!["field1"],
                        op: FieldOp::Assign(vec![Value::new(ValueKind::Integer(123), (1, 11))]),
                    }),
                    FieldOrInclude::Field(Field {
                        path: vec!["field2"],
                        op: FieldOp::Assign(vec![Value::new(ValueKind::Integer(321), (2, 10))]),
                    })
                ],
                (1, 1)
            ))
        );
    }

    #[test]
    fn test_parse_separator() {
        assert!(separator(",next".into()).is_ok());
        assert!(separator(",   next".into()).is_ok());
        assert!(separator(", \nnext".into()).is_ok());
        assert!(separator("\n,\nnext".into()).is_ok());
        assert!(separator("\n \n, \nnext".into()).is_ok());
        assert!(separator("\n \n , \nnext".into()).is_ok());
    }

    #[test]
    fn test_parse_field_path() {
        assert_eq!(field_path("field".into()).map(|x| x.1), Ok(vec!["field"]));
        assert_eq!(field_path("field1.field2".into()).map(|x| x.1), Ok(vec!["field1", "field2"]));
    }

    #[test]
    fn test_parse_include() {
        let res = include(r#"include required( file(  "file"   ) )"#.into()).map(|x| x.1);
        assert_eq!(res, Ok(Include::Required(IncludePath::File("file"))));

        let res = include(r#"include required  ( file(  "file"   ) )"#.into()).map(|x| x.1);
        assert_eq!(res, Ok(Include::Required(IncludePath::File("file"))));

        let res = include(r#"include required(url("url"))"#.into()).map(|x| x.1);
        assert_eq!(res, Ok(Include::Required(IncludePath::Url("url"))));

        let res = include(r#"include classpath("classpath")"#.into()).map(|x| x.1);
        assert_eq!(res, Ok(Include::NonRequired(IncludePath::Classpath("classpath"))));
    }

    #[test]
    fn test_parse_multi_string() {
        let res = multi_string(r#""""qwerty""""#.into()).map(pair_span_to_position);
        assert_eq!(res, Ok(Value::new(ValueKind::String("qwerty"), (1, 1))));

        let res = multi_string(r#""""qwerty"123""""#.into()).map(pair_span_to_position);
        assert_eq!(res, Ok(Value::new(ValueKind::String(r#"qwerty"123"#), (1, 1))));

        let res = multi_string(r#""""qwerty""123""""#.into()).map(pair_span_to_position);
        assert_eq!(res, Ok(Value::new(ValueKind::String(r#"qwerty""123"#), (1, 1))));

        let res = multi_string(r#""""qwerty"""123""""#.into()).map(pair_span_to_position);
        assert_eq!(res, Ok(Value::new(ValueKind::String("qwerty"), (1, 1))));

        let res = multi_string(r#""""""""#.into()).map(pair_span_to_position);
        assert_eq!(res, Ok(Value::new(ValueKind::String(""), (1, 1))));

        let res = multi_string(r#""""x""""#.into()).map(pair_span_to_position);
        assert_eq!(res, Ok(Value::new(ValueKind::String("x"), (1, 1))));

        let res = multi_string(r#""""xx""""#.into()).map(pair_span_to_position);
        assert_eq!(res, Ok(Value::new(ValueKind::String("xx"), (1, 1))));
    }

    #[test]
    fn test_parse_raw_string() {
        assert_eq!(
            raw_string(r#"\u1234\nend"#.into()).map(|x| (x.0.into_fragment(), x.1.into_fragment())),
            Ok(("", r#"\u1234\nend"#))
        );
        assert_eq!(
            raw_string(r#"start\u1234\nend"#.into()).map(|x| (x.0.into_fragment(), x.1.into_fragment())),
            Ok(("", r#"start\u1234\nend"#))
        );
        assert_eq!(
            raw_string(r#"start\u1234\nend""#.into()).map(|x| (x.0.into_fragment(), x.1.into_fragment())),
            Ok((r#"""#, r#"start\u1234\nend"#))
        );
    }

    #[test]
    fn test_parse_escape() {
        fn convert<'a>(x: (Span<'a>, Span<'a>)) -> &'a str {
            x.1.into_fragment()
        }

        assert_eq!(escape(r#"\n"#.into()).map(convert), Ok("\\n"));
        assert_eq!(escape(r#"\b"#.into()).map(convert), Ok("\\b"));
        assert_eq!(escape(r#"\f"#.into()).map(convert), Ok("\\f"));
        assert_eq!(escape(r#"\t"#.into()).map(convert), Ok("\\t"));
        assert_eq!(escape(r#"\r"#.into()).map(convert), Ok("\\r"));
        assert_eq!(escape(r#"\u1234"#.into()).map(convert), Ok("\\u1234"));
    }

    #[test]
    fn test_parse_unicode() {
        assert_eq!(unicode("u0faa".into()).map(|x| x.1.into_fragment()), Ok("0faa"))
    }

    #[test]
    fn test_parse_comment() {
        fn convert<'a>(x: (Span<'a>, Span<'a>)) -> &'a str {
            x.1.into_fragment()
        }

        assert_eq!(comment("# This is a comment".into()).map(convert), Ok(" This is a comment"));
        assert_eq!(comment("// This is a comment".into()).map(convert), Ok(" This is a comment"));
        assert_eq!(comment("// This is a new-line comment\n".into()).map(convert), Ok(" This is a new-line comment"));

        let res = many0(comment)("// Comment 1\n# Comment 2".into())
            .map(|x| x.1.into_iter().map(|i| i.into_fragment()).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(res, &[" Comment 1", " Comment 2"]);
    }

    #[test]
    fn test_parse_boolean() {
        assert_eq!(boolean("True".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Boolean(true), (1, 1))));
        assert_eq!(boolean("t".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Boolean(true), (1, 1))));
        assert_eq!(boolean("T".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Boolean(true), (1, 1))));
        assert_eq!(boolean("Yes".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Boolean(true), (1, 1))));

        assert_eq!(boolean("False".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Boolean(false), (1, 1))));
        assert_eq!(boolean("F".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Boolean(false), (1, 1))));
        assert_eq!(boolean("No".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Boolean(false), (1, 1))));
    }

    #[test]
    fn test_parse_null() {
        assert_eq!(null("Null".into()).map(pair_span_to_position), Ok(Value::new(ValueKind::Null, (1, 1))));
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
        let result = root(input.into()).map(pair_span_to_position).expect("line breaks are allowed");
        assert_eq!(
            result,
            Value::new(
                ValueKind::Object(Object::new(
                    vec![FieldOrInclude::Field(Field {
                        path: vec!["requirements"],
                        op: FieldOp::Object(Object::new(
                            vec![
                                FieldOrInclude::Field(Field {
                                    path: vec!["akka.dispatch.UnboundedMessageQueueSemantics"],
                                    op: FieldOp::Assign(vec![Value::new(ValueKind::String("akka.actor.mailbox.unbounded-queue-based"), (4, 11))])
                                }),
                                FieldOrInclude::Field(Field {
                                    path: vec!["akka.dispatch.BoundedMessageQueueSemantics"],
                                    op: FieldOp::Assign(vec![Value::new(ValueKind::String("akka.actor.mailbox.bounded-queue-based"), (6, 11))])
                                })
                            ],
                            (2, 20)
                        ))
                    })],
                    (1, 1)
                )),
                (1, 1)
            )
        );
    }

    #[test]
    fn parse_null_false() {
        let input = r#"
        field-null = null
        field-no = n
        "#;
        let input = Span::new(input);
        let result = root(input).map(pair_span_to_position).expect("line breaks are allowed");
        assert_eq!(
            result,
            Value::new(
                ValueKind::Object(Object::new(
                    vec![
                        FieldOrInclude::Field(Field {
                            path: vec!["field-null"],
                            op: FieldOp::Assign(vec![Value::new(ValueKind::Null, (2, 22))]),
                        }),
                        FieldOrInclude::Field(Field {
                            path: vec!["field-no"],
                            op: FieldOp::Assign(vec![Value::new(ValueKind::Boolean(false), (3, 20))]),
                        })
                    ],
                    (1, 1)
                )),
                (1, 1)
            )
        );
    }

    type Line = u32;

    type Column = usize;

    type Position = (Line, Column);

    fn replace_field<'a>(Field { path, op }: Field<'a, Span<'a>>) -> Field<'a, Position> {
        Field {
            path,
            op: match op {
                FieldOp::Assign(i) => FieldOp::Assign(i.into_iter().map(replace_span).collect()),
                FieldOp::Append(i) => FieldOp::Append(i.into_iter().map(replace_span).collect()),
                FieldOp::Object(Object { fields, span }) => FieldOp::Object({
                    let fields = fields.into_iter().map(replace_field_or_include).collect();
                    let line = span.location_line();
                    let column = span.get_column();
                    let position = (line, column);
                    Object::new(fields, position)
                }),
            },
        }
    }

    fn replace_field_or_include<'a>(x: FieldOrInclude<'a, Span<'a>>) -> FieldOrInclude<'a, Position> {
        match x {
            FieldOrInclude::Field(f) => FieldOrInclude::Field(replace_field(f)),
            FieldOrInclude::Include(i) => FieldOrInclude::Include(i),
        }
    }

    fn replace_span<'a>(value: Value<'a, Span<'a>>) -> Value<'a, Position> {
        let line = value.span().location_line();
        let column = value.span().get_column();

        let new_kind = match value.kind() {
            ValueKind::Null => ValueKind::Null,
            ValueKind::Boolean(v) => ValueKind::Boolean(v),
            ValueKind::Integer(v) => ValueKind::Integer(v),
            ValueKind::Real(v) => ValueKind::Real(v),
            ValueKind::String(v) => ValueKind::String(v),
            ValueKind::Substitution(v) => ValueKind::Substitution(v),
            ValueKind::Array(v) => {
                let span = v.span;
                let line = span.location_line();
                let column = span.get_column();
                let position = (line, column);
                let items = v.items.into_iter().map(|x| x.into_iter().map(replace_span).collect()).collect();
                ValueKind::Array(Array::new(items, position))
            }
            ValueKind::Object(v) => ValueKind::Object(replace_object(v)),
        };

        Value::new(new_kind, (line, column))
    }

    fn replace_object<'a>(o: Object<'a, Span<'a>>) -> Object<'a, Position> {
        let Object { fields, span } = o;
        let fields = fields.into_iter().map(replace_field_or_include).collect();
        let span = (span.location_line(), span.get_column());
        Object::new(fields, span)
    }

    fn pair_span_to_position<'a, T>((_, x): (T, Value<'a, Span<'a>>)) -> Value<'a, Position> {
        replace_span(x)
    }
}
