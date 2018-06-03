use std::vec::Vec;
use std::cmp::PartialEq;
use std::mem::transmute;
use std::ops::{Range, RangeFrom, RangeTo};
use std::str;
use std::collections::HashMap;
use std::collections::BTreeMap;


use nom::{
    alpha, alphanumeric, eol, need_more, not_line_ending, recognize_float, space, space0, AsBytes, AsChar, AtEof, Compare, CompareResult, FindToken, IResult, InputIter,
    InputLength, InputTake, InputTakeAtPosition, Needed, Offset, Slice,
};

use nom::{Err as NErr, ErrorKind as NErrorKind, Context as NContext};

use super::config::{Config, Object, Array};

/// A helper function that transforms a slice to utf-8 string without coping
/// its content. To do this a `transmute` function is used which extends lifetime
/// of a variable returned by `as_bytes`.
fn convert_to_str<'a, T: 'a + AsBytes>(input: T) -> Result<&'a str, str::Utf8Error> {
    let t: &'a [u8] = unsafe { transmute(input.as_bytes()) };
    str::from_utf8(t)
}

/// Parser recognizes a simple identifier.
fn identifier<'a, T>(input: T) -> IResult<T, &'a str>
where
    T: InputIter + InputTake,
    T: AsBytes + AtEof + 'a,
    <T as InputIter>::Item: AsChar,
{
    let mut iter = input.iter_elements();

    if let Some(x) = iter.next() {
        let chr = x.as_char();
        if chr.is_alpha() {

            let mut pos = 1;

            while let Some(x) = iter.next() {
                let chr = x.as_char();
                if !chr.is_alphanum() {
                    break;
                } else {
                    pos += 1;
                }
            }

            let (next, data) = input.take_split(pos);

            match convert_to_str(data) {
                Ok(id) => Ok((next, id)),
                Err(cause) => Err(NErr::Error(NContext::Code(input, NErrorKind::Custom(1))))
            }

        } else {
            Err(NErr::Error(NContext::Code(input, NErrorKind::Custom(0))))
        }
    } else {
        need_more(input,Needed::Size(1))
    }
}

/// An identifier can be composite. Sub-identifier is separated by dot symbol.
fn field_name<'a, T>(input: T) -> IResult<T, Vec<&'a str>>
where
    T: Slice<RangeFrom<usize>>,
    T: InputIter + InputLength + InputTake,
    T: Clone + AtEof,
    T: AsBytes + 'a,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
{
    alt!(input,
        map_res!(double_quoted_string, |x| {
            match convert_to_str(x) {
                Ok(s) => {
                    let mut res: Vec<&'a str> = Vec::with_capacity(1);
                    res.push(s);
                    Ok(res)
                },
                Err(cause) => Err(cause)
            }
        }) |
        separated_list!(char!('.'), identifier)
    )
}

fn take_until_closing_triple_quotes<T>(input: T) -> IResult<T, T>
where
    T: InputIter + InputTake,
    T: AtEof,
    <T as InputIter>::Item: AsChar,
{
    let mut prev = ['\0'; 3];
    for (i, elem) in input.iter_elements().enumerate() {
        let chr = elem.as_char();
        if prev[2] != '\\' && prev[1] == '"' && prev[0] == '"' && chr == '"' {
            let (a, b) = input.take_split(i - 2);
            return Ok((a, b));
        }
        prev[2] = prev[1];
        prev[1] = prev[0];
        prev[0] = chr;
    }
    need_more(input, Needed::Unknown)
}

fn triple_quoted_string<T>(input: T) -> IResult<T, T>
where
    T: InputIter + InputLength + InputTake,
    T: Clone,
    T: AtEof,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
{
    delimited!(input, tag!("\"\"\""), take_until_closing_triple_quotes, tag!("\"\"\""))
}

/// Parser consumes an input until it reaches closing double-quote character.
fn take_until_closing_double_quotes<T>(input: T) -> IResult<T, T>
where
    T: InputIter + InputTake + AtEof,
    <T as InputIter>::Item: AsChar,
{
    let mut prev = '\0';
    for (i, elem) in input.iter_elements().enumerate() {
        let chr = elem.as_char();
        if prev != '\\' && chr == '"' {
            return Ok(input.take_split(i));
        }
        prev = chr;
    }

    need_more(input, Needed::Size(1))
}

/// Parses double quoted strings, taking into account escape characters.
/// E.g. "an example" is a valid input for this parser.
fn double_quoted_string<T>(input: T) -> IResult<T, T>
where
    T: InputIter + InputLength + InputTake,
    T: AtEof + Clone,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
{
    delimited!(input, tag!("\""), take_until_closing_double_quotes, tag!("\""))
}

/// The parser matches triple and double quoted strings
fn string_value<T>(input: T) -> IResult<T, T>
where
    T: InputIter + InputLength + InputTake,
    T: Clone + AtEof,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
{
    alt!(input, triple_quoted_string | double_quoted_string)
}

/// The parser consumes any value provided, including sub-strings. And it terminates
/// when an array, an object, a line ends, or a file ends.
///
/// All the whitespaces before and after a value are consumed by the parse. Trailing
/// whitespaces, if any, are supposed to be trimmed at later stages.
fn any_value<'a, T>(input: T) -> IResult<T, Config>
where
    T: InputIter + InputLength + InputTake,
    T: Offset + Clone + AtEof + AsBytes + 'a,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
{
    let len = input.input_len();

    let mut iter = input.iter_elements().enumerate();
    let mut offset: Option<usize> = None;

    while let Some((i, x)) = iter.next() {
        let chr = x.as_char();

        if chr == ']' || chr == '}' || chr == ',' || chr == '\r' || chr == '\n' || chr == '#' {
            let pos = i + offset.unwrap_or(0);
            let (next, data) = input.take_split(pos);
            return match convert_to_str(data) {
                Ok(s) => Ok((next, Config::Value(s.trim().to_owned()))),
                Err(_) => Err(NErr::Error(NContext::Code(input, NErrorKind::Custom(0))))
            };
        } else if chr == '/' {
            let (next, _) = input.take_split(i + offset.unwrap_or(0));
            if next.compare("//") == CompareResult::Ok {
                let pos = i + offset.unwrap_or(0);
                let (next, data) = input.take_split(pos);
                return match convert_to_str(data) {
                    Ok(s) => Ok((next, Config::Value(s.trim().to_owned()))),
                    Err(_) => Err(NErr::Error(NContext::Code(input, NErrorKind::Custom(0))))
                };
            }
        } else if chr == '"' {
            let (next, _) = input.take_split(i + offset.unwrap_or(0));
            match string_value(next) {
                Ok((l, _)) => {
                    offset = Some(input.offset(&l));
                    iter = l.iter_elements().enumerate();
                }
                Err(NErr::Incomplete(needed)) => {
                    return need_more(input, needed);
                }
                Err(cause) => {
                    return Err(cause);
                }
            }
        }
    }

    if input.at_eof() {
        let (next, data) = input.take_split(len);
        match convert_to_str(data) {
            Ok(s) => Ok((next,  Config::Value(s.trim().to_owned()))),
            Err(_) => Err(NErr::Error(NContext::Code(input, NErrorKind::Custom(0))))
        }
    } else {
        Err(NErr::Incomplete(Needed::Size(1)))
    }
}

/// The parser consume single line comment including trailing carriage return.
/// It also supports comments a comment at the end of a file.
fn parse_comment<T>(input: T) -> IResult<T, T>
where
    T: Slice<Range<usize>>,
    T: Slice<RangeFrom<usize>>,
    T: Slice<RangeTo<usize>>,
    T: InputIter,
    T: InputLength,
    T: InputTake,
    T: Copy,
    T: Offset,
    T: AtEof,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
    <T as InputIter>::RawItem: AsChar,
{
    recognize!(input, do_parse!(alt!(tag!("#") | tag!("//")) >> not_line_ending >> alt!(eol | eof!()) >> (())))
}

/// The parser consume single empty line or a comment line,
/// including trailing spaces.
fn parse_empty_line<T>(input: T) -> IResult<T, T>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition,
    T: Offset + Copy + AtEof,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
    <T as InputIter>::RawItem: AsChar,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    recognize!(input, pair!(alt!(eol | parse_comment), space0))
}

/// Array elements are be separated by a comma or EOL.
/// The rest of empty and comment lines must be ignored.
fn parse_array_separator<T>(input: T) -> IResult<T, T>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition,
    T: Offset + AtEof + Clone + Copy + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
    <T as InputIter>::RawItem: AsChar,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    recognize!(input,
        pair!(
            space0,
            alt!(
                do_parse!(
                    pair!(tag!(","), space0) >>
                    many0!(parse_empty_line) >>
                    (())
                ) |
                do_parse!(
                    many1!(parse_empty_line) >>
                    opt!(pair!(tag!(","), space0)) >>
                    many0!(parse_empty_line) >>
                    (())
                )
            )
        )
    )
}

fn parse_array_body<T>(input: T) -> IResult<T, Array>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition + AsBytes + Offset,
    T: AtEof,
    T: Clone + Copy + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    let parsed_array = do_parse!(input,
        pair!(space0, many0!(parse_empty_line)) >>
        r: opt!(
            do_parse!(
                v: separated_list!(parse_array_separator, parse_value) >>
                space0 >>
                opt!(parse_array_separator) >>
                space0 >>
                ((v))
            )
        ) >>
        ((r))
    );

    match parsed_array {
        Ok((next, r)) => {
            match r {
                Some(a) => Ok((next, a)),
                None => Ok((next, Array::new()))
            }
        },
        Err(cause) => Err(cause)
    }
}

/// Parsers array of values.
/// An array can be empty, it can contain single element.
fn parse_array<T>(input: T) -> IResult<T, Array>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition + AsBytes + Offset,
    T: AtEof,
    T: Clone + Copy + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited!(input, char!('['), parse_array_body, char!(']'))
}

/// HOCON format allows a user specify multiple arrays using two or more consecutive
/// arrays definitions, but only if both arrays are defined on the same line.
///
/// Here I slightly weaken requirements and allow a user to define multiple arrays if the second
/// array begins on the same line on which the first array ended.
fn parse_arrays<T>(input: T) -> IResult<T, Config>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition + AsBytes + Offset,
    T: AtEof,
    T: Clone + Copy + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    let parsed_arrays = fold_many1!(input,
            do_parse!(
                a: parse_array >>
                space0 >>
                ((a))
            ),
            Array::new(),
            |acc: Array, v: Array| {
                let mut r = Array::new();
                for i in acc.iter() {
                    r.push(i.clone());
                }
                for i in v.iter() {
                    r.push(i.clone());
                }
                r
            }
    );

    match parsed_arrays {
        Ok((next, a)) => Ok((next, Config::Array(a))),
        Err(cause) => Err(cause)
    }
}

enum FieldOrInclude<'a> {
    Field((Vec<&'a str>, Config)),
    Include(Object)
}

fn parse_include<'a, T>(input: T) -> IResult<T, FieldOrInclude<'a>>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition,
    T: Offset + Copy + AtEof + AsBytes + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    let parsed_include = do_parse!(input,
        tag!("include") >>
        space >>
        alt!(
            value!((), double_quoted_string) |
            do_parse!(tag!("url(") >> space0 >> double_quoted_string >> space0 >> tag!(")") >> (())) |
            do_parse!(tag!("file(") >> space0 >> double_quoted_string >> space0 >> tag!(")") >> (())) |
            do_parse!(tag!("classpath(") >> space0 >> double_quoted_string >> space0 >> tag!(")") >> (()))
        ) >>
        (())
    );

    match parsed_include {
        Ok((next, _)) => Ok((next, FieldOrInclude::Include(Object::new()))),
        Err(cause) => Err(cause)
    }
}

fn parse_field<'a, T>(input: T) -> IResult<T, FieldOrInclude<'a>>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition,
    T: Offset + Copy + AtEof + AsBytes + PartialEq + 'a,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    let parsed_field = do_parse!(input,
        f: field_name >>
        space0 >>
        alt!(recognize!(
            do_parse!(
                alt!(tag!(":") | tag!("=")) >>
                space0 >>
                parse_value >>
                (()))
            ) |
            recognize!(parse_object)
        ) >>
        ((f))
    );

    match parsed_field {
        Ok((next, f)) => Ok((next, FieldOrInclude::Field((f, Config::Array(Array::new()))))),
        Err(cause) => Err(cause)
    }
}

fn parse_field_or_include<'a, T>(input: T) -> IResult<T, FieldOrInclude<'a>>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition,
    T: Offset + Copy + AtEof + AsBytes + PartialEq + 'a,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    alt!(input, parse_include | parse_field)
}

fn merge_arrays(first: &Array, second: &Array) -> Array {
    let mut merged_array = Array::with_capacity(first.len() + second.len());
    merged_array.extend(first.iter().map(|x| x.clone()));
    merged_array.extend(second.iter().map(|x| x.clone()));
    merged_array
}

fn combine_objects(first: &Object, second: &Object) -> Object {
    let mut merged = Object::new();

    merged.extend(first.iter().map(|(k,v)| (k.clone(), v.clone())));

    for (k, v) in second.iter() {
        merged.insert(k.clone(), v.clone());
    }
    merged
}

fn insert_values<'a>(object: &mut Object, path: &[&'a str], value: &Config) {

    match path {
        [] => (),
        [k] => {
            let key = k.to_string();
            match object.remove(&key) {
                Some(Config::Array(existing_array)) => {
                    match value {
                        Config::Array(new_array) => {
                            object.insert(key, Config::Array(merge_arrays(&existing_array, &new_array)));
                        },
                        _ => {
                            object.insert(key, value.clone());
                        }
                    }
                },
                Some(Config::Object(existing_obj)) => {
                    match value {
                        Config::Object(new_obj) => {
                            object.insert(key, Config::Object(combine_objects(&existing_obj, &new_obj)));
                        },
                        _ => {
                            object.insert(key, value.clone());
                        }
                    }
                },
                _ => {
                    object.insert(key.to_string(), value.clone());
                }
            }
        },
        [k, rest..] => {
            let mut sub_object = Object::new();
            insert_values(&mut sub_object, rest, value);

            let key = k.to_string();
            match object.remove(&key) {
                Some(Config::Object(existing_object)) => {
                    object.insert(key, Config::Object(combine_objects(&existing_object, &sub_object)));
                },
                _ => {
                    object.insert(key, Config::Object(sub_object));
                }
            }
        }
    }
}


fn merge_objects(dest: &mut Object, src: &Object) {
    for (k, v) in src.iter() {
        dest.insert(k.clone(), v.clone());
    }
}

fn parse_object_body<T>(input: T) -> IResult<T, Config>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition + AsBytes + Offset,
    T: AtEof,
    T: Clone + Copy + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    let parsed_object = do_parse!(input,
        pair!(space0, many0!(parse_empty_line)) >>
        r: opt!(do_parse!(
            v: separated_list!(parse_array_separator, parse_field_or_include) >>
            space0 >>
            opt!(parse_array_separator) >>
            space0 >>
            ((v))
        )) >>
        ((r))
    );

    match parsed_object {
        Ok((next, maybe_fields)) => {
            match maybe_fields {
                Some(field_or_includes) => {

                    let mut obj = Object::new();
                    for i in field_or_includes.iter() {
                        match i {
                            FieldOrInclude::Field((k, v)) => {
                                insert_values(&mut obj, &k, &v);
                            },
                            FieldOrInclude::Include(sub_obj) => {
                                merge_objects(&mut obj, &sub_obj);
                            }
                        }
                    }

                    Ok((next, Config::Object(obj)))
                },
                None => Ok((next, Config::Object(Object::new())))
            }
        },
        Err(cause) => Err(cause)
    }
}

fn parse_object<T>(input: T) -> IResult<T, Config>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition + AsBytes + Offset,
    T: AtEof,
    T: Clone + Copy + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    delimited!(input, tag!("{"), parse_object_body, tag!("}"))
}

fn parse_value<T>(input: T) -> IResult<T, Config>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition + AsBytes + Offset,
    T: AtEof,
    T: Clone + Copy + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    alt!(input, parse_arrays | parse_object | any_value)
}

fn parse_root<T>(input: T) -> IResult<T, Config>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition,
    T: Offset + Copy + AtEof + AsBytes + PartialEq,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{

    let parsed_root = do_parse!(input,
        pair!(space0, many0!(parse_empty_line)) >>
        alt!(recognize!(parse_object) | recognize!(parse_object_body)) >>
        pair!(space0, many0!(parse_empty_line)) >>
        (())
    );

    match parsed_root {
        Ok((next, _)) => Ok((next, Config::Object(Object::new()))),
        Err(cause) => Err(cause)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use nom;
    use nom::types::CompleteStr;

    /*
    #[test]
    fn test_parse_root() {
        assert!(parse_root(CompleteStr("\nroot.key=123\n")).is_ok());
        assert!(parse_root(CompleteStr("\n{root.key=123}\n")).is_ok());
    }
    */

    /*
    #[test]
    fn test_parse_object() {
        assert_eq!(parse_object(CompleteStr("{\t}")), Ok((CompleteStr(""), CompleteStr("\t"))));
        assert_eq!(parse_object(CompleteStr("{value=value}")), Ok((CompleteStr(""), CompleteStr("value=value"))));
        assert_eq!(
            parse_object(CompleteStr("{key1=value1,key2=value2\nkey3=value3}")),
            Ok((CompleteStr(""), CompleteStr("key1=value1,key2=value2\nkey3=value3")))
        );
        assert_eq!(
            parse_object(CompleteStr(
                r#"{
                key1.subkey = value1
                # comment line

                key2 : value2
                key3=value3
                include url("http://example.com")
                include "http://example.com" #comment line
                key4 {
                    subkey2 = [1, 2
                    # sub comment
                    4
                    {subsub.key=123}
                    ]
                }
            }"#
            )),
            Ok((
                CompleteStr(""),
                CompleteStr(
                    r#"
                key1.subkey = value1
                # comment line

                key2 : value2
                key3=value3
                include url("http://example.com")
                include "http://example.com" #comment line
                key4 {
                    subkey2 = [1, 2
                    # sub comment
                    4
                    {subsub.key=123}
                    ]
                }
            "#
                )
            ))
        );
    }

    #[test]
    fn test_parse_array_separator() {
        // first alternative
        assert_eq!(parse_array_separator(CompleteStr(",")), Ok((CompleteStr(""), CompleteStr(","))));
        assert_eq!(parse_array_separator(CompleteStr("  ,  ")), Ok((CompleteStr(""), CompleteStr("  ,  "))));
        assert_eq!(parse_array_separator(CompleteStr(",\n  ")), Ok((CompleteStr(""), CompleteStr(",\n  "))));
        assert_eq!(parse_array_separator(CompleteStr(",#comment\n  ")), Ok((CompleteStr(""), CompleteStr(",#comment\n  "))));
        assert_eq!(parse_array_separator(CompleteStr(",#comment\n")), Ok((CompleteStr(""), CompleteStr(",#comment\n"))));
        assert_eq!(parse_array_separator(CompleteStr(",#comment")), Ok((CompleteStr(""), CompleteStr(",#comment"))));
        assert_eq!(
            parse_array_separator(CompleteStr(",#comment1\n\n#comment2\n")),
            Ok((CompleteStr(""), CompleteStr(",#comment1\n\n#comment2\n")))
        );
        assert_eq!(
            parse_array_separator(CompleteStr(",#comment1\n\n#comment2\n,")),
            Ok((CompleteStr(","), CompleteStr(",#comment1\n\n#comment2\n")))
        );

        // second alternative
        assert_eq!(parse_array_separator(CompleteStr("#comment\n")), Ok((CompleteStr(""), CompleteStr("#comment\n"))));
        assert_eq!(parse_array_separator(CompleteStr("\n")), Ok((CompleteStr(""), CompleteStr("\n"))));
        assert_eq!(parse_array_separator(CompleteStr("\n\n \n")), Ok((CompleteStr(""), CompleteStr("\n\n \n"))));
        assert_eq!(parse_array_separator(CompleteStr("\n\n \n ,")), Ok((CompleteStr(""), CompleteStr("\n\n \n ,"))));
        assert_eq!(
            parse_array_separator(CompleteStr("\n\n,#comment1\n\n#comment2\n")),
            Ok((CompleteStr(""), CompleteStr("\n\n,#comment1\n\n#comment2\n")))
        );
        assert_eq!(
            parse_array_separator(CompleteStr("\n\n,#comment1\n,\n#comment2\n")),
            Ok((CompleteStr(",\n#comment2\n"), CompleteStr("\n\n,#comment1\n")))
        );
    }
    */

    #[test]
    fn test_parse_comment() {
        assert_eq!(
            parse_comment(CompleteStr("#comment\n")),
            Ok((CompleteStr(""), CompleteStr("#comment\n"))),
            "it should parse comment line"
        );
        assert_eq!(
            parse_comment(CompleteStr("#comment")),
            Ok((CompleteStr(""), CompleteStr("#comment"))),
            "it should parse comment if it is the last line in a file"
        );
        assert_eq!(
            parse_comment(CompleteStr("//comment\n")),
            Ok((CompleteStr(""), CompleteStr("//comment\n"))),
            "it should parse comment which starts with // sequence"
        );
        assert_eq!(
            parse_comment(CompleteStr("#comment\nabc")),
            Ok((CompleteStr("abc"), CompleteStr("#comment\n"))),
            "it should not consume non comment"
        );
        assert!(
            parse_comment(CompleteStr("")).is_err(),
            "a comment must start with hash symbol or double-slash sequence"
        );
    }

    /*
    #[test]
    fn test_parse_array() {
        assert_eq!(parse_array(CompleteStr("[]")), Ok((CompleteStr(""), CompleteStr("[]"))));
        assert_eq!(parse_array(CompleteStr("[ ]")), Ok((CompleteStr(""), CompleteStr("[ ]"))));
        assert_eq!(parse_array(CompleteStr("[ 1 ]")), Ok((CompleteStr(""), CompleteStr("[ 1 ]"))));
        assert_eq!(parse_array(CompleteStr("[#comment\n\n1\n]")), Ok((CompleteStr(""), CompleteStr("[#comment\n\n1\n]"))));
        assert_eq!(parse_array(CompleteStr("[1,\"1222\",1]")), Ok((CompleteStr(""), CompleteStr("[1,\"1222\",1]"))));
        assert_eq!(parse_array(CompleteStr("[ 1 , 1 , 1 ]")), Ok((CompleteStr(""), CompleteStr("[ 1 , 1 , 1 ]"))));
        assert_eq!(
            parse_array(CompleteStr("[ 1 , \"1\" , 1 asd , ]")),
            Ok((CompleteStr(""), CompleteStr("[ 1 , \"1\" , 1 asd , ]")))
        );
        assert_eq!(parse_array(CompleteStr("[ 1 \n 1 \r\n 1 ]")), Ok((CompleteStr(""), CompleteStr("[ 1 \n 1 \r\n 1 ]"))));
    }

    #[test]
    fn test_parse_arrays() {
        assert_eq!(parse_arrays(CompleteStr("[1,2,3]")), Ok((CompleteStr(""), CompleteStr("[1,2,3]"))));
        assert_eq!(parse_arrays(CompleteStr("[1,2,3][3,2,1]")), Ok((CompleteStr(""), CompleteStr("[1,2,3][3,2,1]"))));
        assert_eq!(parse_arrays(CompleteStr("[1,2,3] [3,2,1]")), Ok((CompleteStr(""), CompleteStr("[1,2,3] [3,2,1]"))));
        assert_eq!(
            parse_arrays(CompleteStr("[[1,2,3], [4,5,6],] [3,2,1]")),
            Ok((CompleteStr(""), CompleteStr("[[1,2,3], [4,5,6],] [3,2,1]")))
        );
    }
    */

    #[test]
    fn test_any_value() {
        assert_eq!(
            any_value(CompleteStr("some value")),
            Ok((CompleteStr(""), Config::Value("some value".to_owned()))),
            "it should parse a string value"
        );
        assert_eq!(
            any_value(CompleteStr(" some value ")),
            Ok((CompleteStr(""), Config::Value("some value".to_owned()))),
            "it should parse a string value prefixed with whitespaces"
        );
        assert_eq!(
            any_value(CompleteStr(" some value # comment")),
            Ok((CompleteStr("# comment"), Config::Value("some value".to_owned()))),
            "it should not include trailing comments into parsed value"
        );
        assert_eq!(
            any_value(CompleteStr("some value//")),
            Ok((CompleteStr("//"), Config::Value("some value".to_owned()))),
            "it should not include trailing comments even if they begins right after the value"
        );
        assert_eq!(
            any_value(CompleteStr("some value / not comment")),
            Ok((CompleteStr(""), Config::Value("some value / not comment".to_owned()))),
            "it should allow to include a slash-character into the value"
        );
        assert_eq!(
            any_value(CompleteStr("some value/")),
            Ok((CompleteStr(""), Config::Value("some value/".to_owned()))),
            "it should allow a slash-character to be included into the value even if it is the latest character in the string"
        );
        assert_eq!(
            any_value(CompleteStr("\"quoted string\"")),
            Ok((CompleteStr(""), Config::Value("\"quoted string\"".to_owned()))),
            "it should parse quoted strings"
        );
        assert_eq!(
            any_value(CompleteStr("  \"quoted string\"  ")),
            Ok((CompleteStr(""), Config::Value("\"quoted string\"".to_owned()))),
            "it should parse quoted strings prefixed with whitespaces"
        );

        /*
        assert_eq!(any_value(CompleteStr("\"quoted string\"]")), Ok((CompleteStr("]"), CompleteStr("\"quoted string\""))));
        assert_eq!(any_value(CompleteStr("\"quoted string\"  ]")), Ok((CompleteStr("]"), CompleteStr("\"quoted string\"  "))));
        assert_eq!(
            any_value(CompleteStr("a value with \"quoted\" string")),
            Ok((CompleteStr(""), CompleteStr("a value with \"quoted\" string")))
        );
        assert_eq!(
            any_value(CompleteStr("a value with \"quoted\" string # comment line")),
            Ok((CompleteStr("# comment line"), CompleteStr("a value with \"quoted\" string ")))
        );
        assert_eq!(
            any_value(CompleteStr("a value with \"# quoted comment\" string # comment line")),
            Ok((CompleteStr("# comment line"), CompleteStr("a value with \"# quoted comment\" string ")))
        );
        assert_eq!(
            any_value(CompleteStr("a value with \"\"\" // quoted comment # test \"\"\" string # comment line")),
            Ok((CompleteStr("# comment line"), CompleteStr("a value with \"\"\" // quoted comment # test \"\"\" string ")))
        );
        assert_eq!(any_value(CompleteStr("    ")), Ok((CompleteStr(""), CompleteStr("    "))));
        assert_eq!(any_value(CompleteStr("]")), Ok((CompleteStr("]"), CompleteStr(""))));
        assert_eq!(any_value(CompleteStr(" \t ]")), Ok((CompleteStr("]"), CompleteStr(" \t "))));
        assert_eq!(
            any_value(CompleteStr("a \"value\" with \"quoted\" string")),
            Ok((CompleteStr(""), CompleteStr("a \"value\" with \"quoted\" string")))
        );
        assert_eq!(any_value(CompleteStr("\"value\"\"quoted\"")), Ok((CompleteStr(""), CompleteStr("\"value\"\"quoted\""))));
        */
    }

    /*
    #[test]
    fn parse_quoted_string() {
        assert_eq!(
            take_until_closing_double_quotes("\\n\r \\\"hello\\\" all\" "),
            Ok(("\" ", "\\n\r \\\"hello\\\" all")),
            "must consume entire row"
        );

        assert_eq!(double_quoted_string("\"hello world!\""), Ok(("", "hello world!")), "must recognize quoted string");

        assert_eq!(double_quoted_string("\"\""), Ok(("", "")), "must recognize empty quoted string");

        assert_eq!(
            double_quoted_string("\"Hello, \\\"username\\\"!\\\n\nHow are your doing?\""),
            Ok(("", "Hello, \\\"username\\\"!\\\n\nHow are your doing?")),
            "must handle escaped characters"
        );
    }

    #[test]
    fn parse_triple_quoted_string() {
        assert_eq!(
            take_until_closing_triple_quotes("ends with triple quotes\"\"\""),
            Ok(("\"\"\"", "ends with triple quotes")),
            "should stop when it reaches triple quotes"
        );

        assert_eq!(
            take_until_closing_triple_quotes("ends with triple quotes\"\""),
            Err(nom::Err::Incomplete(Needed::Unknown)),
            "should report that input is incomplete (missing one?)"
        );

        assert_eq!(
            take_until_closing_triple_quotes("ends with triple quotes\""),
            Err(nom::Err::Incomplete(Needed::Unknown)),
            "should report that input is incomplete (missing two?)"
        );

        assert_eq!(
            triple_quoted_string("\"\"\"hello world\"\"\""),
            Ok(("", "hello world")),
            "should recognize triple quoted strings"
        );

        assert_eq!(
            triple_quoted_string("\"\"\"hello world\"\""),
            Err(nom::Err::Incomplete(Needed::Unknown)),
            "should not recognize incomplete triple quoted strings (one quote is missing)"
        );

        assert_eq!(
            triple_quoted_string("\"\"\"hello world\""),
            Err(nom::Err::Incomplete(Needed::Unknown)),
            "should not recognize incomplete triple quoted strings (two quotes are missing)"
        );

        assert_eq!(
            triple_quoted_string("\"\"\"\"hello\" world\"\"\""),
            Ok(("", "\"hello\" world")),
            "should ignore internal quotation"
        );

        assert_eq!(
            triple_quoted_string("\"\"\"\"hello\" \\\"world\\\"\"\"\""),
            Ok(("", "\"hello\" \\\"world\\\"")),
            "should ignore escaped quote"
        );
    }
    */

    #[test]
    fn test_identifier() {
        assert_eq!(
            identifier(CompleteStr("key")), Ok((CompleteStr(""), "key")),
            "an identifier can be made of letters"
        );
        assert_eq!(
            identifier(CompleteStr("key123")), Ok((CompleteStr(""), "key123")),
            "an identifier can begin with a letter and can contain digits"
        );
        assert_eq!(
            identifier(CompleteStr("key123.")), Ok((CompleteStr("."), "key123")),
            "an identifier ends if the next character is not alphanumeric"
        );
        assert_eq!(
            identifier(CompleteStr("k")), Ok((CompleteStr(""), "k")),
            "single letter is a valid identifier"
        );
        assert!(identifier(CompleteStr("")).is_err(), "an identifier cannot be empty");
        assert!(identifier(CompleteStr("1key")).is_err(), "an identifier cannot begin from a number");
    }

    #[test]
    fn test_field_name() {
        assert_eq!(
            field_name(CompleteStr("key")), Ok((CompleteStr(""), vec!["key"])),
            "field name can be a single identifier"
        );
        assert_eq!(
            field_name(CompleteStr("key1.key2")), Ok((CompleteStr(""), vec!["key1", "key2"])),
            "field name can be a sequence of identifiers"
        );
        assert_eq!(
            field_name(CompleteStr("k.k=")), Ok((CompleteStr("="), vec!["k", "k"])),
            "field name can be a sequence of short identifiers"
        );
        assert_eq!(field_name(CompleteStr("\"any string\"")),
                   Ok((CompleteStr(""), vec!["any string"])),
                   "field name can be any enquoted string");
    }
}
