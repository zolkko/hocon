use std::cmp::PartialEq;
use std::mem::transmute;
use std::ops::{Range, RangeFrom, RangeTo};
use std::str;

use nom::{
    alpha, alphanumeric, eol, need_more, not_line_ending, recognize_float, space, space0, AsBytes, AsChar, AtEof, Compare, CompareResult, IResult, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Needed, Offset, Slice,
};

named!(number<f64>, flat_map!(call!(recognize_float), parse_to!(f64)));

named!(
    boolean<bool>,
    alt!(
    tag_no_case!("true")  => { |_| true } |
    tag_no_case!("false") => { |_| false } |
    tag_no_case!("yes")  => { |_| true } |
    tag_no_case!("no") => { |_| false }
  )
);

///
/// Consume a comment until the end of the line.
///
fn line_comment<T>(input: T) -> IResult<T, T>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake,
    T: Compare<&'static str> + AtEof + Offset,
    T: Clone + PartialEq,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    recognize!(input, do_parse!(alt!(tag!("#") | tag!("//")) >> call!(not_line_ending) >> (())))
}

/// A helper function that transforms a slice to utf-8 string without coping
/// its content. To do this a `transmute` function is used which extends lifetime
/// of a variable returned by `as_bytes`.
fn convert_to_str<'a, T: 'a + AsBytes>(input: T) -> Result<&'a str, str::Utf8Error> {
    let t: &'a [u8] = unsafe { transmute(input.as_bytes()) };
    str::from_utf8(t)
}

/// Parser for identifiers in the configuration file
fn simple_identifier<'a, T>(input: T) -> IResult<T, &'a str>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputTake + InputTakeAtPosition + InputLength,
    T: Offset + AtEof + AsBytes + 'a,
    T: Clone,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    map_res!(input, recognize!(pair!(value!((), alpha), value!((), opt!(alphanumeric)))), convert_to_str)
}

/// An identifier can be composite. Sub-identifier is separated by dot symbol.
fn identifier<'a, T>(input: T) -> IResult<T, ::std::vec::Vec<&'a str>>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + InputTakeAtPosition,
    T: Offset + AtEof + AsBytes + 'a,
    T: Clone,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    separated_list!(input, char!('.'), simple_identifier)
}

fn take_until_closing_triple_quotes<T>(input: T) -> IResult<T, T>
where
    T: InputIter + InputLength + InputTake,
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
    T: AtEof + Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
    T: Clone,
{
    delimited!(input, tag!("\"\"\""), take_until_closing_triple_quotes, tag!("\"\"\""))
}

fn take_until_closing_double_quotes<T>(input: T) -> IResult<T, T>
where
    T: InputIter + InputLength + InputTake,
    T: AtEof,
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
    T: AtEof,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
    T: Clone,
{
    alt_complete!(input, triple_quoted_string | double_quoted_string)
}

/// The parser consumes any value provided, including sub-strings. And it terminates
/// when an array, an object, a line ends, or a file ends.
///
/// All the whitespaces before and after a value are consumed by the parse. Trailing
/// whitespaces, if any, are supposed to be trimmed at later stages.
fn any_value<T>(input: T) -> IResult<T, T>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake + Offset,
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
    T: Clone + Copy,
    T: AtEof,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    let len = input.input_len();

    let mut iter = input.iter_elements().enumerate();
    let mut offset: Option<usize> = None;

    while let Some((i, x)) = iter.next() {
        let chr = x.as_char();

        if chr == ']' || chr == '}' || chr == ',' || chr == '\r' || chr == '\n' || chr == '#' {
            let pos = i + offset.unwrap_or(0);
            return Ok(input.take_split(pos));
        } else if chr == '/' {
            let (next, _) = input.take_split(i + offset.unwrap_or(0));
            if next.compare("//") == CompareResult::Ok {
                let pos = i + offset.unwrap_or(0);
                return Ok(input.take_split(pos));
            }
        } else if chr == '"' {
            use nom;
            let (next, _) = input.take_split(i + offset.unwrap_or(0));
            match string_value(next) {
                Ok((l, _)) => {
                    offset = Some(input.offset(&l));
                    iter = l.iter_elements().enumerate();
                }
                Err(nom::Err::Incomplete(needed)) => {
                    return need_more(input, needed);
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }
    }

    if input.at_eof() {
        Ok(input.take_split(len))
    } else {
        need_more(input, Needed::Size(1))
    }
}

/// Recognizes either end of line or end of the file "character".
fn eol_or_eof<T>(input: T) -> IResult<T, T>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake,
    T: AtEof,
    T: Compare<&'static str>,
    T: Clone + Copy,
{
    alt!(input, eol | eof!())
}

/// Recognizes a sequence of empty lines. These can be whitespaces, comments,
/// empty new lines and any combinations of those.
fn empty_lines<T>(input: T) -> IResult<T, T>
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
    recognize!(input,
        many0!(
            alt_complete!(
                value!((), space) |
                value!((), tuple!(opt!(line_comment), eol_or_eof))
            )
        )
    )
}

/// This is supposed to be a value separator inside arrays and objects.
/// Values of an array may be separated by a comma and an end of line.
/// There are could be some empty lines, which need to be ignored.
/// Also a user may decide to put a few comment lines into an array or an object definition.
fn value_separator<T>(input: T) -> IResult<T, T>
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
    recognize!(input,
        do_parse!(
            space0 >>
            alt!(tag!(",") | eol | line_comment) >>
            space0 >>
            (())
        )
    )
}

/// Parsers array of values
fn parse_array<T>(input: T) -> IResult<T, ()>
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
    do_parse!(input,
        tag!("[") >>
        space0 >>
        separated_list!(
            value_separator,
            parse_value
        ) >>
        space0 >>
        opt!(value_separator) >>
        space0 >>
        tag!("]") >>
        (())
    )
}

/// For some unknown for me reason HOCON format allows a user specify an array
/// using two or more consecutive arrays, but only if the second array
/// ends at the same line where first array begins.
fn parse_arrays<T>(input: T) -> IResult<T, ()>
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
    value!(input, (),
        many1!(
            do_parse!(
                parse_array >>
                space0 >>
                (())
            )
        )
    )
}


fn parse_value<T>(input: T) -> IResult<T, ()>
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
    alt!(input,
        parse_arrays |
        value!((), any_value)
    )
}

#[cfg(test)]
mod tests {

    use super::*;
    use nom;
    use nom::types::CompleteStr;

    #[test]
    fn test_parse_array() {
        assert_eq!(parse_array(CompleteStr("[1,\"1222\",1]")), Ok((CompleteStr(""), ())));
        assert_eq!(parse_array(CompleteStr("[ 1 , 1 , 1 ]")), Ok((CompleteStr(""), ())));
        assert_eq!(parse_array(CompleteStr("[ 1 , \"1\" , 1 asd , ]")), Ok((CompleteStr(""), ())));
        assert_eq!(parse_array(CompleteStr("[ 1 \n 1 \r\n 1 ]")), Ok((CompleteStr(""), ())));
    }

    #[test]
    fn test_parse_arrays() {
        assert_eq!(parse_arrays(CompleteStr("[1,2,3]")), Ok((CompleteStr(""), ())));
        assert_eq!(parse_arrays(CompleteStr("[1,2,3][3,2,1]")), Ok((CompleteStr(""), ())));
        assert_eq!(parse_arrays(CompleteStr("[1,2,3] [3,2,1]")), Ok((CompleteStr(""), ())));
        assert_eq!(parse_arrays(CompleteStr("[[1,2,3], [4,5,6],] [3,2,1]")), Ok((CompleteStr(""), ())));
    }

    #[test]
    fn test_generic_value() {
        assert_eq!(
            any_value(CompleteStr("some value")),
            Ok((CompleteStr(""), CompleteStr("some value"))),
            "supports basic values"
        );
        assert_eq!(
            any_value(CompleteStr(" some value")),
            Ok((CompleteStr(""), CompleteStr(" some value"))));
        assert_eq!(
            any_value(CompleteStr("  some value  ")),
            Ok((CompleteStr(""), CompleteStr("  some value  "))));
        assert_eq!(
            any_value(CompleteStr(" some value # comment")),
            Ok((CompleteStr("# comment"), CompleteStr(" some value "))));
        assert_eq!(
            any_value(CompleteStr("some value // comment")),
            Ok((CompleteStr("// comment"), CompleteStr("some value "))),
            "value ends when a // comment begins"
        );
        assert_eq!(
            any_value(CompleteStr("some value//")),
            Ok((CompleteStr("//"), CompleteStr("some value"))),
            "there could be no spaces between a value and a comment line"
        );
        assert_eq!(
            any_value(CompleteStr("some value / not comment")),
            Ok((CompleteStr(""), CompleteStr("some value / not comment"))),
            "slash-character is a valid symbol of a value"
        );

        assert_eq!(
            any_value(CompleteStr("some value/")),
            Ok((CompleteStr(""), CompleteStr("some value/"))),
            "slash-character is a valid symbol even if it is a last character in the input"
        );
        assert_eq!(
            any_value(CompleteStr("  \"quoted string\"  ")),
            Ok((CompleteStr(""), CompleteStr("  \"quoted string\"  "))),
            "supports quoted strings"
        );
        assert_eq!(any_value(CompleteStr("\"quoted string\"]")), Ok((CompleteStr("]"), CompleteStr("\"quoted string\""))));
        assert_eq!(any_value(CompleteStr("\"quoted string\"  ]")), Ok((CompleteStr("]"), CompleteStr("\"quoted string\"  "))));
        assert_eq!(any_value(CompleteStr("\"quoted string\"")), Ok((CompleteStr(""), CompleteStr("\"quoted string\""))));

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
    }

    #[test]
    fn parse_empty_lines() {
        assert_eq!(
            empty_lines("  # some comment\n\n\n  \n   \n# another comment\n  ,"),
            Ok((",", "  # some comment\n\n\n  \n   \n# another comment\n  "))
        );
    }

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

    #[test]
    fn parse_line_comments() {
        assert_eq!(
            line_comment(b"# this is a line comment\n".as_ref()),
            Ok((&b"\n"[..], &b"# this is a line comment"[..])),
            "a line comment can start with a hash sign"
        );
        assert_eq!(
            line_comment(b"// this is another line comment\n".as_ref()),
            Ok((&b"\n"[..], &b"// this is another line comment"[..])),
            "a line comment can start with a double slash sign"
        );
    }

    #[test]
    fn parse_simple_identifier() {
        assert_eq!(
            simple_identifier(CompleteStr(&"key")),
            Ok((CompleteStr(&""), "key")),
            "simple_identifier can parse an identifier made of letters"
        );
        assert_eq!(
            simple_identifier(CompleteStr(&"key123")),
            Ok((CompleteStr(&""), "key123")),
            "simple_identifier can parse an identifier made of letters followed by numbers"
        );
    }

    #[test]
    fn parse_identifier() {
        assert_eq!(
            identifier(CompleteStr(&"key")),
            Ok((CompleteStr(&""), vec!["key"])),
            "identifier function can parse a simple identifier"
        );
        assert_eq!(
            identifier(CompleteStr(&"key1.key2")),
            Ok((CompleteStr(&""), vec!["key1", "key2"])),
            "identifier function can parse a composite identifier"
        );
    }
}
