use std::cmp::PartialEq;
use std::mem::transmute;
use std::ops::{Range, RangeFrom, RangeTo};
use std::str;

use nom::{
    alpha, alphanumeric, eol, need_more, not_line_ending, recognize_float, space, space0, AsBytes, AsChar, AtEof, Compare, CompareResult, IResult, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Needed, Offset, Slice,
};


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
    T: InputIter + InputLength + InputTake + InputTakeAtPosition,
    T: Offset + Clone + AtEof,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
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

/// The parser consume single line comment including trailing carriage return.
/// It also supports comments a comment at the end of a file.
fn parse_comment<T>(input: T) -> IResult<T, T>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength + InputTake,
    T: Copy + Offset + AtEof,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar + Clone,
    <T as InputIter>::RawItem: AsChar + Clone,
{
    recognize!(input,
        do_parse!(
            alt!(tag!("#") | tag!("//")) >>
            not_line_ending >>
            alt!(eol | eof!()) >>
            (())
        )
    )
}

/// The parser consume single empty line or a comment line,
/// including trailing spaces.
fn parse_empty_line<T>(input: T) -> IResult<T, T>
    where
        T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
        T: InputIter + InputLength + InputTake + InputTakeAtPosition,
        T: Offset + Copy + AtEof,
        T: Compare<&'static str>,
        <T as InputIter>::Item: AsChar + Clone,
        <T as InputIter>::RawItem: AsChar + Clone,
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
        <T as InputIter>::Item: AsChar + Clone,
        <T as InputIter>::RawItem: AsChar + Clone,
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

/// Parsers array of values.
/// An array can be empty, it can contain single element.
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
        opt!(
            do_parse!(
                separated_list!(
                    parse_array_separator,
                    parse_value
                ) >>
                space0 >>
                opt!(parse_array_separator) >>
                space0 >>
                (())
            )
        ) >>
        tag!("]") >>
        (())
    )
}

/// HOCON format allows a user specify multiple arrays using two or more consecutive
/// arrays definitions, but only if both arrays are defined on the same line.
///
/// Here I slightly weaken requirements and allow a user to define multiple arrays if the second
/// array begins on the same line on which the first array ended.
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
    fn test_parse_array_separator() {
        // first alternative
        assert_eq!(parse_array_separator(CompleteStr(",")),
                   Ok((CompleteStr(""), CompleteStr(","))));
        assert_eq!(parse_array_separator(CompleteStr("  ,  ")),
                   Ok((CompleteStr(""), CompleteStr("  ,  "))));
        assert_eq!(parse_array_separator(CompleteStr(",\n  ")),
                   Ok((CompleteStr(""), CompleteStr(",\n  "))));
        assert_eq!(parse_array_separator(CompleteStr(",#comment\n  ")),
                   Ok((CompleteStr(""), CompleteStr(",#comment\n  "))));
        assert_eq!(parse_array_separator(CompleteStr(",#comment\n")),
                   Ok((CompleteStr(""), CompleteStr(",#comment\n"))));
        assert_eq!(parse_array_separator(CompleteStr(",#comment")),
                   Ok((CompleteStr(""), CompleteStr(",#comment"))));
        assert_eq!(parse_array_separator(CompleteStr(",#comment1\n\n#comment2\n")),
                   Ok((CompleteStr(""), CompleteStr(",#comment1\n\n#comment2\n"))));
        assert_eq!(parse_array_separator(CompleteStr(",#comment1\n\n#comment2\n,")),
                   Ok((CompleteStr(","), CompleteStr(",#comment1\n\n#comment2\n"))));

        // second alternative
        assert_eq!(parse_array_separator(CompleteStr("#comment\n")),
                   Ok((CompleteStr(""), CompleteStr("#comment\n"))));
        assert_eq!(parse_array_separator(CompleteStr("\n")),
                   Ok((CompleteStr(""), CompleteStr("\n"))));
        assert_eq!(parse_array_separator(CompleteStr("\n\n \n")),
                   Ok((CompleteStr(""), CompleteStr("\n\n \n"))));
        assert_eq!(parse_array_separator(CompleteStr("\n\n \n ,")),
                   Ok((CompleteStr(""), CompleteStr("\n\n \n ,"))));
        assert_eq!(parse_array_separator(CompleteStr("\n\n,#comment1\n\n#comment2\n")),
                   Ok((CompleteStr(""), CompleteStr("\n\n,#comment1\n\n#comment2\n"))));
        assert_eq!(parse_array_separator(CompleteStr("\n\n,#comment1\n,\n#comment2\n")),
                   Ok((CompleteStr(",\n#comment2\n"), CompleteStr("\n\n,#comment1\n"))));
    }

    #[test]
    fn test_parse_comment() {
        assert_eq!(parse_comment(CompleteStr("#comment\n")), Ok((CompleteStr(""), CompleteStr("#comment\n"))), "should parse comment line");
        assert_eq!(parse_comment(CompleteStr("#comment")), Ok( (CompleteStr(""), CompleteStr("#comment"))), "should parse comment if it is the last line in a file");
        assert_eq!(parse_comment(CompleteStr("//comment\n")), Ok( (CompleteStr(""), CompleteStr("//comment\n"))), "should parse comment which starts with // sequence");
    }

    #[test]
    fn test_parse_array() {
        assert_eq!(parse_array(CompleteStr("[]")), Ok((CompleteStr(""), ())));
        assert_eq!(parse_array(CompleteStr("[ ]")), Ok((CompleteStr(""), ())));
        assert_eq!(parse_array(CompleteStr("[ 1 ]")), Ok((CompleteStr(""), ())));
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
