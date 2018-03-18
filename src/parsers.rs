use std::str;
use std::mem::transmute;
use std::cmp::PartialEq;
use std::ops::{Range, RangeFrom, RangeTo};

use nom::{alpha, alphanumeric, digit, not_line_ending, recognize_float, space, space0, eol, AsBytes, AsChar,
          AtEof, Compare, IResult, InputIter, InputLength, InputTake, Offset, Slice,
          Err, Needed, ErrorKind, need_more, CompareResult, Context};

named!(
  number<f64>,
  flat_map!(call!(recognize_float), parse_to!(f64))
);

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
  recognize!(
    input,
    do_parse!(
      alt!(tag!("#") | tag!("//")) >>
      call!(not_line_ending) >>
      (())
    )
  )
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
  T: InputIter + InputTake + InputLength,
  T: Offset + AtEof + AsBytes + 'a,
  T: Clone,
  <T as InputIter>::Item: AsChar + Clone,
  <T as InputIter>::RawItem: AsChar + Clone,
{
  map_res!(
    input,
    recognize!(pair!(
      value!((), alpha),
      value!((), opt!(alphanumeric))
    )),
    convert_to_str
  )
}

/// An identifier can be composite. Sub-identifier is separated by dot symbol.
fn identifier<'a, T>(input: T) -> IResult<T, ::std::vec::Vec<&'a str>>
where
  T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
  T: InputIter + InputLength + InputTake,
  T: Offset + AtEof + AsBytes + 'a,
  T: Clone,
  <T as InputIter>::Item: AsChar + Clone,
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
  delimited!(input,
    tag!("\"\"\""),
    take_until_closing_triple_quotes,
    tag!("\"\"\"")
  )
}

fn take_until_closing_double_quotes<'a, T>(input: T) -> IResult<T, T>
where
  T: InputIter + InputLength + InputTake,
  T: AtEof,
  <T as InputIter>::Item: AsChar,
{
  let mut prev = '\0';
  for (i, elem) in input.iter_elements().enumerate() {
    let chr = elem.as_char();
    if prev != '\\' && chr == '"' {
      return Ok(input.take_split(i))
    }
    prev = chr;
  }

  need_more(input, Needed::Size(1))
}

fn double_quoted_string<T>(input: T) -> IResult<T, T>
where
  T: InputIter + InputLength + InputTake,
  T: AtEof,
  T: Compare<&'static str>,
  <T as InputIter>::Item: AsChar,
  T: Clone,
{
  delimited!(input,
    tag!("\""),
    take_until_closing_double_quotes,
    tag!("\"")
  )
}

fn generic_string<T>(input: T) -> IResult<T, T>
where
  T: InputIter + InputLength + InputTake,
  T: AtEof,
  T: Compare<&'static str>,
  <T as InputIter>::Item: AsChar,
{
  let mut index: Option<usize> = None;
  let mut whitespace_pos: Option<usize> = None;

  for (i, elem) in input.iter_elements().enumerate() {
    let chr = elem.as_char();
    if chr == ' ' || chr == '\t' {
      whitespace_pos = whitespace_pos.or(Some(i));
    } else if chr == '#' || chr == ',' || chr == '\r' || chr == '\n' || chr == ']' || chr == '}' {
      // FIXME: terminals ']' and '}' must be checked only in certain contexts
      index = Some(i);
      break;
    } else if chr == '/' {
      let (next, _) = input.take_split(i);
      if next.input_len() < 2 {
        if next.at_eof() {
          whitespace_pos = None;
        } else {
          return need_more(input, Needed::Size(1))
        }
      } else {
        match next.compare("//") {
          CompareResult::Ok => {
            index = Some(i);
            break;
          },
          CompareResult::Incomplete => {
            return need_more(input, Needed::Size(1))
          },
          CompareResult::Error => {
            whitespace_pos = None;
          }
        }
      }
    } else {
      whitespace_pos = None;
    }
  }

  match index {
    Some(i) => {
      Ok(input.take_split(whitespace_pos.unwrap_or(i)))
    },
    None => {
      if input.at_eof() {
        let len = input.input_len();
        Ok(input.take_split(whitespace_pos.unwrap_or(len)))
      } else {
        need_more(input, Needed::Size(1))
      }
    }
  }
}

fn string_value<T>(input: T) -> IResult<T, T>
where
  T: InputIter + InputLength + InputTake,
  T: AtEof,
  T: Compare<&'static str>,
  <T as InputIter>::Item: AsChar,
  T: Clone,
{
  alt_complete!(input,
    triple_quoted_string |
    double_quoted_string |
    generic_string
  )
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
  T: InputIter + InputLength + InputTake + AsBytes + Offset,
  T: AtEof,
  T: Clone + Copy + PartialEq,
  T: Compare<&'static str>,
  <T as InputIter>::Item: AsChar + Clone,
  <T as InputIter>::RawItem: AsChar + Clone,
{
  recognize!(input,
    many0!(
      alt_complete!(
          value!((), space) |
          value!((), tuple!(
            opt!(line_comment),
            eol_or_eof
          ))
      )
    )
  )
}


#[cfg(test)]
mod tests {

  use nom::types::CompleteStr;
  use mem::{MemorySize, MemoryUnit};
  use super::*;

  #[test]
  fn parse_empty_lines() {
    assert_eq!(
      empty_lines("  # some comment\n\n\n  \n   \n# another comment\n  ,"),
      Ok((",", "  # some comment\n\n\n  \n   \n# another comment\n  "))
    )
  }

  /*
  #[test]
  fn parse_array_value() {
    assert_eq!(
      array_value("[ 1, 2 ,3,4]"),
      Ok(("", vec!["1", "2", "3", "4"]))
    );
    assert_eq!(
      array_value("[ 1, 2 ,3,4,  ]"),
      Ok(("", vec!["1", "2", "3", "4", ""]))
    );
    assert_eq!(
      array_value("[ 1\n 2 \n3\n4\n  ]"),
      Ok(("", vec!["1", "2", "3", "4", ""]))
    );
  }*/

  #[test]
  fn parse_string_value() {
    assert_eq!(
      string_value(CompleteStr("value of unknown type")),
      Ok((CompleteStr(""), CompleteStr("value of unknown type"))),
      "must recognize generic value"
    );
    assert_eq!(
      string_value("value of unknown type\n"),
      Ok(("\n", "value of unknown type")),
      "must recognize generic value with ends with terminal symbol"
    );
    assert_eq!(
      string_value("\"quoted string\""),
      Ok(("", "quoted string")),
      "must recognize double-quoted string"
    );
    assert_eq!(
      string_value("\"\"\"quoted string\"\"\""),
      Ok(("", "quoted string")),
      "must recognize triple-quoted string"
    );
  }

  #[test]
  fn parse_generic_string() {
    assert_eq!(generic_string(CompleteStr(&"123mb")), Ok((CompleteStr(&""), CompleteStr(&"123mb"))));
    assert_eq!(generic_string(CompleteStr(&"123mb   ")), Ok((CompleteStr(&"   "), CompleteStr(&"123mb"))));
    assert_eq!(generic_string(CompleteStr(&"123 mb")), Ok((CompleteStr(&""), CompleteStr(&"123 mb"))));
    assert_eq!(generic_string("123 mb\n"), Ok(("\n", "123 mb")));
    assert_eq!(generic_string("123 mb #comment"), Ok((" #comment", "123 mb")));
    assert_eq!(generic_string("123 mb // comment"), Ok((" // comment", "123 mb")));
    assert_eq!(generic_string("123 mb  //"), Ok(("  //", "123 mb")));
    assert_eq!(generic_string("123 mb  //"), Ok(("  //", "123 mb")));
    assert_eq!(generic_string("123 mb//"), Ok(("//", "123 mb")));
    assert_eq!(generic_string(CompleteStr("123 mb  /")), Ok((CompleteStr(""), CompleteStr("123 mb  /"))));
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
      Result::Err(Err::Incomplete(Needed::Unknown)),
      "should report that input is incomplete (missing one?)"
    );
    assert_eq!(
      take_until_closing_triple_quotes("ends with triple quotes\""),
      Result::Err(Err::Incomplete(Needed::Unknown)),
      "should report that input is incomplete (missing two?)"
    );

    assert_eq!(
      triple_quoted_string("\"\"\"hello world\"\"\""),
      Ok(("", "hello world")),
      "should recognize triple quoted strings"
    );
    assert_eq!(
      triple_quoted_string("\"\"\"hello world\"\""),
      Result::Err(Err::Incomplete(Needed::Unknown)),
      "should not recognize incomplete triple quoted strings (one quote is missing)"
    );
    assert_eq!(
      triple_quoted_string("\"\"\"hello world\""),
      Result::Err(Err::Incomplete(Needed::Unknown)),
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
    )
  }

  #[test]
  fn parse_quoted_string() {
    assert_eq!(
      take_until_closing_double_quotes("\\n\r \\\"hello\\\" all\" "),
      Ok(("\" ", "\\n\r \\\"hello\\\" all")),
      "must consume entire row"
    );
    assert_eq!(
      double_quoted_string("\"hello world!\""),
      Ok(("", "hello world!")),
      "must recognize quoted string"
    );
    assert_eq!(
      double_quoted_string("\"\""),
      Ok(("", "")),
      "must recognize empty quoted string"
    );
    assert_eq!(
      double_quoted_string("\"Hello, \\\"username\\\"!\\\n\nHow are your doing?\""),
      Ok((
        "",
        "Hello, \\\"username\\\"!\\\n\nHow are your doing?"
      )),
      "must handle escaped characters"
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
