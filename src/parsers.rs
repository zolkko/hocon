use std::str;
use std::mem::transmute;
use std::ops::{Range, RangeFrom, RangeTo};

use nom::{alpha, alphanumeric, digit, not_line_ending, recognize_float, space, AsBytes, AsChar,
          AtEof, Compare, IResult, InputIter, InputLength, InputTake, Offset, Slice,
          Err, Needed, ErrorKind, need_more, CompareResult, Context};


use super::mem::{MemorySize, MemoryUnit};

#[derive(Debug, PartialEq)]
enum DurationUnit {
  Nanoseconds,
  Microseconds,
  Milliseconds,
  Seconds,
  Minutes,
  Hours,
  Days,
}

named!(
  nanoseconds,
  alt_complete!(tag!(b"ns") | tag!(b"nanoseconds") | tag!(b"nanosecond") | tag!(b"nanos") | tag!(b"nano"))
);

named!(
  microseconds,
  alt_complete!(tag!(b"us") | tag!(b"microseconds") | tag!(b"microsecond") | tag!(b"micros") | tag!(b"micro"))
);

named!(
  milliseconds,
  alt_complete!(tag!(b"ms") | tag!(b"milliseconds") | tag!(b"millisecond") | tag!(b"millis") | tag!(b"milli"))
);

named!(
  seconds,
  alt_complete!(tag!(b"seconds") | tag!(b"second") | tag!(b"s"))
);

named!(
  minutes,
  alt_complete!(tag!(b"minutes") | tag!(b"minute") | tag!(b"mm"))
);

named!(
  hours,
  alt_complete!(tag!(b"hours") | tag!(b"hour") | tag!(b"h"))
);

named!(
  days,
  alt_complete!(tag!(b"days") | tag!(b"day") | tag!(b"d"))
);

named!(
  duration_unit<DurationUnit>,
  alt!(
    nanoseconds => { |_| DurationUnit::Nanoseconds } |
    microseconds => { |_| DurationUnit::Microseconds } |
    milliseconds => { |_| DurationUnit::Milliseconds } |
    seconds => { |_| DurationUnit::Seconds } |
    minutes => { |_| DurationUnit::Minutes } |
    hours => { |_| DurationUnit::Hours } |
    days => { |_| DurationUnit::Days }
  )
);

named!(
  duration<::std::time::Duration>,
  map!(
    separated_pair!(flat_map!(digit, parse_to!(u64)), opt!(space), duration_unit),
    |(v, du): (u64, DurationUnit)| match du {
      DurationUnit::Nanoseconds => {
        if v <= 0xff_ff_ff_ff {
          ::std::time::Duration::new(0, v as u32)
        } else {
          ::std::time::Duration::new(v / 1_000_000_000, (v % 1_000_000_000) as u32)
        }
      }
      DurationUnit::Microseconds => ::std::time::Duration::from_micros(v),
      DurationUnit::Milliseconds => ::std::time::Duration::from_millis(v),
      DurationUnit::Seconds => ::std::time::Duration::new(v, 0),
      DurationUnit::Minutes => ::std::time::Duration::new(v * 60, 0),
      DurationUnit::Hours => ::std::time::Duration::new(v * 60 * 60, 0),
      DurationUnit::Days => ::std::time::Duration::new(v * 60 * 60 * 24, 0),
    }
  )
);

named!(
  mem_bytes,
  alt_complete!(tag!(b"bytes") | tag!(b"byte") | tag!(b"b") | tag!(b"B"))
);

named!(
  mem_kilobytes,
  alt_complete!(tag!(b"kilobytes") | tag!(b"kilobyte") | tag!(b"kB"))
);

named!(
  mem_kibibytes,
  alt_complete!(tag!(b"kibibytes") | tag!(b"kibibyte") | tag!(b"KiB") | tag!(b"Ki") | tag!(b"K") | tag!(b"k"))
);

named!(
  mem_megabytes,
  alt_complete!(tag!(b"megabytes") | tag!(b"megabyte") | tag!(b"MB"))
);

named!(
  mem_mebibytes,
  alt_complete!(tag!(b"mebibytes") | tag!(b"mebibyte") | tag!(b"MiB") | tag!(b"Mi") | tag!(b"M") | tag!(b"m"))
);

named!(
  mem_gigabytes,
  alt_complete!(tag!(b"gigabytes") | tag!(b"gigabyte") | tag!(b"GB"))
);

named!(
  mem_gibibytes,
  alt_complete!(tag!(b"gibibytes") | tag!(b"gibibyte") | tag!(b"GiB") | tag!(b"Gi") | tag!(b"G") | tag!(b"g"))
);

named!(
  mem_terabytes,
  alt_complete!(tag!(b"terabytes") | tag!(b"terabyte") | tag!(b"TB"))
);

named!(
  mem_tebibytes,
  alt_complete!(tag!(b"tebibytes") | tag!(b"tebibyte") | tag!(b"TiB") | tag!(b"Ti") | tag!(b"T") | tag!(b"t"))
);

named!(
  mem_petabytes,
  alt_complete!(tag!(b"petabytes") | tag!(b"petabyte") | tag!(b"PB"))
);

named!(
  mem_pebibytes,
  alt_complete!(tag!(b"pebibytes") | tag!(b"pebibyte") | tag!(b"PiB") | tag!(b"Pi") | tag!(b"P") | tag!(b"p"))
);

named!(
  mem_exabytes,
  alt_complete!(tag!(b"exabytes") | tag!(b"exabyte") | tag!(b"EB"))
);

named!(
  mem_exbibytes,
  alt_complete!(tag!(b"exbibytes") | tag!(b"exbibyte") | tag!(b"EiB") | tag!(b"Ei") | tag!(b"E") | tag!(b"e"))
);

named!(
  mem_zettabytes,
  alt_complete!(tag!(b"zettabytes") | tag!(b"zettabyte") | tag!(b"ZB"))
);

named!(
  mem_zebibytes,
  alt_complete!(tag!(b"zebibytes") | tag!(b"zebibyte") | tag!(b"ZiB") | tag!(b"Zi") | tag!(b"Z") | tag!(b"z"))
);

named!(
  mem_yottabytes,
  alt_complete!(tag!(b"yottabytes") | tag!(b"yottabyte") | tag!(b"YB"))
);

named!(
  mem_yobibytes,
  alt_complete!(tag!(b"yobibytes") | tag!(b"yobibyte") | tag!(b"YiB") | tag!(b"Yi") | tag!(b"Y") | tag!(b"y"))
);

named!(
  memory_unit<MemoryUnit>,
  alt!(
    mem_bytes => { |_| MemoryUnit::Bytes } |
    mem_kilobytes => { |_| MemoryUnit::Kilobytes } |
    mem_kibibytes => { |_| MemoryUnit::Kibibytes } |
    mem_megabytes => { |_| MemoryUnit::Megabytes } |
    mem_mebibytes => { |_| MemoryUnit::Mebibytes } |
    mem_gigabytes => { |_| MemoryUnit::Gigabytes } |
    mem_gibibytes => { |_| MemoryUnit::Gibibytes } |
    mem_terabytes => { |_| MemoryUnit::Terabytes } |
    mem_tebibytes => { |_| MemoryUnit::Tebibytes } |
    mem_petabytes => { |_| MemoryUnit::Petabytes } |
    mem_pebibytes => { |_| MemoryUnit::Pebibytes } |
    mem_exabytes => { |_| MemoryUnit::Exabytes } |
    mem_exbibytes => { |_| MemoryUnit::Exbibytes } |
    mem_zettabytes => { |_| MemoryUnit::Zettabytes } |
    mem_zebibytes => { |_| MemoryUnit::Zebibytes } |
    mem_yottabytes => { |_| MemoryUnit::Yottabytes } |
    mem_yobibytes => { |_| MemoryUnit::Yobibytes }
  )
);

named!(
  memory_size<MemorySize>,
  map!(
    separated_pair!(flat_map!(digit, parse_to!(u32)), opt!(space), memory_unit),
    |(v, mu): (u32, MemoryUnit)| MemorySize::new(v, mu)
  )
);

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

/// Consume a single line comment
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
    } else if chr == '#' || chr == ',' || chr == '\r' || chr == '\n' {
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

#[cfg(test)]
mod tests {

  use nom::types::CompleteStr;
  use mem::{MemorySize, MemoryUnit};
  use super::*;

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

  macro_rules! assert_duration (
    ($unit:expr, $expect:expr, $v:expr) => {
      assert_eq!(duration_unit($v.as_bytes()), Ok((&b""[..], $unit)));
      assert_eq!(duration(format!("100{}", $v).as_bytes()), Ok((&b""[..], $expect)));
      assert_eq!(duration(format!("100 {}", $v).as_bytes()), Ok((&b""[..], $expect)));
      assert_eq!(duration(format!("100  {}", $v).as_bytes()), Ok((&b""[..], $expect)));
    };
    ($unit:expr, $expect:expr, $v:expr, $($rest:expr),+) => {
      assert_duration!($unit, $expect, $v);
      assert_duration!($unit, $expect, $($rest),+);
    }
  );

  #[test]
  fn duration_should_recognize_nanoseconds() {
    assert_duration!(
      DurationUnit::Nanoseconds,
      ::std::time::Duration::new(0, 100),
      "nanoseconds",
      "nanosecond",
      "nanos",
      "nano",
      "ns"
    );
  }

  #[test]
  fn duration_should_support_big_nanoseconds_values() {
    assert_eq!(
      duration(b"4294967395nanoseconds"),
      Ok((&b""[..], ::std::time::Duration::new(4, 294967395)))
    );
  }

  #[test]
  fn duration_should_recognize_microseconds() {
    assert_duration!(
      DurationUnit::Microseconds,
      ::std::time::Duration::from_micros(100),
      "microseconds",
      "microsecond",
      "micros",
      "micro",
      "us"
    );
  }

  #[test]
  fn duration_should_recognize_milliseconds() {
    assert_duration!(
      DurationUnit::Milliseconds,
      ::std::time::Duration::from_millis(100),
      "milliseconds",
      "millisecond",
      "millis",
      "milli",
      "ms"
    );
  }

  #[test]
  fn duration_should_recognize_seconds() {
    assert_duration!(
      DurationUnit::Seconds,
      ::std::time::Duration::new(100, 0),
      "seconds",
      "second",
      "s"
    );
  }

  #[test]
  fn duration_should_recognize_minutes() {
    assert_duration!(
      DurationUnit::Minutes,
      ::std::time::Duration::new(100 * 60, 0),
      "minutes",
      "minute",
      "mm"
    );
  }

  #[test]
  fn duration_should_recognize_hours() {
    assert_duration!(
      DurationUnit::Hours,
      ::std::time::Duration::new(100 * 60 * 60, 0),
      "hours",
      "hour",
      "h"
    );
  }

  #[test]
  fn duration_should_recognize_days() {
    assert_duration!(
      DurationUnit::Days,
      ::std::time::Duration::new(100 * 60 * 60 * 24, 0),
      "days",
      "day",
      "d"
    );
  }

  macro_rules! assert_memory_size (
    ($mem_unit:expr, $value:expr, $($rest:expr),+) => {
      assert_memory_size!($mem_unit, $value);
      assert_memory_size!($mem_unit, $($rest),+);
    };
    ($mem_unit:expr, $value:expr) => {
      assert_eq!(memory_size(format!("123{}", $value).as_bytes()), Ok((&b""[..], MemorySize::new(123, $mem_unit))));
      assert_eq!(memory_size(format!("321 {}", $value).as_bytes()), Ok((&b""[..], MemorySize::new(321, $mem_unit))));
      assert_eq!(memory_size(format!("213  {}", $value).as_bytes()), Ok((&b""[..], MemorySize::new(213, $mem_unit))));
    }
  );

  #[test]
  fn memory_size_should_recognize_bytes() {
    assert_memory_size!(MemoryUnit::Bytes, "B", "b", "byte", "bytes");
  }

  #[test]
  fn memory_size_should_recognize_kilobytes() {
    assert_memory_size!(MemoryUnit::Kilobytes, "kilobytes", "kilobyte", "kB");
  }

  #[test]
  fn memory_size_should_recognize_kibibytes() {
    assert_memory_size!(
      MemoryUnit::Kibibytes,
      "kibibytes",
      "kibibyte",
      "KiB",
      "Ki",
      "K",
      "k"
    );
  }

  #[test]
  fn memory_size_should_recognize_megabytes() {
    assert_memory_size!(MemoryUnit::Megabytes, "megabytes", "megabyte", "MB");
  }

  #[test]
  fn memory_size_should_recognize_mebibytes() {
    assert_memory_size!(
      MemoryUnit::Mebibytes,
      "mebibytes",
      "mebibyte",
      "MiB",
      "Mi",
      "M",
      "m"
    );
  }

  #[test]
  fn memory_size_should_recognize_gigabytes() {
    assert_memory_size!(MemoryUnit::Gigabytes, "gigabytes", "gigabyte", "GB");
  }

  #[test]
  fn memory_size_should_recognize_gibibytes() {
    assert_memory_size!(
      MemoryUnit::Gibibytes,
      "gibibytes",
      "gibibyte",
      "GiB",
      "Gi",
      "G",
      "g"
    );
  }

  #[test]
  fn memory_size_should_recognize_terabytes() {
    assert_memory_size!(MemoryUnit::Terabytes, "terabytes", "terabyte", "TB");
  }

  #[test]
  fn memory_size_should_recognize_tebibytes() {
    assert_memory_size!(
      MemoryUnit::Tebibytes,
      "tebibytes",
      "tebibyte",
      "TiB",
      "Ti",
      "T",
      "t"
    );
  }

  #[test]
  fn memory_size_should_recognize_petabytes() {
    assert_memory_size!(MemoryUnit::Petabytes, "petabytes", "petabyte", "PB");
  }

  #[test]
  fn memory_size_should_recognize_pebibytes() {
    assert_memory_size!(
      MemoryUnit::Pebibytes,
      "pebibytes",
      "pebibyte",
      "PiB",
      "Pi",
      "P",
      "p"
    );
  }

  #[test]
  fn memory_size_should_recognize_exabytes() {
    assert_memory_size!(MemoryUnit::Exabytes, "exabytes", "exabyte", "EB");
  }

  #[test]
  fn memory_size_should_recognize_exbibytes() {
    assert_memory_size!(
      MemoryUnit::Exbibytes,
      "exbibytes",
      "exbibyte",
      "EiB",
      "Ei",
      "E",
      "e"
    );
  }

  #[test]
  fn memory_size_should_recognize_zettabytes() {
    assert_memory_size!(MemoryUnit::Zettabytes, "zettabytes", "zettabyte", "ZB");
  }

  #[test]
  fn memory_size_should_recognize_zebibytes() {
    assert_memory_size!(
      MemoryUnit::Zebibytes,
      "zebibytes",
      "zebibyte",
      "ZiB",
      "Zi",
      "Z",
      "z"
    );
  }

  #[test]
  fn memory_size_should_recognize_yottabytes() {
    assert_memory_size!(MemoryUnit::Yottabytes, "yottabytes", "yottabyte", "YB");
  }

  #[test]
  fn memory_size_should_recognize_yobibytes() {
    assert_memory_size!(MemoryUnit::Yobibytes, "yobibytes", "yobibyte", "YiB", "Yi", "Y", "y");
  }
}
