extern crate combine;

use std::f64;
use std::str::FromStr;
use std::vec::Vec;

use combine::parser::combinator::recognize;
use combine::parser::char::{crlf, newline, string, letter, digit};
use combine::stream::state::State;
use combine::error::Consumed;
use combine::*;


/// Substitutions inside a Hocon file can be either required `${required}`
/// or optional `${?optional.key}`.
/// If required substitution is not found in the context the parser should fail.
#[derive(Clone, PartialEq, Debug)]
enum Substitution {
    Required(Vec<String>),
    Optional(Vec<String>)
}

/// A user may include another file or url. Included configuration will be
/// merged into an object in the context.
#[derive(PartialEq, Debug)]
enum Include {
    Any(String),
    Url(String),
    File(String),
    Classpath(String)
}

/// Users can go crazy when they define a value of a field.
/// Thus, each value consists of zero or more value-chunks.
#[derive(Clone, Debug)]
enum ValueChunk {
    Str(String),
    Variable(Substitution),
    Array(Vec<Vec<ValueChunk>>),
    Object
}

/// Field's value can be assigned, reassigned or appended to an existing field.
#[derive(Debug)]
enum FieldValue {
    Assign(Vec<ValueChunk>),
    Append(Vec<ValueChunk>),
}

/// I model a field and an include directive because these entities appear in
/// the same position of the grammar.
#[derive(Debug)]
enum Field {
    Field((Vec<String>, FieldValue)),
    Inc(Include)
}

#[derive(Clone, Copy, Debug)]
enum UnquotedStringContext {
    Array,
    Object
}

/// Discard return type of the parser which was passed as argument to the function.
fn unit<I>(p: impl Parser<Input = I>) -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    p.map(|_| ())
}

/// This parser matches only spaces and tabulations and it does not
/// consume crlf and newline characters.
fn ws<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    unit(satisfy(|c| c == ' ' || c == '\t'))
}

/// Consume zero or more spaces and tabulation characters.
fn ws0<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_many(ws())
}

/// The parser consume single comment. It stops when it reaches a newline
/// or the end of the file.
fn comment<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let slash_slash = token('/').with(token('/'));
    let comment = unit((
        token('#').or(slash_slash),
        skip_many(satisfy(|c| c != '\n' && c != '\r')),
        choice((
            unit(eof()),
            unit(crlf()),
            unit(newline())
        ))
    ));

    comment
}

/// The parser consumes the empty-line: enter-key or a comment followed by
/// zero or more whitespaces.
fn empty_line<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        unit(crlf()),
        unit(newline()),
        comment(),
    )).skip(ws0())
}

/// The parser consumes zero or more empty-lines.
fn empty_lines<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    ws0().skip(skip_many(empty_line()))
}

/// The separator is a comma or a newline symbol, used to separate elements of an array
/// or fields of an object.
/// All additional whitespaces, comments and newlines must be ignored.
fn separator<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let first_alt = unit((
        token(','),
        empty_lines()
    ));

    let second_alt = unit((
        skip_many1(empty_line()),
        optional(token(',')),
        empty_lines()
    ));

    ws0().with(first_alt.or(second_alt))
}

/// The function escapes a character inside a Hocon string
fn escape_char(c: char) -> Option<char> {
    Some(match c {
        '"' => '"',
        '\\' => '\\',
        '/' => '/',
        'b' => '\u{0008}',
        'f' => '\u{000c}',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        _ => return None,
    })
}

/// The parser recognizes enquoted string and return it's escaped content.
fn double_string<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let char_in_string = parser(|input: &mut I| {
        let (c, next) = try!(any().parse_lazy(input).into());

        let mut back_slash_char = satisfy_map(escape_char);

        match c {
            '\\' => next.combine(|_| back_slash_char.parse_stream(input)),
            '"' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            '\r' | '\n' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            _ => Ok((c, next)),
        }
    });

    between(token('"'), token('"'), many(char_in_string))
}

/// The parser recognizes triple quoted strings as in Python and Scala.
fn triple_quoted_string<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let char_in_triple_string = parser(|input: &mut I| {
        let (c, next) = try!(any().parse_lazy(input).into());

        let mut back_slash_char = satisfy_map(escape_char);

        match c {
            '\\' => next.combine(|_| back_slash_char.parse_stream(input)),
            '"' => {
                match look_ahead(string("\"\"")).parse_stream(input) {
                    Ok(_) => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
                    Err(_) => Ok((c, next))
                }
            },
            _ => Ok((c, next)),
        }
    });

    between(try(string("\"\"\"")), string("\"\"\""), many(char_in_triple_string))
}

/// Recognizes an identifier. The first character of an identifier starts with letter and
/// later characters can be letters, digits, underscores etc.
fn identifier<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    recognize((
        letter(),
        many::<String, _>(satisfy(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '$'))
    ))
}

/// Hocon allows to address objects' sub-fields though dot notation.
fn identifiers<I>() -> impl Parser<Input = I, Output = Vec<String>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    sep_by1(identifier(), token('.'))
}

/// A user can place substitutions of `${required}` and `${?optional}` forms inside
/// fields' values.
fn substitution<I>() -> impl Parser<Input = I, Output = Substitution>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        between(try(string("${?")).skip(ws0()), ws0().with(string("}")), identifiers())
            .map(|x: Vec<String>| Substitution::Optional(x)),
        between(try(string("${")).skip(ws0()), ws0().with(string("}")), identifiers())
            .map(|x: Vec<String>| Substitution::Required(x))
    ))
}

/// Hocon format allows a user to omit double-quote symbol when they define a string value.
/// Because a user can use an unquoted string inside arrays and objects we must
/// specify the context in argument `ctx`.
fn unquoted_string<I>(ctx: UnquotedStringContext) -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let end_char = match ctx {
        UnquotedStringContext::Array => ']',
        UnquotedStringContext::Object => '}',
    };

    let until_line_end = parser(move |input: &mut I| {
        let (c, next) = try!(any().parse_lazy(input).into());
        match c {
            '#' | '\n' | '"' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            v if v == end_char => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            '$' => match look_ahead(token('{')).parse_stream(input) {
                Ok(_) => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
                Err(_) => Ok((c, next))
            },
            '/' => match look_ahead(token('/')).parse_stream(input) {
                Ok(_) => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
                Err(_) => Ok((c, next))
            },
            '\r' => match look_ahead(newline()).parse_stream(input) {
                Ok(_) => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
                Err(_) => Ok((c, next))
            },
            _ => Ok((c, next)),
        }
    });

    many1(until_line_end)
}

/// A user can include another configuration file, its fields must be
/// merged into current object.
fn include<I>() -> impl Parser<Input = I, Output = Include>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    try(string("include")).skip(ws()).skip(ws0()).with(
        choice((
            double_string().map(Include::Any),
            between(string("url(").skip(ws0()), token(')'), double_string().skip(ws0())).map(Include::Url),
            between(string("file(").skip(ws0()), token(')'), double_string().skip(ws0())).map(Include::File),
            between(string("classpath(").skip(ws0()), token(')'), double_string().skip(ws0())).map(Include::Classpath),
        ))
    )
}

fn field_name<I>() -> impl Parser<Input = I, Output = Vec<String>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        double_string().map(|x| vec![x]),
        identifiers()
    ))
}

fn field<I>() -> impl Parser<Input = I, Output = Field>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        include().map(|i| Field::Inc(i)),
        (
            field_name().skip(ws0()),
            choice((
                token(':').or(token('=')).skip(ws0()).with(field_value()).map(|v| FieldValue::Assign(v)),
// TODO: find a way to uncomment following line
//                object().map(|_| FieldValue::Assign(Vec::new())),
                string("+=").skip(ws0()).with(field_value()).map(|v| FieldValue::Append(v)),
            ))
        ).map(|x| Field::Field(x))
    ))
}

fn array<I>() -> impl Parser<Input = I, Output = Vec<Vec<ValueChunk>>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let array_body = empty_lines().with(
        optional(sep_by(field_value(), separator()).skip(ws0()).skip(optional(separator())).skip(ws0()))
    ).map(|x| match x {
        Some(v) => v,
        None => Vec::new()
    });

    between(token('['), token(']'), array_body)
}

/// Object's body parser is extracted into a separate function because Hocon
/// format allow a top-level object to omit enclosing curly braces.
fn object_body<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    ws0().skip(skip_many(empty_line())).with(
        optional(sep_by::<Vec<Field>, _, _>(field(), separator()).skip(ws0()).skip(optional(separator())).skip(ws0()))
    ).map(|maybe_fields| match maybe_fields {
        Some(fields) => {

            for f in fields {
                // TODO: add fields to the object.
                // TODO: values can be resolved over here as well.
                match f {
                    v @ Field::Field(_) => {
                        unimplemented!()
                    },
                    v @ Field::Inc(_) => {
                        unimplemented!()
                    }
                }
            }

            ()
        },
        None => ()
    })
}

/// The parser decodes json/hocon object.
fn object<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(token('{'), token('}'), object_body()).map(|_| ())
}

fn field_value<I>() -> impl Parser<Input = I, Output = Vec<ValueChunk>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many(choice((
        triple_quoted_string().map(|s: String| ValueChunk::Str(s)),
        double_string().map(|s: String| ValueChunk::Str(s)),
        substitution().map(|s: Substitution| ValueChunk::Variable(s)),
// FIXME: uncomment following line
//        object().map(|_| ValueChunk::Object),
//        array().map(|a| ValueChunk::Array(a)),
        unquoted_string(UnquotedStringContext::Object).map(|s| ValueChunk::Str(s))
    ))).map(|x: Vec<ValueChunk>| {

        // if the last value chunk is a string, then all trailing spaces must be
        // removed. If we end up with an empty string, we can omit it entirely.
        if let Some((ValueChunk::Str(last), elements)) = x.split_last() {
            let mut chunks: Vec<ValueChunk> = elements.into();
            let trimmed = last.trim_right();
            if !trimmed.is_empty() {
                chunks.push(ValueChunk::Str(trimmed.into()));
            }
            chunks
        } else {
            x.clone()
        }

    })
}

fn hocon<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    empty_lines().with(
        choice((
            object(),
            object_body()
        ))
    ).skip(empty_lines())
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_ws() {
        assert_eq!(ws().parse(" "), Ok(((), "")));
        assert_eq!(ws().parse("\t"), Ok(((), "")));
        assert_eq!(ws().parse("\t\t"), Ok(((), "\t")));

        assert!(ws().parse("").is_err());
        assert!(ws().parse("\n").is_err());
    }

    #[test]
    fn test_ws0() {
        assert_eq!(ws0().parse(""), Ok(((), "")));
        assert_eq!(ws0().parse("\t"), Ok(((), "")));
        assert_eq!(ws0().parse("\t\t"), Ok(((), "")));
        assert_eq!(ws0().parse("\n"), Ok(((), "\n")));
    }

    #[test]
    fn test_comment() {
        assert_eq!(comment().parse("#comment"), Ok(((), "")));
        assert_eq!(comment().parse("#comment\n"), Ok(((), "")));
        assert_eq!(comment().parse("#comment\r\n"), Ok(((), "")));
        assert_eq!(comment().parse("#comment\nx"), Ok(((), "x")));

        assert_eq!(comment().parse("//comment"), Ok(((), "")));
        assert_eq!(comment().parse("//comment\n"), Ok(((), "")));
        assert_eq!(comment().parse("//comment\r\n"), Ok(((), "")));
        assert_eq!(comment().parse("//comment\nx"), Ok(((), "x")));

        assert!(comment().parse("/x/").is_err());
    }

    #[test]
    fn test_empty_line() {
        assert_eq!(empty_line().parse("\n"), Ok(((), "")));
        assert_eq!(empty_line().parse("\n\n"), Ok(((), "\n")));
        assert_eq!(empty_line().parse("\r\n"), Ok(((), "")));
        assert_eq!(empty_line().parse("#comment"), Ok(((), "")));
        assert_eq!(empty_line().parse("#comment\n"), Ok(((), "")));

        assert!(empty_line().parse("").is_err());
    }

    #[test]
    fn test_empty_lines() {
        assert_eq!(empty_lines().parse(""), Ok(((), "")));
        assert_eq!(empty_lines().parse("\t"), Ok(((), "")));
        assert_eq!(empty_lines().parse("\t#c1\n#c2"), Ok(((), "")));
        assert_eq!(empty_lines().parse("\t#c1\n#c2\nx"), Ok(((), "x")));
    }

    #[test]
    fn test_separator() {
        assert!(separator().parse("").is_err());

        assert_eq!(separator().parse(","), Ok(((), "")));
        assert_eq!(separator().parse(",\n"), Ok(((), "")));
        assert_eq!(separator().parse(",\n#comment"), Ok(((), "")));

        assert_eq!(separator().parse("\n,"), Ok(((), "")));
        assert_eq!(separator().parse("#comment\n\n,"), Ok(((), "")));
        assert_eq!(separator().parse("#comment\n\n,\n"), Ok(((), "")));
        assert_eq!(separator().parse("#comment\n\n,\n#comment"), Ok(((), "")));

        assert_eq!(separator().parse("\n"), Ok(((), "")));
        assert_eq!(separator().parse("#comment"), Ok(((), "")));
        assert_eq!(separator().parse("#comment\n"), Ok(((), "")));
    }

    #[test]
    fn test_escape_char() {
        assert_eq!(escape_char('t'), Some('\t'));
        assert_eq!(escape_char('n'), Some('\n'));
        assert_eq!(escape_char('r'), Some('\r'));
        assert_eq!(escape_char('b'), Some('\u{0008}'));
        assert_eq!(escape_char('f'), Some('\u{000c}'));
        assert_eq!(escape_char('\\'), Some('\\'));
        assert_eq!(escape_char('/'), Some('/'));
        assert_eq!(escape_char('\\'), Some('\\'));
    }

    #[test]
    fn test_double_string() {
        assert!(double_string().parse("").is_err());
        assert!(double_string().parse("\"").is_err());
        assert!(double_string().parse("\"x").is_err());

        assert_eq!(double_string().parse("\"xx\""), Ok(("xx".into(), "")));
        assert!(double_string().parse("\"x\nx\"").is_err());
    }

    #[test]
    fn test_triple_quoted_string() {
        assert_eq!(triple_quoted_string().parse("\"\"\"xx\"\"\""), Ok(("xx".into(), "")));
        assert_eq!(triple_quoted_string().parse("\"\"\"x\nx\"\"\""), Ok(("x\nx".into(), "")));

        assert!(triple_quoted_string().parse("").is_err());
        assert!(triple_quoted_string().parse("\"").is_err());
        assert!(triple_quoted_string().parse("\"\"x").is_err());
        assert!(triple_quoted_string().parse("\"\"\"x").is_err());
    }

    #[test]
    fn test_identifier() {
        assert_eq!(identifier().parse("i"), Ok(("i".into(), "")));
        assert_eq!(identifier().parse("i3"), Ok(("i3".into(), "")));
        assert_eq!(identifier().parse("i3_"), Ok(("i3_".into(), "")));
        assert_eq!(identifier().parse("i3-"), Ok(("i3-".into(), "")));
        assert_eq!(identifier().parse("i3$"), Ok(("i3$".into(), "")));
        assert_eq!(identifier().parse("i3$."), Ok(("i3$".into(), ".")));

        assert!(identifier().parse("").is_err());
        assert!(identifier().parse("1i").is_err());
        assert!(identifier().parse(".").is_err());
    }

    #[test]
    fn test_identifiers() {
        assert!(identifiers().parse("").is_err());
        assert!(identifiers().parse("i1.i2.").is_err());

        assert_eq!(identifiers().parse("i"), Ok((vec!["i".into()], "")));
        assert_eq!(identifiers().parse("key.sub"), Ok((vec!["key".into(), "sub".into()], "")));
    }

    #[test]
    fn test_substitution() {
        assert!(substitution().parse("${}").is_err());
        assert!(substitution().parse("${ }").is_err());

        assert_eq!(substitution().parse("${ x }"), Ok((Substitution::Required(vec!["x".into()]), "")));
        assert_eq!(substitution().parse("${i1.i2}"), Ok((Substitution::Required(vec!["i1".into(), "i2".into()]), "")));

        assert_eq!(substitution().parse("${? x }"), Ok((Substitution::Optional(vec!["x".into()]), "")));
        assert_eq!(substitution().parse("${?i1.i2}"), Ok((Substitution::Optional(vec!["i1".into(), "i2".into()]), "")));
    }

    #[test]
    fn test_unquoted_string() {
        assert!(unquoted_string(UnquotedStringContext::Array).parse("").is_err());

        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x"), Ok(("x".into(), "")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("xx"), Ok(("xx".into(), "")));

        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x\n"), Ok(("x".into(), "\n")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x\r\n"), Ok(("x".into(), "\r\n")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x#"), Ok(("x".into(), "#")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x//"), Ok(("x".into(), "//")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x\""), Ok(("x".into(), "\"")));

        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x$"), Ok(("x$".into(), "")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x${"), Ok(("x".into(), "${")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x//"), Ok(("x".into(), "//")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x/x"), Ok(("x/x".into(), "")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x/"), Ok(("x/".into(), "")));

        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x}"), Ok(("x}".into(), "")));
        assert_eq!(unquoted_string(UnquotedStringContext::Array).parse("x]"), Ok(("x".into(), "]")));

        assert_eq!(unquoted_string(UnquotedStringContext::Object).parse("x}"), Ok(("x".into(), "}")));
        assert_eq!(unquoted_string(UnquotedStringContext::Object).parse("x]"), Ok(("x]".into(), "")));
    }

    #[test]
    fn test_include() {
        assert_eq!(include().parse("include \"any\""), Ok((Include::Any("any".into()), "")));
        assert_eq!(include().parse("include url( \"url\" )"), Ok((Include::Url("url".into()), "")));
        assert_eq!(include().parse("include file( \"file\" )"), Ok((Include::File("file".into()), "")));
        assert_eq!(include().parse("include classpath( \"classpath\" )"), Ok((Include::Classpath("classpath".into()), "")));
    }
}
