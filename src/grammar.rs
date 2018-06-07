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
#[derive(Debug, Clone)]
enum Substitution {
    Required(Vec<String>),
    Optional(Vec<String>)
}

/// A user may include another file or url. Included configuration will be
/// merged into an object in the context.
#[derive(Debug)]
enum Include {
    Any(String),
    Url(String),
    File(String),
    Classpath(String)
}

/// Users can go crazy when they define a value of a field.
/// Thus, each value consists of zero or more value-chunks.
#[derive(Debug, Clone)]
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

fn double_string<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let char_in_string = parser(|input: &mut I| {
        let (c, next) = try!(any().parse_lazy(input).into());

        let mut back_slash_char = satisfy_map(|c| {
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
        });

        match c {
            '\\' => next.combine(|_| back_slash_char.parse_stream(input)),
            '"' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            _ => Ok((c, next)),
        }
    });

    between(token('"'), token('"'), many(char_in_string))
}

fn triple_quoted_string<I>() -> impl Parser<Input = I, Output=String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let char_in_triple_string = parser(|input: &mut I| {
        let (c, next) = try!(any().parse_lazy(input).into());

        let mut back_slash_char = satisfy_map(|c| {
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
        });

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

fn identifiers<I>() -> impl Parser<Input = I, Output = Vec<String>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    sep_by1(identifier(), token('.'))
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

fn unquoted_string<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let until_line_end = parser(|input: &mut I| {
        let (c, next) = try!(any().parse_lazy(input).into());
        match c {
            '#' | '\n' | '"' | '[' | '{' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
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

fn include<I>() -> impl Parser<Input = I, Output = Include>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    try(string("include")).skip(ws()).skip(ws0()).with(
        choice((
            double_string().map(|s: String| Include::Any(s)),
            between(string("url(").skip(ws0()), token(')').skip(ws0()), double_string()).map(|s: String| Include::Url(s)),
            between(string("file(").skip(ws0()), token(')').skip(ws0()), double_string()).map(|s: String| Include::File(s)),
            between(string("classpath").skip(ws0()), token(')').skip(ws0()), double_string()).map(|s: String| Include::Classpath(s)),
        ))
    )
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
        unquoted_string().map(|s| ValueChunk::Str(s))
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
