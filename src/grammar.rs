extern crate combine;

use std::vec::Vec;

use combine::char::{crlf, newline, string, spaces};
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
#[derive(Clone, PartialEq, Debug)]
enum Include {
    Any(String),
    Url(String),
    File(String),
    Classpath(String)
}

/// Users can go crazy when they define a value of a field.
/// Thus, each value consists of zero or more value-chunks.
#[derive(Clone, PartialEq, Debug)]
enum ValueChunk {
    Str(String),
    Variable(Substitution),
    Array(Vec<ValueChunks>),
    Object(Vec<Field>)
}

#[derive(Clone, PartialEq, Debug)]
struct ValueChunks {
    chunks: Vec<ValueChunk>
}

impl ValueChunks {
    fn is_empty(&self) -> bool {
        self.chunks.is_empty()
    }
}

/// Field's value can be assigned, reassigned or appended to an existing field.
#[derive(Clone, PartialEq, Debug)]
enum FieldValue {
    Assign(ValueChunks),
    Append(ValueChunks),
}

/// I model a field and an include directive because these entities appear in
/// the same position of the grammar.
#[derive(Clone, PartialEq, Debug)]
enum Field {
    Field((Vec<String>, FieldValue)),
    Inc(Include)
}

/// Depending on the context of a value (an array element or a field's value), the
/// parser should stop and certain character. This enum encodes this information.
#[derive(Clone, Copy, Debug)]
enum ValueContext {
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
    unit((
        unit(token('#')).or(unit(try(string("//")))),
        skip_many(satisfy(|c| c != '\n' && c != '\r')),
        choice((
            unit(eof()),
            unit(crlf()),
            unit(newline())
        ))
    ))
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

/// Identifiers in Hocon are not normal variable names. For instance in `akka.conf` a field name
/// may start with slash character. Because the list of allowed characters is unknown and not
/// specified anywhere this function is extracted
fn identifier_char<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy(|c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '$' || c == '/' || c == '\\')
}

/// Recognizes an identifier. The first character of an identifier starts with letter and
/// later characters can be letters, digits, underscores etc.
fn identifier<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(identifier_char())
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
    let opt = between(try(string("${?")).skip(ws0()), ws0().with(string("}")), identifiers())
        .map(Substitution::Optional);

    let req = between(try(string("${")).skip(ws0()), ws0().with(string("}")), identifiers())
        .map(Substitution::Required);

    opt.or(req)
}

/// Hocon format allows a user to omit double-quote symbol when they define a string value.
/// Because a user can use an unquoted string inside arrays and objects we must
/// specify the context in argument `ctx`.
fn unquoted_string<I>(ctx: ValueContext) -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let end_char = match ctx {
        ValueContext::Array => ']',
        ValueContext::Object => '}',
    };

    let until_line_end = parser(move |input: &mut I| {
        let (c, next) = try!(any().parse_lazy(input).into());
        match c {
            '#' | '\n' | '"' | ',' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
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

/// The parser recognizes an array. Arrays in Hocon are similar to json-arrays, but
/// the separator can either comma or newline character.
///
/// Also it is allowed to add a trailing comma.
fn array<I>() -> impl Parser<Input = I, Output = Vec<ValueChunks>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{

    let can_be_element = look_ahead(satisfy(|c: char| c != ']'));

    let array_body = empty_lines().with(
        optional(sep_by(value(ValueContext::Array), try(separator().skip(can_be_element)))).skip(optional(separator())).skip(empty_lines())
    ).map(|x: Option<Vec<ValueChunks>>| {
        match x {
            Some(v) => {
                if v.is_empty() {
                    v
                } else {
                    let last_is_empty = v.last().map(|i| i.is_empty()).unwrap_or(false);
                    if last_is_empty {
                        v[0..v.len() - 1].to_vec()
                    } else {
                        v
                    }
                }
            },
            None => Vec::new()
        }
    });

    between(token('['), token(']'), array_body)
}

/// Fields' names in Hocon format are either enquoted strings as in json, or identifiers.
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

/// Despite its name, this parser recognizes not only fields but also include directives
fn field<I>() -> impl Parser<Input = I, Output = Field>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let object_parser = parser(|input: &mut I| object().parse_stream(input));

    choice((
        include().map(Field::Inc),
        (
            field_name().skip(spaces()),
            choice((
                token(':').or(token('=')).skip(spaces()).with(value(ValueContext::Object)).map(FieldValue::Assign),
                object_parser.map(|fields| {
                    let obj = vec![ValueChunk::Object(fields)];
                    FieldValue::Assign(ValueChunks { chunks: obj })
                }),
                string("+=").skip(ws0()).with(value(ValueContext::Object)).map(FieldValue::Append),
            ))
        ).map(Field::Field)
    ))
}

/// Object's body parser is extracted into a separate function because Hocon
/// format allow a top-level object to omit enclosing curly braces.
fn object_body<I>() -> impl Parser<Input = I, Output = Vec<Field>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let can_field_name = look_ahead(identifier_char().or(token('"')));

    empty_lines().with(
        sep_by(field(), try(separator().skip(can_field_name))).skip(optional(separator())).skip(empty_lines())
    )
}

/// The parser decodes json/hocon object.
fn object<I>() -> impl Parser<Input = I, Output = Vec<Field>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(token('{'), token('}'), object_body())
}

/// The parser recognizes field's value or an element of an array.
/// The value returned as a vector of chunks. These values will be combined when the parser
/// will be resolving substitutions.
fn value<I>(ctx: ValueContext) -> impl Parser<Input = I, Output = ValueChunks>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // Need this to prevent an overflow evaluating the requirement `impl combine::Parser`
    let object_parser = parser(|input: &mut I| object().parse_stream(input));
    let array_parser = parser(|input: &mut I| array().parse_stream(input));

    many(choice((
        triple_quoted_string().map(ValueChunk::Str),
        double_string().map(ValueChunk::Str),
        substitution().map(ValueChunk::Variable),
        object_parser.map(ValueChunk::Object),
        array_parser.map(ValueChunk::Array),
        unquoted_string(ctx).map(ValueChunk::Str)
    ))).map(|x: Vec<ValueChunk>| {

        // if the last value chunk is a string, then all trailing spaces must be
        // removed. If we end up with an empty string, we can omit it entirely.
        if let Some((ValueChunk::Str(last), elements)) = x.split_last() {
            let mut chunks: Vec<ValueChunk> = elements.into();
            let trimmed = last.trim_right();
            if !trimmed.is_empty() {
                chunks.push(ValueChunk::Str(trimmed.into()));
            }
            ValueChunks { chunks }
        } else {
            ValueChunks { chunks: x.clone() }
        }

    })
}

pub fn hocon<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    empty_lines().with(
        choice((
            object(),
            object_body()
        ))
    ).skip(empty_lines()).skip(eof()).map(|_| ())
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
        assert_eq!(identifier().parse("1i."), Ok(("1i".into(), ".")));
        assert_eq!(identifier().parse("/item/item."), Ok(("/item/item".into(), ".")));

        assert!(identifier().parse("").is_err());
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
        assert!(unquoted_string(ValueContext::Array).parse("").is_err());

        assert_eq!(unquoted_string(ValueContext::Array).parse("x"), Ok(("x".into(), "")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("xx"), Ok(("xx".into(), "")));

        assert_eq!(unquoted_string(ValueContext::Array).parse("x\n"), Ok(("x".into(), "\n")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x,"), Ok(("x".into(), ",")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x\r\n"), Ok(("x".into(), "\r\n")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x#"), Ok(("x".into(), "#")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x//"), Ok(("x".into(), "//")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x\""), Ok(("x".into(), "\"")));

        assert_eq!(unquoted_string(ValueContext::Array).parse("x$"), Ok(("x$".into(), "")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x${"), Ok(("x".into(), "${")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x//"), Ok(("x".into(), "//")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x/x"), Ok(("x/x".into(), "")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x/"), Ok(("x/".into(), "")));

        assert_eq!(unquoted_string(ValueContext::Array).parse("x}"), Ok(("x}".into(), "")));
        assert_eq!(unquoted_string(ValueContext::Array).parse("x]"), Ok(("x".into(), "]")));

        assert_eq!(unquoted_string(ValueContext::Object).parse("x}"), Ok(("x".into(), "}")));
        assert_eq!(unquoted_string(ValueContext::Object).parse("x]"), Ok(("x]".into(), "")));
    }

    #[test]
    fn test_include() {
        assert_eq!(include().parse("include \"any\""), Ok((Include::Any("any".into()), "")));
        assert_eq!(include().parse("include url( \"url\" )"), Ok((Include::Url("url".into()), "")));
        assert_eq!(include().parse("include file( \"file\" )"), Ok((Include::File("file".into()), "")));
        assert_eq!(include().parse("include classpath( \"classpath\" )"), Ok((Include::Classpath("classpath".into()), "")));
    }

    #[test]
    fn test_array() {
        assert_eq!(array().parse("[]"), Ok((vec![], "")));
        assert_eq!(array().parse("[  ]"), Ok((vec![], "")));
        assert_eq!(array().parse("[ x ]"), Ok((vec![ValueChunks { chunks: vec![ValueChunk::Str("x".into())]}], "")));
        assert_eq!(array().parse("[x#comment\n]"), Ok((vec![ValueChunks { chunks: vec![ValueChunk::Str("x".into())]}], "")));
        assert_eq!(array().parse("[x,y]"), Ok((vec![ValueChunks { chunks: vec![ValueChunk::Str("x".into())]}, ValueChunks { chunks: vec![ValueChunk::Str("y".into())]}], "")));
        assert_eq!(array().parse("[x,y,]"), Ok((vec![ValueChunks { chunks: vec![ValueChunk::Str("x".into())]}, ValueChunks { chunks: vec![ValueChunk::Str("y".into())]}], "")));
        assert_eq!(array().parse("[#c1\nx#c2\n\n\ny#c3\n,]"), Ok((vec![ValueChunks { chunks: vec![ValueChunk::Str("x".into())]},  ValueChunks { chunks: vec![ValueChunk::Str("y".into())]}], "")));
    }

    #[test]
    fn test_field_name() {
        assert_eq!(field_name().parse("key"), Ok((vec!["key".into()], "")));
        assert_eq!(field_name().parse("key.sub"), Ok((vec!["key".into(), "sub".into()], "")));
        assert_eq!(field_name().parse("\"key.sub\""), Ok((vec!["key.sub".into()], "")));
    }

    #[test]
    fn test_field() {
        assert_eq!(field().parse("key = 123"), Ok((Field::Field((vec!["key".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("123".into())]}))), "")));
        assert_eq!(field().parse("key : 123"), Ok((Field::Field((vec!["key".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("123".into())]}))), "")));
        assert_eq!(field().parse("key += 123"), Ok((Field::Field((vec!["key".into()], FieldValue::Append(ValueChunks { chunks: vec![ValueChunk::Str("123".into())]}))), "")));

        assert_eq!(field().parse("key.sub = 123"), Ok((Field::Field((vec!["key".into(), "sub".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("123".into())]}))), "")));
        assert_eq!(field().parse("key.sub : 123"), Ok((Field::Field((vec!["key".into(), "sub".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("123".into())]}))), "")));
        assert_eq!(field().parse("key.sub += 123"), Ok((Field::Field((vec!["key".into(), "sub".into()], FieldValue::Append(ValueChunks { chunks: vec![ValueChunk::Str("123".into())]}))), "")));

        assert_eq!(field().parse("include \"test\""), Ok((Field::Inc(Include::Any("test".into())), "")));

        assert_eq!(field().parse("key { }"), Ok((Field::Field((vec!["key".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Object(vec![])]}))), "")));
    }

    #[test]
    fn test_object_body() {
        assert_eq!(object_body().parse("key=value"), Ok((
            vec![
                Field::Field((vec!["key".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value".into())]})))
            ], "")));
        assert_eq!(object_body().parse("key=value\n"), Ok((
            vec![
                Field::Field((vec!["key".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value".into())]})))
            ], "")));
        assert_eq!(object_body().parse("key=value#comment\n"), Ok((
            vec![
                Field::Field((vec!["key".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value".into())]})))
            ], "")));

        assert_eq!(object_body().parse("key1=value1,key2=value2"), Ok((
            vec![
                Field::Field((vec!["key1".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value1".into())]}))),
                Field::Field((vec!["key2".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value2".into())]})))
            ], "")));
        assert_eq!(object_body().parse("key1=value1\nkey2=value2"), Ok((
            vec![
                Field::Field((vec!["key1".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value1".into())]}))),
                Field::Field((vec!["key2".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value2".into())]})))
            ], "")));

        assert_eq!(object_body().parse("key1=value1\nkey2=value2\n"), Ok((
            vec![
                Field::Field((vec!["key1".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value1".into())]}))),
                Field::Field((vec!["key2".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value2".into())]})))
            ], "")));
        assert_eq!(object_body().parse("key1=value1\nkey2=value2#comment\n"), Ok((
            vec![
                Field::Field((vec!["key1".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value1".into())]}))),
                Field::Field((vec!["key2".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value2".into())]})))
            ], "")));
        assert_eq!(object_body().parse("key1=value1\nkey2=value2#comment1\n\n#comment2"), Ok((
            vec![
                Field::Field((vec!["key1".into()], FieldValue::Assign(ValueChunks{ chunks: vec![ValueChunk::Str("value1".into())]}))),
                Field::Field((vec!["key2".into()], FieldValue::Assign(ValueChunks{ chunks: vec![ValueChunk::Str("value2".into())]})))
            ], "")));
        assert_eq!(object_body().parse("key1=value1#comment\nkey2=value2"), Ok((
            vec![
                Field::Field((vec!["key1".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value1".into())]}))),
                Field::Field((vec!["key2".into()], FieldValue::Assign(ValueChunks { chunks: vec![ValueChunk::Str("value2".into())]})))
            ], "")));
    }

    #[test]
    fn test_object() {
        assert_eq!(object().parse("{ }"), Ok((vec![], "")));
        assert_eq!(object().parse("{ key=value }"), Ok((
            vec![
                Field::Field((vec!["key".into()], FieldValue::Assign(
                    ValueChunks { chunks: vec![ValueChunk::Str("value".into())] }
                )))
            ], "")));
    }

    #[test]
    fn test_hocon() {
        assert_eq!(hocon().parse(""), Ok(((), "")));
        assert_eq!(hocon().parse("{}"), Ok(((), "")));

        assert_eq!(hocon().parse(" "), Ok(((), "")));
        assert_eq!(hocon().parse("{ }"), Ok(((), "")));

        assert_eq!(hocon().parse("key=value"), Ok(((), "")));
        assert_eq!(hocon().parse("{key=value}"), Ok(((), "")));

        assert_eq!(hocon().parse(" key=value "), Ok(((), "")));
        assert_eq!(hocon().parse("{ key=value }"), Ok(((), "")));
    }

    #[test]
    fn test_akka() {
        let s = r#"
        /IO-DNS/inet-address {
            mailbox = "unbounded"
            router = "consistent-hashing-pool"
            nr-of-instances = 4
        }
        "#;

        assert!(hocon().parse(s).is_ok());
    }
}
