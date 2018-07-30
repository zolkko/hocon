//! Typically lexers recognize regular grammars, they process a char-sequence and extract tokens,
//! they may attach an additional information to a token. Then a stream of tokens is passed
//! into a parser to further processing.
//!
//! In the case of this library we additionally strip out whitespaces, comments and redundant
//! item separators. Actually an initial implementation of the parser worked directly on
//! an input char-sequences and the parser it self was responsible for whitespace and comments
//! recognition, but that made the parser overcomplicated and it took half a minute to compile
//! it, what is annoying at least.

use std::fmt;
use std::fmt::{Display, Formatter};

use combine::{StreamOnce, Positioned};
use combine::error::{StreamError};
use combine::stream::{Resetable};
use combine::easy::{Error, Errors};


#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Token<'a> {
    LeftBrace,
    RightBrace,

    LeftBracket,
    RightBracket,

    Newline,
    Comma,
    Equals,
    Append,
    Dot,

    Include,
    File,
    Url,
    Classpath,

    Unquoted(&'a str),
    String(&'a str),
    Integer(isize),
    Double(f64),
    True,
    False,
    Null,

    Subs,
    OptionalSubs,

    Comment(&'a str),
    Whitespace(&'a str)

}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub(crate) struct Position {
    pub column: usize,
    pub line: usize,
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        unimplemented!()
    }

    fn max(self, other: Self) -> Self where Self: Sized {
        unimplemented!()
    }

    fn min(self, other: Self) -> Self where Self: Sized {
        unimplemented!()
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<::std::cmp::Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        unimplemented!()
    }

    fn le(&self, other: &Self) -> bool {
        unimplemented!()
    }

    fn gt(&self, other: &Self) -> bool {
        unimplemented!()
    }

    fn ge(&self, other: &Self) -> bool {
        unimplemented!()
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "line: {}, column: {}", self.line, self.column)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Checkpoint {
    position: Position,
    offset: usize,
}

#[derive(Debug, PartialEq)]
struct WhitespaceSaver {
    buff: Vec<char>,
    last_was_simple: bool,
}

impl WhitespaceSaver {
    fn new() -> Self {
        WhitespaceSaver {
            buff: Vec::new(),
            last_was_simple: false,
        }
    }

    fn add(&mut self, c: char) {
        self.buff.push(c);
    }
}


#[derive(Debug, PartialEq)]
pub(crate) struct TokenStream<'a> {
    buf: &'a str,
    position: Position,
    offset: usize,
    ws_saver: WhitespaceSaver,
}

impl<'a> Positioned for TokenStream<'a> {
    fn position(&self) -> Self::Position {
        self.position
    }
}

impl<'a> Resetable for TokenStream<'a> {

    type Checkpoint = Checkpoint;

    fn checkpoint(&self) -> Self::Checkpoint {
        Checkpoint {
            position: self.position,
            offset: self.offset,
        }
    }

    fn reset(&mut self, checkpoint: Checkpoint) {
        self.position = checkpoint.position;
        self.offset = checkpoint.offset;
    }
}

impl<'a> StreamOnce for TokenStream<'a> {

    type Item = Token<'a>;
    type Range = Token<'a>;
    type Position = Position;
    type Error = Errors<Token<'a>, Token<'a>, Position>;

    fn uncons(&mut self) -> Result<Self::Item, Error<Token<'a>, Token<'a>>> {
        self.next()
    }
}

impl<'a> TokenStream<'a> {

    /// Create new token stream from a str.
    pub fn new(string: &str) -> TokenStream {
        let mut stream = TokenStream {
            buf: string,
            position: Position { line: 1, column: 1 },
            offset: 0,
            ws_saver: WhitespaceSaver::new(),
        };
        stream
    }

    fn skip_ws(&mut self) {
        for c in self.buf[self.offset..].chars() {
            if is_whitespace_not_newline(c) {
                self.offset += 1;
                self.ws_saver.add(c);
            } else {
                break
            }
        }
    }

    fn next(&mut self) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {

        self.skip_ws();

        let mut iter = self.buf[self.offset..].char_indices();
        let cur_char = match iter.next() {
            Some((_, x)) => x,
            None => return Err(Error::end_of_input()),
        };

        match cur_char {
            '{' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::LeftBrace)
            },
            '}' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::RightBrace)
            },
            '[' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::LeftBracket)
            },
            ']' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::RightBracket)
            },
            '.' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::Dot)
            },
            ',' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::Comma)
            },
            '=' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::Equals)
            },
            '\n' => {
                self.offset += 1;
                self.position.column = 1;
                self.position.line += 1;
                Ok(Token::Newline)
            },
            '+' => self.pull_plus_equal(&mut iter),
            '"' => self.pull_quoted_string(&mut iter),
            '$' => {
                let s = iter.as_str();
                if s.starts_with("{?") {
                    self.position.column += 3;
                    self.offset += 3;
                    Ok(Token::OptionalSubs)
                } else if s.starts_with("{") {
                    self.position.column += 2;
                    self.offset += 2;
                    Ok(Token::Subs)
                } else {
                    let mut iter = self.buf[self.offset..].char_indices();
                    self.pull_unquoted_string(&mut iter)
                }
            },
            '#' => self.pull_hash_comment(&mut iter),
            '/' => self.pull_slash_comment(&mut iter),
            '0'..='9' | '-' => self.pull_number(&mut iter),
            _ => self.pull_unquoted_string(&mut iter)
        }
    }

    fn pull_slash_comment(&mut self, iter: &mut ::std::str::CharIndices) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {

        /*

        let mut sub_iter = iter.peekable();
                    if let Some((_, sub_chr)) = sub_iter.peek() {
                        if *sub_chr == '/' {
                            // TODO:
                        }
                    }*/

        let idx = loop {
            let (idx, chr) = match iter.next() {
                Some(pair) => pair,
                None => break self.buf.len() - self.offset,
            };

            if chr == '\r' || chr == '\n' {
                break idx;
            }
        };

        let comment = &self.buf[self.offset..][..idx];
        self.offset += idx;
        self.position.column += idx + 1;
        Ok(Token::Comment(comment))
    }

    fn pull_hash_comment(&mut self, iter: &mut ::std::str::CharIndices) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {
        let idx = loop {
            let (idx, chr) = match iter.next() {
                Some(pair) => pair,
                None => break self.buf.len() - self.offset,
            };

            if chr == '\r' || chr == '\n' {
                break idx;
            }
        };

        let comment = &self.buf[self.offset..][..idx];
        self.offset += idx;
        self.position.column += idx + 1;
        Ok(Token::Comment(comment))
    }

    /// This implementation does not make an attempt to decode escaped sequences.
    /// Unescaping is going to be performed on the parsing stage.
    fn pull_quoted_string(&mut self, iter: &mut ::std::str::CharIndices) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {
        self.position.column += 1;

        if iter.as_str().starts_with("\"\"") {

            let mut prev = ['"', '"', '"'];
            for (idx, chr) in iter.skip(2) {
                prev[0] = prev[1];
                prev[1] = prev[2];
                prev[2] = chr;

                if chr == '\n' {
                    self.position.column = 1;
                    self.position.line += 1;
                } else {
                    self.position.column += 1;

                    if prev[0] == '"' && prev[1] == '"' && prev[2] == '"' {
                        let string = &self.buf[self.offset + 3..][..idx - 5];
                        self.offset += idx + 1;
                        return Ok(Token::String(string));
                    }
                }
            }

            Err(Error::unexpected_message("unterminated block string value"))
        } else {
            let mut prev_char = '"';
            let mut nchars = 1;
            for (idx, cur_char) in iter {
                nchars += 1;
                match cur_char {
                    '"' if prev_char == '\\' => {},
                    '"' => {
                        self.position.column += nchars;
                        let string = &self.buf[self.offset + 1..][..nchars - 2];
                        self.offset += idx + 1;
                        return Ok(Token::String(string));
                    },
                    '\n' => {
                        return Err(Error::unexpected_message("unterminated string value"));
                    },
                    _ => {
                    }
                }
                prev_char = cur_char;
            }

            Err(Error::unexpected_message("unterminated string value"))
        }
    }

    fn pull_plus_equal(&mut self, iter: &mut impl Iterator<Item=(usize, char)>) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {
        let (idx, cur_char) = iter.next().ok_or_else(|| Error::unexpected_message("expected plus-equals sign"))?;
        if cur_char == '=' {
            self.position.column += 2;
            self.offset += 2;
            Ok(Token::Append)
        } else {
            Err(Error::unexpected_message("expected plus-equals sign"))
        }
    }

    fn pull_number(&mut self, iter: &mut impl Iterator<Item=(usize, char)>) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {

        let mut is_decimal = false;

        let idx = loop {
            let (idx, chr) = match iter.next() {
                Some(pair) => pair,
                None => break self.buf.len() - self.offset,
            };

            if !is_number_char(chr) {
                break idx;
            } else if chr == '.' || chr == 'e' || chr == 'E' {
                is_decimal = true;
            }
        };

        let number_str = &self.buf[self.offset..][..idx];

        let parsed: Result<Token<'a>, Error<Token<'a>, Token<'a>>> = if is_decimal {
            number_str.parse::<f64>().map(|x| {
                self.offset += idx;
                self.position.column += idx + 1;
                Token::Double(x)
            }).map_err(|err| {
                Error::unexpected_message("failed to parse float point number")
            })
        } else {
            number_str.parse::<isize>().map(|x| {
                self.offset += idx;
                self.position.column += idx + 1;
                Token::Integer(x)
            }).map_err(|err| {
                Error::unexpected_message("failed to parse integer number")
            })
        };

        match parsed {
            Ok(token) => Ok(token),
            _ => {
                let mut iter = self.buf[self.offset..].char_indices();
                self.pull_unquoted_string(&mut iter)
            }
        }
    }

    fn pull_unquoted_string(&mut self, iter: &mut ::std::str::CharIndices) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {

        let idx = loop {
            let (idx, chr) = match iter.next() {
                Some(pair) => pair,
                None => break self.buf.len() - self.offset,
            };

            if is_whitespace(chr) || is_not_in_unquoted_text(chr) {
                break idx;
            } else if chr == '#' {
                break idx;
            } else if chr == '/' {
                let mut sub_iter = iter.peekable();
                if let Some((_, next_chr)) = sub_iter.peek() {
                    if *next_chr == '/' {
                        break idx;
                    }
                }
            }

            if idx == 3 {
                let value = &self.buf[self.offset..][..=idx];
                if value == "true" {
                    self.position.column += 4;
                    self.offset += 4;
                    return Ok(Token::True);
                } else if value == "null" {
                    self.position.column += 4;
                    self.offset += 4;
                    return Ok(Token::Null)
                }
            } else if idx == 4 {
                let value = &self.buf[self.offset..][..=idx];
                if value == "false" {
                    self.position.column += 5;
                    self.offset += 5;
                    return Ok(Token::False);
                }
            }
        };

        let value = &self.buf[self.offset..][..idx];
        self.position.column += idx;
        self.offset += idx;

        Ok(Token::Unquoted(value))
    }
}

/// Similar to Lightbend's implementation this function extends
/// Java's set of whitespace characters with Unicode BOM, which is accepted as
/// zero-width non-breaking space.
///
/// In summary a character is a whitespace if it is one of: SPACE_SEPARATOR, LINE_SEPARATOR,
/// PARAGRAPH_SEPARATOR, LINE FEED, VERTICAL TABULATION, FORM FEED, FILE SEPARATOR,
/// GROUP SEPARATOR, RECORD SEPARATOR, UNIT SEPARATOR, UNIT SEPARATOR or BOM.
fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\r' | '\n' | '\u{2007}' | '\u{202f}' | '\u{feff}' | '\u{00b}' |
        '\u{000c}' | '\u{001c}' | '\u{001d}' | '\u{001e}' | '\u{001f}' => true,
        _ => false
    }
}

fn is_whitespace_not_newline(c: char) -> bool {
    c != '\n' && is_whitespace(c)
}

fn is_first_number_char(c: char) -> bool {
    match c {
        '0'..='9' | '-' => true,
        _ => false
    }
}

fn is_number_char(c: char) -> bool {
    match c {
        '0'..='9' | '+' | '-' | '.' | 'e' | 'E' => true,
        _ => false
    }
}

fn is_not_in_unquoted_text(c: char) -> bool {
    match c {
        '$' | '\"' | '{' | '}' | '[' | ']' | ':' | '=' | ',' | '+' | '#' | '`' | '^' | '?' | '!' |
        '@' | '*' | '&' | '\\' => true,
        _ => false
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn parse_tokens<'a>(s: &'a str) -> Vec<Token<'a>> {
        let mut r = Vec::new();
        let mut s = TokenStream::new(s);
        loop {
            match s.uncons() {
                Ok(x) => r.push(x),
                Err(ref e) if e == &Error::end_of_input() => break,
                Err(e) => panic!("Parse error at {}: {:?}", s.position(), e),
            }
        }
        return r;
    }

    #[test]
    fn it_must_regonize_simple_tokens() {
        assert_eq!(parse_tokens("{"), vec![Token::LeftBrace]);
        assert_eq!(parse_tokens("}"), vec![Token::RightBrace]);

        assert_eq!(parse_tokens("["), vec![Token::LeftBracket]);
        assert_eq!(parse_tokens("]"), vec![Token::RightBracket]);

        assert_eq!(parse_tokens("."), vec![Token::Dot]);
        assert_eq!(parse_tokens(","), vec![Token::Comma]);
        assert_eq!(parse_tokens("="), vec![Token::Equals]);
    }

    #[test]
    fn it_must_recognize_ws() {
        assert!(parse_tokens("  ").is_empty());
        assert_eq!(parse_tokens("  { \t"), vec![Token::LeftBrace]);
    }

    #[test]
    fn it_must_recognize_newlines() {
        assert_eq!(parse_tokens("\n"), vec![Token::Newline]);
        assert_eq!(parse_tokens("\r\n"), vec![Token::Newline]);

        assert_eq!(parse_tokens("\r \n"), vec![Token::Newline]);

        let mut stream = TokenStream::new("\r");
        assert!(stream.uncons().is_err())
    }

    #[test]
    fn it_must_recognize_comments() {
        assert_eq!(parse_tokens("# comment"), vec![Token::Comment("# comment")]);
        assert_eq!(parse_tokens("# comment\r\n"), vec![Token::Comment("# comment"), Token::Newline]);
        assert_eq!(parse_tokens("# comment\n"), vec![Token::Comment("# comment"), Token::Newline]);
    }

    #[test]
    fn it_must_recognize_strings() {
        assert_eq!(parse_tokens("\" string value \""), vec![Token::String(" string value ")]);
        assert_eq!(parse_tokens("\"string1\"\"string2\""), vec![Token::String("string1"), Token::String("string2")]);

        assert_eq!(parse_tokens("\"\"\"string value\"\"\""), vec![Token::String("string value")]);
        assert_eq!(parse_tokens("\"\"\"string\r\nvalue\"\"\""), vec![Token::String("string\r\nvalue")]);
        assert_eq!(parse_tokens("\"\"\"string\nvalue\"\"\""), vec![Token::String("string\nvalue")]);
    }

    #[test]
    fn it_must_recognize_substitutions() {
        assert_eq!(parse_tokens("${?"), vec![Token::OptionalSubs]);
        assert_eq!(parse_tokens("${"), vec![Token::Subs]);
        // TODO: regulat token
    }

    #[test]
    fn it_must_recognize_appends() {
        assert_eq!(parse_tokens("+="), vec![Token::Append]);
        // TODO:
    }

    #[test]
    fn it_must_recognize_booleans() {
        assert_eq!(parse_tokens("false"), vec![Token::False]);
        assert_eq!(parse_tokens("false\tfalse"), vec![Token::False, Token::False]);
        assert_eq!(parse_tokens("true"), vec![Token::True]);
        assert_eq!(parse_tokens("true\ttrue"), vec![Token::True, Token::True]);
    }

    #[test]
    fn it_must_recognize_nulls() {
        assert_eq!(parse_tokens("null"), vec![Token::Null]);
        assert_eq!(parse_tokens("null\tnull"), vec![Token::Null, Token::Null]);
    }

    #[test]
    fn it_must_recognize_unquoted_strings() {
        assert_eq!(parse_tokens("non-null"), vec![Token::Unquoted("non-null")]);
        assert_eq!(parse_tokens("non-null#comment"), vec![Token::Unquoted("non-null"), Token::Comment("#comment")]);
    }

    #[test]
    fn it_must_recognize_float_point_numbers() {
        assert_eq!(parse_tokens("123.45"), vec![Token::Double(123.45f64)]);
        assert_eq!(parse_tokens("1e23"), vec![Token::Double(1e23f64)]);
        assert_eq!(parse_tokens("-123.45"), vec![Token::Double(-123.45f64)]);
    }

    #[test]
    fn it_must_recognize_itegers() {
        assert_eq!(parse_tokens("123"), vec![Token::Integer(123)]);
        assert_eq!(parse_tokens("-123"), vec![Token::Integer(-123)]);
        assert_eq!(parse_tokens("123 321"), vec![Token::Integer(123), Token::Integer(321)]);
    }
}
