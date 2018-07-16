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

    LeftRoundBracket,
    RightRoundBracket,

    Newline,
    Comma,
    Equals,
    Append,
    Dot,

    Include,
    File,
    Url,
    Classpath,

    Identifier(&'a str),
    String(&'a str),
    Integer(isize),
    Double(f64),

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
pub(crate) struct TokenStream<'a> {
    buf: &'a str,
    position: Position,
    offset: usize,
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
        use std::io::Read;
        let mut stream = TokenStream {
            buf: string,
            position: Position { line: 1, column: 1 },
            offset: 0,
        };
        stream
    }

    /*
    /// Advances the stream to the next non-whitespace token, comments are also skipped.
    /// Newline characters are not consumed.
    fn skip_whitespace(&mut self) {
        let mut iter = self.buf[self.offset..].char_indices();

        let idx = loop {
            let (idx, cur_char) = match iter.next() {
                Some(pair) => pair,
                None => break self.buf.len() - self.offset,
            };

            match cur_char {
                '\u{feff}' | '\t' | ' ' => {
                    self.position.column += 1
                },
                '#' => {
                    while let Some((_, cur_char)) = iter.next() {
                        if cur_char == '\n' {
                            self.position.column = 1;
                            self.position.line += 1;
                            break;
                        }
                    }
                },
                '/' => {
                    if let Some((_, cur_char)) = iter.next() {
                        if cur_char == '/' {
                            while let Some((_, cur_char)) = iter.next() {
                                if cur_char == '\n' {
                                    self.position.column = 1;
                                    self.position.line += 1;
                                    break;
                                }
                            }
                            continue // outer loop
                        }
                    }
                    break idx
                },
                _ => break idx,
            }
        };

        self.offset += idx;
    }

    fn skip_crlf(&mut self) -> Result<(), Error<Token<'a>, Token<'a>>> {
        let mut iter = self.buf[self.offset..].chars();

        if let Some(chr) = iter.next() {
            match chr {
                '\r' => {
                    if let Some(chr) = iter.next() {
                        if chr == '\n' {
                            self.offset += 2;
                            self.position.column = 1;
                            self.position.line += 1;
                            return Ok(())
                        }
                    }

                    Err(Error::unexpected_message("unterminated block string value"))
                },
                '\n' => {
                    self.offset += 1;
                    self.position.column = 1;
                    self.position.line += 1;
                    Ok(())
                },
                _ => Ok(()),
            }
        } else {
            Ok(())
        }
    }
    */

    fn next(&mut self) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {

        println!("next called");

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
            '(' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::LeftRoundBracket)
            },
            ')' => {
                self.offset += 1;
                self.position.column += 1;
                Ok(Token::RightRoundBracket)
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
            '\r' => {
                if let Some((_, chr)) = iter.next() {
                    if chr == '\n' {
                        self.offset += 2;
                        self.position.column = 1;
                        self.position.line += 1;
                        return Ok(Token::Newline)
                    }
                }
                Err(Error::unexpected_message("malformed newline value"))
            },
            '#' => {
                self.position.column += 1;

                let idx = loop {
                    let (idx, cur_char) = match iter.next() {
                        Some(pair) => pair,
                        None => break self.buf.len() - self.offset,
                    };

                    match cur_char {
                        '\r' | '\n' => break idx,
                        _ => {
                            self.position.column += 1;
                        }
                    }
                };

                let comment = &self.buf[self.offset..][..idx];
                self.offset += idx;
                Ok(Token::Comment(comment))
            },
            '\u{feff}' | '\t' | ' ' => {

                self.position.column += 1;

                let idx = loop {
                    let (idx, cur_char) = match iter.next() {
                        Some(pair) => pair,
                        None => break self.buf.len() - self.offset,
                    };

                    match cur_char {
                        '\u{feff}' | '\t' | ' ' => self.position.column += 1,
                        _ => break idx,
                    }
                };

                let substr = &self.buf[self.offset..][..idx];
                self.offset += idx;
                Ok(Token::Whitespace(substr))
            },
            '"' => {

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
                    let mut prev_char = cur_char;
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
            },
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
                    unimplemented!()
                }
            },
            '+' => {
                // can be a number or append sign or regular value

                let (idx, cur_char) = match iter.next() {
                    Some(pair) => pair,
                    None => unimplemented!(),
                };

                if cur_char == '=' {
                    self.position.column += 2;
                    self.offset += 2;
                    Ok(Token::Append)
                } else {
                    unimplemented!()
                }

            },
            'i' => {
                let s = iter.as_str();

                let tok_end = |c: char| -> bool {
                    !c.is_alphanumeric() && c != '_' && c != '-' && c != '$'
                };

                if s.starts_with("nclude") && s.chars().nth(6).map(tok_end).unwrap_or(true) {
                    self.position.column += 7;
                    self.offset += 7;
                    Ok(Token::Include)
                } else {
                    unimplemented!()
                }
            },
            _ => {
                unimplemented!()
            }
        }

    }

    /*
    fn next_token(&mut self) -> Result<Token<'a>, Error<Token<'a>, Token<'a>>> {

        let mut iter = self.buf[self.offset..].char_indices();

        let cur_char = match iter.next() {
            Some((_, x)) => x,
            None => return Err(Error::end_of_input()),
        };

        match cur_char {
            '{' => {
                self.offset += 1;
                Ok(Token::LeftBrace)
            },
            '}' => {
                self.offset += 1;
                Ok(Token::RightBrace)
            },
            '[' => {
                self.offset += 1;
                Ok(Token::LeftBracket)
            },
            ']' => {
                self.offset += 1;
                Ok(Token::RightBrace)
            },
            '.' => {
                self.offset += 1;
                Ok(Token::Dot)
            },
            '"' => {
                if iter.as_str().starts_with("\"\"") {
                    let tail = &iter.as_str()[2..];
                    for (end_idx, _) in tail.match_indices("\"\"\"") {
                        if !tail[..end_idx].ends_with('\\') {
                            self.update_position(end_idx + 6);
                            return Ok((BlockString, end_idx + 6));
                        }
                    }

                    Err(Error::unexpected_message("unterminated block string value"))

                } else {
                    let mut prev_char = cur_char;
                    let mut nchars = 1;
                    for (idx, cur_char) in iter {
                        nchars += 1;
                        match cur_char {
                            '"' if prev_char == '\\' => {}
                            '"' => {
                                self.position.column += nchars;
                                self.offset += idx + 1;
                                return Ok((StringValue, idx+1));
                            }
                            '\n' => {
                                return Err(Error::unexpected_message("unterminated string value"));
                            }
                            _ => {
                            }
                        }
                        prev_char = cur_char;
                    }
                    Err(Error::unexpected_message("unterminated string value"))
                }

                // todo string
            }
            'i' | 'I' => {
                return Ok((Token::Include))
            },
            _ => {
                return Ok((Token::Include))
            }
        }

        match cur_char {
            '!' | '$' | ':' | '=' | '@' | '|' |
            '(' | ')' | '[' | ']' | '{' | '}' | '&' => {
                self.position.column += 1;
                self.off += 1;

                Ok((Punctuator, 1))
            }
            '.' => {
                if iter.as_str().starts_with("..") {
                    self.position.column += 3;
                    self.off += 3;

                    Ok((Punctuator, 3))
                } else {
                    Err(
                        Error::unexpected_message(
                            format_args!("bare dot {:?} is not supported, \
                            only \"...\"", cur_char)
                        )
                    )
                }
            }
            '_' | 'a'...'z' | 'A'...'Z' => {
                while let Some((idx, cur_char)) = iter.next() {
                    match cur_char {
                        '_' | 'a'...'z' | 'A'...'Z' | '0'...'9' => continue,
                        _ => {
                            self.position.column += idx;
                            self.off += idx;
                            return Ok((Name, idx));
                        }
                    }
                }
                let len = self.buf.len() - self.off;
                self.position.column += len;
                self.off += len;

                Ok((Name, len))
            }
            '-' | '0'...'9' => {
                let mut exponent = None;
                let mut real = None;
                let len = loop {
                    let (idx, cur_char) = match iter.next() {
                        Some(pair) => pair,
                        None => break self.buf.len() - self.off,
                    };
                    match cur_char {
                        // just scan for now, will validate later on
                        ' ' | '\n' | '\r' | '\t' | ',' | '#' |
                        '!' | '$' | ':' | '=' | '@' | '|' | '&' |
                        '(' | ')' | '[' | ']' | '{' | '}'
                        => break idx,
                        '.' => real = Some(idx),
                        'e' | 'E' => exponent = Some(idx),
                        _ => {},
                    }
                };

                if exponent.is_some() || real.is_some() {
                    let value = &self.buf[self.off..][..len];
                    if !check_float(value, exponent, real) {
                        return Err(
                            Error::unexpected_message(
                                format_args!("unsupported float {:?}", value)
                            )
                        );
                    }
                    self.position.column += len;
                    self.off += len;

                    Ok((FloatValue, len))
                } else {
                    let value = &self.buf[self.off..][..len];
                    if !check_int(value) {
                        return Err(
                            Error::unexpected_message(
                                format_args!("unsupported integer {:?}", value)
                            )
                        );
                    }
                    self.position.column += len;
                    self.off += len;

                    Ok((IntValue, len))
                }
            }
            '"' => {
                if iter.as_str().starts_with("\"\"") {
                    let tail = &iter.as_str()[2..];
                    for (end_idx, _) in tail.match_indices("\"\"\"") {
                        if !tail[..end_idx].ends_with('\\') {
                            self.update_position(end_idx + 6);
                            return Ok((BlockString, end_idx + 6));
                        }
                    }

                    Err(
                        Error::unexpected_message(
                            "unterminated block string value"
                        )
                    )
                } else {
                    let mut prev_char = cur_char;
                    let mut nchars = 1;
                    for (idx, cur_char) in iter {
                        nchars += 1;
                        match cur_char {
                            '"' if prev_char == '\\' => {}
                            '"' => {
                                self.position.column += nchars;
                                self.off += idx+1;
                                return Ok((StringValue, idx+1));
                            }
                            '\n' => {
                                return Err(
                                    Error::unexpected_message(
                                        "unterminated string value"
                                    )
                                );
                            }
                            _ => {

                            }
                        }
                        prev_char = cur_char;
                    }
                    Err(
                        Error::unexpected_message(
                            "unterminated string value"
                        )
                    )
                }
            }
            _ => Err(
                Error::unexpected_message(
                    format_args!("unexpected character {:?}", cur_char)
                )
            ),
        }
    }
    */

    /*
    fn update_position(&mut self, len: usize) {
        let val = &self.buf[self.off..][..len];
        self.off += len;
        let lines = val.as_bytes().iter().filter(|&&x| x == b'\n').count();
        self.position.line += lines;
        if lines > 0 {
            let line_offset = val.rfind('\n').unwrap()+1;
            let num = val[line_offset..].chars().count();
            self.position.column = num + 1;
        } else {
            let num = val.chars().count();
            self.position.column += num;
        }
    }
    */
}


/*
 isWhiteSpace function in java

It is a Unicode space character () but is not also a non-breaking space ('\u00A0', '\u2007', '\u202F').
It is '\n', U+000A LINE FEED.
It is '\u000B', U+000B VERTICAL TABULATION.
It is '\f', U+000C FORM FEED.
It is '\u001C', U+001C FILE SEPARATOR.
It is '\u001D', U+001D GROUP SEPARATOR.
It is '\u001E', U+001E RECORD SEPARATOR.
It is '\u001F', U+001F UNIT SEPARATOR.


lightbend implementation uses following implementation


switch (codepoint) {
        // try to hit the most common ASCII ones first, then the nonbreaking
        // spaces that Java brokenly leaves out of isWhitespace.
        case ' ':
        case '\u00A0':
        case '\u2007':
        case '\u202F':
            // this one is the BOM, see
            // http://www.unicode.org/faq/utf_bom.html#BOM
            // we just accept it as a zero-width nonbreaking space.
        case '\uFEFF':
            return true;
        default:
            return Character.isWhitespace(codepoint);
        }
 */

//! Similar to Lightbend's implementation this function extends
//! Java's set of whitespace characters with Unicode BOM, which is accepted as
//! zero-width nonbreaking space.
//!
//! In summary a character is a whitespace if it is one of: SPACE_SEPARATOR, LINE_SEPARATOR,
//! PARAGRAPH_SEPARATOR, LINE FEED, VERTICAL TABULATION, FORM FEED, FILE SEPARATOR,
//! GROUP SEPARATOR, RECORD SEPARATOR, UNIT SEPARATOR, UNIT SEPARATOR or BOM.
fn is_whitespace(c: char) -> bool {
    match chr {
        ' ' | '\t' | '\r' | '\n' | '\u{2007}' | '\u{202f}' | '\u{feff}' | '\u{00b}' | '\u{000c}'
        | '\u{001c}' | '\u{001d}' | '\u{001e}' | '\u{001f}' => true,
        _ => false
    }
}

fn is_whitespace_not_newline(c: char) -> bool {
    chr != '\n' && is_whitespace(chr)
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

        assert_eq!(parse_tokens("("), vec![Token::LeftRoundBracket]);
        assert_eq!(parse_tokens(")"), vec![Token::RightRoundBracket]);

        assert_eq!(parse_tokens("."), vec![Token::Dot]);
        assert_eq!(parse_tokens(","), vec![Token::Comma]);
        assert_eq!(parse_tokens("="), vec![Token::Equals]);
    }

    #[test]
    fn it_must_recognize_ws() {
        assert_eq!(parse_tokens("  "), vec![Token::Whitespace("  ")]);
        assert_eq!(parse_tokens("  { \t"), vec![
            Token::Whitespace("  "),
            Token::LeftBrace,
            Token::Whitespace(" \t")
        ]);
    }

    #[test]
    fn it_must_recognize_newlines() {
        assert_eq!(parse_tokens("\n"), vec![Token::Newline]);
        assert_eq!(parse_tokens("\r\n"), vec![Token::Newline]);

        let mut stream = TokenStream::new("\r \n");
        assert!(stream.uncons().is_err());

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
    fn it_must_recognize_includes() {
        assert_eq!(parse_tokens("include"), vec![Token::Include]);
        assert_eq!(parse_tokens("include include"), vec![Token::Include, Token::Whitespace(" "), Token::Include]);
        // TODO:
    }

}
