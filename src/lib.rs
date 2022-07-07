//! This crate makes it easier to write tokenizers for textual languages.
//! A tokenizer takes a sequence of characters as input and produces a sequence
//! of "tokens" -- instances of some type that categorizes groups of characters.
//! For example, the text "foo < 10" might be tokenized into three tokens:
//! the identifier "foo", the less-than symbol, and the integer 10.
//!
//! This library was designed with the following principles in mind:
//!  1. The library should NOT define tokens, or place constraints on how they are represented.
//!     The library's token type should be an opaque generic parameter, with no type constraints.
//!  2. The library should NOT have any ideas about how the text is interpreted,
//!     beyond that a token may not span lines.  For example, the library should not
//!     assume that whitespace is insignificant, or have any notion of identifiers
//!     or numbers.
//!  3. The decision as to what constitutes a token should be handled in Rust code
//!     (rather than, for example, regular expressions).
//!  4. The API should give you an Iterator over the tokens in an &str,
//!     or in an Iterator of &str's.
//!  5. The library should automatically add the line number and column range for a token.
//!  6. The library should be very efficient.  In particular, it should not allocate
//!     heap memory for tokens, instead yielding &str's of substrings of the input.
//!     Where a String is required, creating it is left to you (by calling .into()),
//!     so that heap allocations are only done when really needed.
//!  7. Invalid tokens are tokens, not errors.  Tokenization shouldn't stop just because
//!     it doesn't recognize something.  And you want the same line/column info for bad tokens
//!     as you do for good ones.  So the tokenizer just produces tokens, not Results that
//!     contain either a token or an error.
//!  8. It should all work with multibyte characters.
//!
//! To use the library, you must
//!  1. define a type for your tokens (typically, but not necessarily, an enum)
//!  2. write a tokenizer function that uses a TokenBuilder (a type provided by this library)
//!     to recognize a token and, if necessary, get the token's characters as an &str
//!  3. call either\
//!     `tokens_in_line(line, &tokenizer)`, if you know your code is a single line, or\
//!     `tokens_in(line_iter, &tokenizer)`, to get line numbers with the tokens
//!
//! Here is a [sequence diagram](https://www.websequencediagrams.com/cgi-bin/cdraw?lz=dGl0bGUgVG9rZW5pemluZyBhIGxpbmUgd2l0aCB0aGUgdG9rZW4taXRlciBjcmF0ZSdzAA0Gc19pbl8AJgVmdW5jdGlvbgpwYXJ0aWNpcGFudCBZT1VSIFBST0dSQU0gYXMgUAARDQAzD2FzIFQAMQ1hbiBJdGVyYXRvciBvdmVyXG4oY29sdW1uIHJhbmdlLACBEAYpIGFzIEkAZRJUT0tFTklaRVIgRm5cbnRoYXQgcmVjb2duaXplc1xuIGFuZCBjcmVhdGUAgUQIIGFzIEYAdA5cbgCCEAVUb29sIGFzIE0KUCAtPisgVDoAgXAPKGxpbmUsICYAgiMFaXplcikKVCAtPi0gUDoAgR8oXG5mb3IgAIEjBQCCdwVvZiBjb2RlAGsHSTogbmV4dCgpCkkAfQVGOiBjYWxscyB5b3VyAIFdBQCDJwVhAIM_BlRvb2wKRgCBJgVNOiB2YXJpb3VzACsGXG50byBmaW5kIGEAg1AGCk0AgSQFRjogACgKYW5kIHBlcmhhcHNcbmdlAIMkByB0ZXh0ACUKdGV4dCBvZgA_B0YAgWoFSTogU29tZSgAgxQGAIElB006IHdoZXJlIHdhcwCBXAU_AG0HSTogAINFDABDCkkAgjQIAEgFAINhFSkAgg8Ka2VlcACBWAZpbmcAgiAHIHVudGlsIC4uLgCBfjNuAIFbDU5vbmUAgVUKAAkFAIEXCQAXBQo&s=earth)
//! showing how `tokens_in_line` works with your tokenizer function to generate a token.
//! `tokens_in` runs `tokens_in_line` on each line, flattening the results and adding line numbers.
//!
//! And here is a fairly beefy example:
//! ```rust
//! use token_iter::*;
//!
//! // A type for the tokens.
//! #[derive(Clone, Debug, PartialEq)]
//! enum Token {
//!     LT, LE,                 // < <=
//!     GT, GE,                 // > >=
//!     EQEQ, EQ,               // == =
//!     LCurly, RCurly,         // { }
//!     While, If,
//!     Ident(String),          // e.g. foo12
//!     Int(u64),               // e.g. 273
//!     BadInt(String),         // e.g. 9873487239482398477132498723423987234
//!     Unrecognized(String)
//! }
//!
//! // Produces a token, using a TokenTool to examine the characters.
//! fn tokenizer(tt: &mut TokenTool) -> Option<Token> {
//!     use Token::*;
//!     let is_digit = |c: char| c >= '0' && c <= '9';
//!     Some(
//!         match tt.skip_while(char::is_whitespace).next()? {
//!             '<' => if tt.next_is('=') {LE} else {LT},
//!             '>' => if tt.next_is('=') {GE} else {GT},
//!             '=' => if tt.next_is('=') {EQEQ} else {EQ},
//!             '{' => LCurly,
//!             '}' => RCurly,
//!             c if c.is_alphabetic() =>
//!                    match tt.take_while(char::is_alphanumeric).get() {
//!                        "while" => While,
//!                        "if" => If,
//!                        text => Ident(text.into())
//!                    },
//!             c if is_digit(c) =>
//!                    tt.take_while(is_digit).map( |it|
//!                        if let Ok(n) = it.parse::<u64>() {
//!                            Int(n)
//!                        } else {
//!                            BadInt(it.into())
//!                        }
//!                    ),
//!             _ => Unrecognized(tt.into())
//!         }
//!     )
//! }
//!
//! fn main() {
//!     let code = r#"
//!         if foo > bar {
//!             foo = 1
//!         }
//!     "#;
//!     for (line_num, col_range, token) in tokens_in(code.lines(), &tokenizer) {
//!        println!("On line {line_num} at columns {col_range:?}: {token:?}");
//!     }
//! }
//!
//! // On line 1 at columns 8..10: If
//! // On line 1 at columns 11..14: Ident("foo")
//! // On line 1 at columns 15..16: GT
//! // On line 1 at columns 17..20: Ident("bar")
//! // On line 1 at columns 21..22: LCurly
//! // On line 2 at columns 12..15: Ident("foo")
//! // On line 2 at columns 16..17: EQ
//! // On line 2 at columns 18..19: Int(1)
//! // On line 3 at columns 8..9: RCurly
//! ```

use std::ops::Range;
use std::str::CharIndices;

/// Returns an Iterator over the tokens found in the specified line,
/// along with their column ranges.  Column numbers start at 0.
pub fn tokens_in_line<'a, Str, Token: 'a, Tokenizer>(line: Str, tokenizer: &'a Tokenizer)
                                                     -> impl Iterator<Item = (Range<usize>, Token)> + 'a
where
    Str: Into<&'a str>,
    Tokenizer: Fn(&mut TokenTool<'a>) -> Option<Token> + 'a
{
    let line = line.into();
    let mut chars = line.char_indices();
    let next = chars.next();
    StrTokenIterator::<'a, Token, Tokenizer> {
        tool: TokenTool::<'a> {
            line,
            chars,
            current: next,
            column: 0,
            start_ix: 0,
            start_column: 0
        },
        tokenizer
    }
}

/// Returns an Iterator over the tokens found in the specified lines,
/// along with their line numbers and column ranges (both of which start at 0).
pub fn tokens_in<'a, Token: 'a, Tokenizer, StrIter, Str>(iter: StrIter, tokenizer: &'a Tokenizer)
                                                                     -> impl Iterator<Item = (usize, Range<usize>, Token)> + 'a
where
    StrIter: Iterator<Item = Str> + 'a,
    Str: Into<&'a str> + 'a,
    Tokenizer: Fn(&mut TokenTool<'a>) -> Option<Token> + 'a
{
    iter.enumerate().flat_map( |(line_num, line)|
        tokens_in_line(line, tokenizer).map( move |(column_range, token)|
            (line_num, column_range, token)
        )
    )
}


///// StrTokenIterator /////////////////////////

struct StrTokenIterator<'a, Token, Tokenizer>
where Tokenizer: Fn(&mut TokenTool<'a>) -> Option<Token>
{
    tool: TokenTool<'a>,
    tokenizer: &'a Tokenizer
}

impl<'a, Token, Tokenizer> Iterator for StrTokenIterator<'a, Token, Tokenizer>
where Tokenizer: Fn(&mut TokenTool<'a>) -> Option<Token>
{
    type Item = (Range<usize>, Token);

    fn next(&mut self) -> Option<Self::Item> {
        self.tool.current.and_then( |_|
            (self.tokenizer)(&mut self.tool.mark_start()).map( |token|
                (self.tool.column_range(), token)
            )
        )
    }
}


///// TokenTool /////////////////////////

/// The tokenizer you write will be passed a TokenTool as an argument,
/// and will call methods on it to figure out what the next token is
/// and, if needed, get its text.  The TokenTool keeps track of the
/// column range of the token.
pub struct TokenTool<'a> {
    line: &'a str,
    chars: CharIndices<'a>,
    current: Option<(usize, char)>,
    column: usize,
    start_ix: usize,
    start_column: usize,
}

impl<'a> TokenTool<'a> {

    /// Adds the current character to the token and advances to the next.
    fn advance(&mut self) {
        if self.current.is_some() {
            self.current = self.chars.next();
            self.column += 1;
        }
    }

    /// Gets the range of columns currently associated with the token.
    fn column_range(&mut self) -> Range<usize> {
        self.start_column .. self.column
    }

    /// Gets the range of the line currently associated with the token.
    fn str_range(&mut self) -> Range<usize> {
        self.start_ix .. self.current.map_or_else(|| self.line.len(), |(ix, _)| ix)
    }

    /// Makes the next char of the text the first char of the token.
    fn mark_start(&mut self) -> &mut Self {
        if let Some((start_ix, _)) = self.current {
            self.start_ix = start_ix;
            self.start_column = self.column;
        }
        self
    }

    /// Returns the next character and adds it to the token.
    pub fn next(&mut self) -> Option<char> {
        self.current.map( |(_, c)| {
            self.advance();
            c
        })
    }

    /// If the next character matches c, adds the character to the token
    /// and returns true.  Otherwise returns false.
    pub fn next_is(&mut self, c: char) -> bool {
        if let Some((_, next_c)) = self.current {
            if next_c == c {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Keeps adding characters to the token while f(char) is true.
    /// Returns self to allow chaining.
    pub fn take_while(&mut self, f: impl Fn(char) -> bool) -> &mut Self {
        while let Some((_, c)) = self.current {
            if !f(c) { break; }
            self.advance();
        }
        self
    }

    /// Skips over characters while f(char) is true;
    /// the token will start at the char where f(char) is false.
    /// Returns self to allow chaining.
    pub fn skip_while(&mut self, f: impl Fn(char) -> bool) -> &mut Self {
        self.take_while(f);
        self.mark_start();
        self
    }

    /// Returns the text of the token as an &str.
    pub fn get(&mut self) -> &'a str {
        &self.line[self.str_range()]
    }

    /// Returns the text of the token, converted to the needed type.
    /// This is just a shortcut for .get().into().
    pub fn into<T: From<&'a str>>(&mut self) -> T {
        T::from(self.get())
    }

    /// Returns the result of applying f() to the token text.
    pub fn map<T>(&mut self, f: impl Fn(&'a str) -> T) -> T {
        f(self.get())
    }

}


///// TESTS /////////////////////////////////////////////////////////////////////////////////////


#[cfg(test)]
mod test {

    use super::TokenTool;

    #[derive(Clone, Debug, PartialEq)]
    pub enum Token {
        LT, LE,                 // < <=
        GT, GE,                 // > >=
        EQ, EQEQ,               // = ==
        LParen, RParen,         // ( )
        LCurly, RCurly,         // { }
        If,                     // if
        While,                  // while
        Ident(String),          // e.g. foo
        Int(u64),               // e.g. 273
        BadInt(String),         // e.g. 9873487239482398472498723423987234
        Unrecognized(String),   // e.g. @
    }
    use Token::*;

    fn tokenizer(tt: &mut TokenTool) -> Option<Token> {
        let is_digit = |c: char| c >= '0' && c <= '9';
        Some(
            match tt.skip_while(char::is_whitespace).next()? {
                '<' =>  if tt.next_is('=') {LE} else {LT},
                '>' =>  if tt.next_is('=') {GE} else {GT},
                '=' =>  if tt.next_is('=') {EQEQ} else {EQ},
                '(' =>  LParen,
                ')' =>  RParen,
                '{' =>  LCurly,
                '}' =>  RCurly,
                c if c.is_alphabetic() =>
                        match tt.take_while(char::is_alphanumeric).get() {
                            "if" => If,
                            "while" => While,
                            text => Ident(text.into()),
                        },
                c if is_digit(c) =>
                        tt.take_while(is_digit).map( |it|
                            if let Ok(n) = it.parse::<u64>() {
                                Int(n)
                            } else {
                                BadInt(it.into())
                            }
                        ),
                _ => Unrecognized(tt.into())
            }
        )
    }

    #[test]
    fn test_empty_str() {
        let next = super::tokens_in_line("", &tokenizer).next();
        assert_eq!(next, None);
    }

    #[test]
    fn test_lots_of_whitespace() {
        let line = "  if ( foo <= 10 ) \n  { x = 2 }  ";
        let expected_tokens = vec![
            If, LParen, Ident("foo".into()), LE, Int(10), RParen,
            LCurly, Ident("x".into()), EQ, Int(2), RCurly
        ];
        let results: Vec<_> = super::tokens_in_line(line, &tokenizer).collect();
        let tokens: Vec<_> = results.iter().map( |(_, token)| token.clone() ).collect();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_multiline() {
        let code = "if (foo<=10) {x=2}";
        let expected_tokens = vec![
            If, LParen, Ident("foo".into()), LE, Int(10), RParen,
            LCurly, Ident("x".into()), EQ, Int(2), RCurly
        ];
        let results: Vec<_> = super::tokens_in(code.lines(), &tokenizer).collect();
        assert!(results.iter().all( |(line, _, _)| *line == 0));
        let tokens: Vec<_> = results.iter().map( |(_, _, token)| (*token).clone() ).collect();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_line_and_column() {
        let code = "{\n    foo = 2\n}";
        let expected_results = vec![
            (0, 0..1, LCurly),
            (1, 4..7, Ident("foo".into())),
            (1, 8..9, EQ),
            (1, 10..11, Int(2)),
            (2, 0..1, RCurly)
        ];
        let results: Vec<_> = super::tokens_in(code.lines(), &tokenizer).collect();
        assert_eq!(results, expected_results);
    }

    #[test]
    fn test_multibyte() {
        let line = "关于本网站的";
        // Tokenizer calls     ^    LT, and anything else an Ident.
        // So this should tokenize to Ident < Ident.
        fn tokenizer(tt: &mut TokenTool) -> Option<Token> {
            Some(
                match tt.next()? {
                    '本' => LT,
                    _ => Ident(tt.take_while( |c| c != '本' ).into())
                }
            )
        }
        let results: Vec<_> = super::tokens_in_line(line, &tokenizer).collect();
        let expected_results = [
            (0..2, Ident("关于".into())),
            (2..3, LT),
            (3..6, Ident("网站的".into())),
        ];
        assert_eq!(results, expected_results);
    }

}
