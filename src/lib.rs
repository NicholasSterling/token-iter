//! This crate makes it easier to write tokenizers for textual languages.
//! A tokenizer takes a sequence of characters as input and produces a sequence
//! of "tokens" -- instances of some type that categorizes groups of characters.
//! For example, the text "foo < 10" might be tokenized into three tokens:
//! the identifier "foo", the less-than symbol, and the integer 10.
//!
//! Note that a sequence of characters that is considered a unit is called
//! a "lexeme," and the lexeme gets converted to a token (typically an enum value).
//! For example, the lexeme "while" might be mapped to a Token::While enum value,
//! and "731" to Token::Int(731).
//!
//! This library was designed with the following principles in mind:
//!  1. The library should NOT define tokens, or place constraints on how they are represented.
//!     The library's token type should be an opaque generic parameter, with no type constraints.
//!  2. The library should NOT have any ideas about how the text is interpreted,
//!     beyond that a lexeme may not span lines.  For example, the library should not
//!     assume that whitespace is insignificant, or have any notion of identifiers
//!     or numbers.
//!  3. The decision as to what constitutes a lexeme should be handled in Rust code
//!     (rather than, for example, regular expressions).
//!  4. The API should give you an Iterator over the tokens in an &str,
//!     or in an Iterator of &str's.
//!  5. The library should automatically add the line number and column range for a token.
//!  6. The library should be very efficient.  In particular, it should not allocate heap
//!     memory for lexemes or tokens, instead yielding &str's of substrings of the input.
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
//!  2. write a tokenizer function that uses a Lexer (a type provided by this library)
//!     to recognize lexemes and convert them to tokens
//!  3. call either\
//!     `tokens_in_line(line, &tokenizer)`, if you know your code is a single line, or\
//!     `tokens_in(line_iter, &tokenizer)`, to get line numbers with the tokens
//!
//! Here is a [sequence diagram](https://www.websequencediagrams.com/cgi-bin/cdraw?lz=dGl0bGUgVG9rZW5pemluZyBhIGxpbmUgd2l0aCB0aGUgdG9rZW4taXRlciBjcmF0ZSdzAA0Gc19pbl8AJgVmdW5jdGlvbgpwYXJ0aWNpcGFudCBZT1VSIFBST0dSQU0gYXMgUAARDQAzD2FzIFQAMQ1hbiBJdGVyYXRvciBvdmVyXG4oY29sdW1uIHJhbmdlLACBEAYpIGFzIEkAZRJUT0tFTklaRVIgRm5cbnRoYXQgcmVjb2duaXplcyBsZXhlbWVzXG4gYW5kIGNyZWF0ZQCBTAggYXMgRgB8DlxuTGV4ZXIgYXMgTQpQIC0-KyBUOgCBdA8obGluZSwgJgCCJwVpemVyKQpUIC0-LSBQOgCBIyhcbmZvciAAgScFAIJ7BW9mIGNvZGUAawdJOiBuZXh0KCkKSQB9BUY6IGNhbGxzIHlvdXIAgWEFAIMrBWEgAIEnBQpGAIEiBU06IHZhcmlvdXMAJwZcbnRvIGZpbmQgYQCBfgcKTQCBIQVGOiAAKQphbmQgcGVyaGFwc1xuZ2V0IGl0cyB0ZXh0ACMKADYHRgCBXgVJOiBTb21lKACDDAYAgRkHTTogd2hlcmUgd2FzAIFQBT8AZAdJOiAAgz0MIG9mAIEDCEkAgikIAEkFAINaFSkAggQKa2VlcACBUQZpbmcAghUHIHVudGlsIC4uLgCBdy9uAIFRDU5vbgCBUQsACQUAgRMJABcFCg&s=earth)
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
//!     EqEq, EQ,               // == =
//!     LCurly, RCurly,         // { }
//!     While, If,
//!     Ident(String),          // e.g. foo12
//!     Int(u64),               // e.g. 273
//!     BadInt(String),         // e.g. 9873487239482398477132498723423987234
//!     Unrecognized(char)
//! }
//!
//! // Produces a token, using a Lexer to examine the characters.
//! fn tokenizer(lx: &mut Lexer) -> Option<Token> {
//!     use Token::*;
//!     let is_digit = |c| char::is_ascii_digit(&c);
//!     Some(
//!         match lx.skip_while(char::is_whitespace).next()? {
//!             '<' => if lx.at('=') {LE} else {LT},
//!             '>' => if lx.at('=') {GE} else {GT},
//!             '=' => if lx.at('=') {EqEq} else {EQ},
//!             '{' => LCurly,
//!             '}' => RCurly,
//!             c if c.is_alphabetic() =>
//!                    match lx.take_while(char::is_alphanumeric).get() {
//!                        "while" => While,
//!                        "if" => If,
//!                        s => Ident(s.into())
//!                    },
//!             c if is_digit(c) =>
//!                    lx.take_while(is_digit).map( |s|
//!                        if let Ok(n) = s.parse::<u64>() {
//!                            Int(n)
//!                        } else {
//!                            BadInt(s.into())
//!                        }
//!                    ),
//!             c => Unrecognized(c)
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
pub fn tokens_in_line<'a, Str, Token: 'a, Tokenizer>(
    line: Str,
    tokenizer: &'a Tokenizer,
) -> impl Iterator<Item = (Range<usize>, Token)> + 'a
where
    Str: Into<&'a str>,
    Tokenizer: Fn(&mut Lexer<'a>) -> Option<Token>,
{
    let line = line.into();
    let mut chars = line.char_indices();
    let next = chars.next();
    StrTokenIterator::<'a, Token, Tokenizer> {
        lexer: Lexer::<'a> {
            line,
            chars,
            current: next,
            column: 0,
            start_ix: 0,
            start_column: 0,
        },
        tokenizer,
    }
}

/// Returns an Iterator over the tokens found in the specified lines,
/// along with their line numbers and column ranges (both of which start at 0).
pub fn tokens_in<'a, Token: 'a, Tokenizer, StrIter, Str>(
    iter: StrIter,
    tokenizer: &'a Tokenizer,
) -> impl Iterator<Item = (usize, Range<usize>, Token)> + 'a
where
    StrIter: Iterator<Item = Str> + 'a,
    Str: Into<&'a str> + 'a,
    Tokenizer: Fn(&mut Lexer<'a>) -> Option<Token>,
{
    iter.enumerate().flat_map(|(line_num, line)| {
        tokens_in_line(line, tokenizer)
            .map(move |(column_range, token)| (line_num, column_range, token))
    })
}

///// StrTokenIterator /////////////////////////

struct StrTokenIterator<'a, Token, Tokenizer>
where
    Tokenizer: Fn(&mut Lexer<'a>) -> Option<Token>,
{
    lexer: Lexer<'a>,
    tokenizer: &'a Tokenizer,
}

impl<'a, Token, Tokenizer> Iterator for StrTokenIterator<'a, Token, Tokenizer>
where
    Tokenizer: Fn(&mut Lexer<'a>) -> Option<Token>,
{
    type Item = (Range<usize>, Token);

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.current.and_then(|_| {
            (self.tokenizer)(self.lexer.mark_start())
                .map(|token| (self.lexer.column_range(), token))
        })
    }
}

///// Lexer /////////////////////////

/// The tokenizer you write will be passed a Lexer as an argument,
/// and will call methods on it to figure out what the next lexeme is
/// and, if needed, get its text.  The Lexer keeps track of the
/// column range of the lexeme.
pub struct Lexer<'a> {
    line: &'a str,
    chars: CharIndices<'a>,
    current: Option<(usize, char)>,
    column: usize,
    start_ix: usize,
    start_column: usize,
}

impl<'a> Lexer<'a> {
    /// Adds the current character to the lexeme and advances to the next.
    #[inline]
    fn advance(&mut self) {
        if self.current.is_some() {
            self.current = self.chars.next();
            self.column += 1;
        }
    }

    /// Gets the range of columns currently associated with the lexeme.
    fn column_range(&self) -> Range<usize> {
        self.start_column..self.column
    }

    /// Gets the byte range of the line currently associated with the lexeme.
    fn str_range(&self) -> Range<usize> {
        self.start_ix..self.current.map_or_else(|| self.line.len(), |(ix, _)| ix)
    }

    /// Makes the next char the start of the lexeme.
    fn mark_start(&mut self) -> &mut Self {
        if let Some((start_ix, _)) = self.current {
            self.start_ix = start_ix;
            self.start_column = self.column;
        }
        self
    }

    /// Returns the next character and adds it to the lexeme.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<char> {
        self.current.map(|(_, c)| {
            self.advance();
            c
        })
    }

    /// Returns the next character without advancing.
    pub fn peek(&self) -> Option<char> {
        self.current.map(|(_, c)| c)
    }

    /// If the next character is c (the argument, not the letter),
    /// adds c to the lexeme and returns true.  Otherwise just returns false.
    pub fn at(&mut self, c: char) -> bool {
        if let Some((_, next_c)) = self.current {
            if next_c == c {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Keeps adding characters to the lexeme while f(char) is true.
    /// Returns self to allow chaining.
    pub fn take_while(&mut self, f: impl Fn(char) -> bool) -> &mut Self {
        while let Some((_, c)) = self.current {
            if !f(c) {
                break;
            }
            self.advance();
        }
        self
    }

    /// Skips over characters while f(char) is true;
    /// the lexeme will start at the char where f(char) is false.
    /// Returns self to allow chaining.
    pub fn skip_while(&mut self, f: impl Fn(char) -> bool) -> &mut Self {
        self.take_while(f);
        self.mark_start();
        self
    }

    /// Returns the lexeme as an &str.
    pub fn get(&self) -> &'a str {
        &self.line[self.str_range()]
    }

    /// Returns the lexeme converted to the needed type (commonly a String).
    /// This is just a shortcut for .lexeme().into().
    pub fn into<T: From<&'a str>>(&mut self) -> T {
        T::from(self.get())
    }

    /// Returns the result of applying f() to the lexeme.
    pub fn map<T>(&mut self, f: impl Fn(&'a str) -> T) -> T {
        f(self.get())
    }
} // impl Lexer
