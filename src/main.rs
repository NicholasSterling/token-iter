use token_iter::*;

// A type for the tokens.
#[derive(Clone, Debug, PartialEq)]
#[rustfmt::skip]
enum Token {
    LT, LE,                 // < <=
    GT, GE,                 // > >=
    EqEq, EQ,               // == =
    LCurly, RCurly,         // { }
    While, If,
    Identifier(String),     // e.g. foo12
    Int(u64),               // e.g. 273
    BadInt(String),         // e.g. 9873487239482398477132498723423987234
    Unrecognized(char)
}

// Produces a token, using a Lexer to examine the characters.
#[rustfmt::skip]
fn tokenizer(lx: &mut Lexer) -> Option<Token> {
    use Token::*;
    let is_digit = |c| char::is_ascii_digit(&c);
    Some(
        match lx.skip_while(char::is_whitespace).next()? {
            '<' => if lx.at('=') {LE} else {LT},
            '>' => if lx.at('=') {GE} else {GT},
            '=' => if lx.at('=') {EqEq} else {EQ},
            '{' => LCurly,
            '}' => RCurly,
            c if c.is_alphabetic() =>
                   match lx.take_while(char::is_alphanumeric).get() {
                       "while" => While,
                       "if" => If,
                       s => Identifier(s.into())
                   },
            c if is_digit(c) =>
                   lx.take_while(is_digit).map( |s|
                       if let Ok(n) = s.parse::<u64>() {
                           Int(n)
                       } else {
                           BadInt(s.into())
                       }
                   ),
            c => Unrecognized(c)
        }
    )
}

fn main() {
    let code = r#"
        if foo > bar {
            foo = 1
        }
    "#;
    for (line_num, col_range, token) in tokens_in(code.lines(), &tokenizer) {
        println!("On line {line_num} at columns {col_range:?}: {token:?}");
    }
}

// On line 1 at columns 8..10: If
// On line 1 at columns 11..14: Identifier("foo")
// On line 1 at columns 15..16: GT
// On line 1 at columns 17..20: Identifier("bar")
// On line 1 at columns 21..22: LCurly
// On line 2 at columns 12..15: Identifier("foo")
// On line 2 at columns 16..17: EQ
// On line 2 at columns 18..19: Int(1)
// On line 3 at columns 8..9: RCurly
