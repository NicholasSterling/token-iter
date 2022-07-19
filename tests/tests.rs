use token_iter::*;

#[derive(Clone, Debug, PartialEq)]
#[rustfmt::skip]
pub enum Token {
    LT, LE,                 // < <=
    GT, GE,                 // > >=
    EQ, EqEq,               // = ==
    LParen, RParen,         // ( )
    LCurly, RCurly,         // { }
    If,                     // if
    While,                  // while
    Ident(String),          // e.g. foo
    Int(usize),             // e.g. 273
    BadInt(String),         // e.g. 9873487239482398472498723423987234
    Unrecognized(String),   // e.g. @
}
use Token::*;

#[rustfmt::skip]
fn tokenizer(l: &mut Lexer) -> Option<Token> {
    let is_digit = |c| char::is_ascii_digit(&c);
    Some(
        match l.skip_while(char::is_whitespace).next()? {
            '<' =>  if l.at('=') {LE} else {LT},
            '>' =>  if l.at('=') {GE} else {GT},
            '=' =>  if l.at('=') {EqEq} else {EQ},
            '(' =>  LParen,
            ')' =>  RParen,
            '{' =>  LCurly,
            '}' =>  RCurly,
            c if c.is_alphabetic() =>
                    match l.take_while(char::is_alphanumeric).get() {
                        "if" => If,
                        "while" => While,
                        s => Ident(s.into()),
                    },
            c if is_digit(c) =>
                    l.take_while(is_digit).map( |it|
                        if let Ok(n) = it.parse::<usize>() {
                            Int(n)
                        } else {
                            BadInt(it.into())
                        }
                    ),
            _ => Unrecognized(l.into())
        }
    )
}

#[test]
fn test_empty_str() {
    let next = tokens_in_line("", &tokenizer).next();
    assert_eq!(next, None);
}

#[test]
fn test_no_whitespace() {
    let line = "if(foo<=10){x=2}";
    #[rustfmt::skip]
    let expected_tokens = vec![
        If, LParen, Ident("foo".into()), LE, Int(10), RParen,
        LCurly, Ident("x".into()), EQ, Int(2), RCurly
    ];
    let results: Vec<_> = tokens_in_line(line, &tokenizer).collect();
    let tokens: Vec<_> = results.iter().map(|(_, token)| token.clone()).collect();
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_lots_of_whitespace() {
    let line = "  if ( foo <= 10 ) \n  { x = 2 }  ";
    #[rustfmt::skip]
    let expected_tokens = vec![
        If, LParen, Ident("foo".into()), LE, Int(10), RParen,
        LCurly, Ident("x".into()), EQ, Int(2), RCurly
    ];
    let results: Vec<_> = tokens_in_line(line, &tokenizer).collect();
    let tokens: Vec<_> = results.iter().map(|(_, token)| token.clone()).collect();
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_multiline() {
    let code = "\n\nif (foo<=10) {x=2}";
    #[rustfmt::skip]
    let expected_tokens = vec![
        If, LParen, Ident("foo".into()), LE, Int(10), RParen,
        LCurly, Ident("x".into()), EQ, Int(2), RCurly
    ];
    let results: Vec<_> = tokens_in(code.lines(), &tokenizer).collect();
    assert!(results.iter().all(|(line, _, _)| *line == 2));
    let tokens: Vec<_> = results
        .iter()
        .map(|(_, _, token)| (*token).clone())
        .collect();
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
        (2, 0..1, RCurly),
    ];
    let results: Vec<_> = tokens_in(code.lines(), &tokenizer).collect();
    assert_eq!(results, expected_results);
}

#[test]
fn test_multibyte_chars() {
    let line = "关于本网站的";
    // Tokenizer calls     ^    LT, and anything else an Ident.
    // So this should tokenize to Ident < Ident.
    fn tokenizer(lx: &mut Lexer) -> Option<Token> {
        Some(match lx.next()? {
            '本' => LT,
            _ => Ident(lx.take_while(|c| c != '本').into()),
        })
    }
    let results: Vec<_> = tokens_in_line(line, &tokenizer).collect();
    let expected_results = [
        (0..2, Ident("关于".into())),
        (2..3, LT),
        (3..6, Ident("网站的".into())),
    ];
    assert_eq!(results, expected_results);
    assert_eq!(line.len(), 18);
}
