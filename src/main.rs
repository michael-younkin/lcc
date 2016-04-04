use std::io::{self, Read};

#[derive(Clone, Copy, Debug)]
enum TokenKind {
    Variable,
    FuncDecStart,
    FuncDecEnd,
    LParen,
    RParen,
    InputStart,
    InputEnd,
}

#[derive(Debug)]
struct Token<'a> {
    value: Option<&'a str>,
    kind: TokenKind,
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind) -> Token<'a> {
        Token {
            value: None,
            kind: kind,
        }
    }

    fn with_value(kind: TokenKind, value: &'a str) -> Token<'a> {
        Token {
            value: Some(value),
            kind: kind,
        }
    }
}

fn main() {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf).unwrap();

    let tokens = buf.split("").filter_map(|c| {
        match c {
            // split("") produces a "" at the start and end of the iterator
            "" | " " | "\n" | "\t" => None,
            "/" => Some(Token::new(TokenKind::FuncDecStart)),
            "." => Some(Token::new(TokenKind::FuncDecEnd)),
            "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o"
                | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
                => Some(Token::with_value(TokenKind::Variable, c)),
            "(" => Some(Token::new(TokenKind::LParen)),
            ")" => Some(Token::new(TokenKind::RParen)),
            _ => panic!("Unexpected token!"),
        }
    });
    let start_token = vec![Token::new(TokenKind::InputStart)];
    let end_token = vec![Token::new(TokenKind::InputEnd)];
    let tokens: Vec<_> = start_token.into_iter().chain(tokens).chain(end_token).collect();
    println!("{:?}", tokens);
}
