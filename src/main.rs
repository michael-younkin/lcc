use std::io::{self, Read};
use std::iter::Peekable;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TokenKind {
    Variable,
    FuncDecStart,
    FuncDecEnd,
    LParen,
    RParen,
}

#[derive(Debug)]
struct Token<'a> {
    value: Option<&'a str>,
    kind: TokenKind,
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind) -> Token<'a> {
        Token {
            kind: kind,
            value: None,
        }
    }

    fn with_value(kind: TokenKind, value: &'a str) -> Token<'a> {
        Token {
            value: Some(value),
            kind: kind,
        }
    }
}

// App -> Var App | e
// Var -> Func | variable
// Func -> Parens | λ variable . App
// Parens -> ( App )

#[derive(Debug, PartialEq, Eq)]
enum LambdaExp<'a> {
    App(Box<LambdaExp<'a>>, Box<LambdaExp<'a>>),
    Func(Box<LambdaExp<'a>>, Box<LambdaExp<'a>>),
    Var(&'a str),
    None
}

impl<'a> Display for LambdaExp<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &LambdaExp::App(ref left, ref right) => {
                if **right == LambdaExp::None {
                    write!(f, "{}", left)
                } else {
                    write!(f, "{} {}", left, right)
                }
            },
            &LambdaExp::Func(ref arg, ref body) => write!(f, "(λ{}.{})", arg, body),
            &LambdaExp::Var(s) => write!(f, "{}", s),
            &LambdaExp::None => write!(f, ""),
        }
    }
}

fn try_consume_value<'a, T>(tokens: &mut Peekable<T>, expected: TokenKind) -> Option<&'a str>
        where T: Iterator<Item=Token<'a>> {
    match tokens.peek() {
        Some(&Token {kind, value}) if kind == expected => {
            tokens.next();
            Some(value.expect("Token should have a value."))
        },
        _ => None
    }
}

fn try_consume<'a, T>(tokens: &mut Peekable<T>, expected: TokenKind) -> bool
        where T: Iterator<Item=Token<'a>> {
    match tokens.peek() {
        Some(&Token {kind, value: _}) if kind == expected => {
            tokens.next();
            true
        },
        _ => false
    }
}

fn parse_app<'a, T>(tokens: &mut Peekable<T>) -> LambdaExp<'a>
        where T: Iterator<Item=Token<'a>> {
    println!("parse_app: {:?}", tokens.peek());
    if let Some(&Token {kind, value: _}) = tokens.peek() {
        match kind {
            TokenKind::LParen | TokenKind::FuncDecStart | TokenKind::Variable => {
                let left = parse_var(tokens);
                let right = parse_app(tokens);
                return LambdaExp::App(Box::new(left), Box::new(right))
            },
            _ => ()
        }
    }
    LambdaExp::None
}

fn parse_var<'a, T>(tokens: &mut Peekable<T>) -> LambdaExp<'a>
        where T: Iterator<Item=Token<'a>> {
    println!("parse_var: {:?}", tokens.peek());
    if let Some(s) = try_consume_value(tokens, TokenKind::Variable) {
        LambdaExp::Var(s)
    } else {
        parse_func(tokens)
    }
}

fn parse_func<'a, T>(tokens: &mut Peekable<T>) -> LambdaExp<'a>
        where T: Iterator<Item=Token<'a>> {
    println!("parse_func: {:?}", tokens.peek());
    if !try_consume(tokens, TokenKind::FuncDecStart) {
        return parse_parens(tokens);
    }
    let variable = try_consume_value(tokens, TokenKind::Variable).expect("Expected variable token.");
    if !try_consume(tokens, TokenKind::FuncDecEnd) {
        panic!("Expected function argument list ending.");
    }
    let func_body = parse_app(tokens);
    LambdaExp::Func(Box::new(LambdaExp::Var(variable)), Box::new(func_body))
}

fn parse_parens<'a, T>(tokens: &mut Peekable<T>) -> LambdaExp<'a>
        where T: Iterator<Item=Token<'a>> {
    println!("parse_parens: {:?}", tokens.peek());
    if !try_consume(tokens, TokenKind::LParen) {
        panic!("Expected left parenthese.");
    }
    let out = parse_app(tokens);
    if !try_consume(tokens, TokenKind::RParen) {
        panic!("Expected right parenthese.");
    }
    out
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
    let lambda_exp = parse_app(&mut tokens.peekable());
    println!("{}", lambda_exp);
}
