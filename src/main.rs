use std::io::{self, Read};
use std::iter::Peekable;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

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

// App -> App Var | e
// Var -> Func | variable
// Func -> Parens | λ variable . App
// Parens -> ( App )

#[derive(Clone, Debug, PartialEq, Eq)]
enum LambdaExp<'a> {
    App(Rc<LambdaExp<'a>>, Rc<LambdaExp<'a>>),
    Func(Rc<LambdaExp<'a>>, Rc<LambdaExp<'a>>),
    Var(&'a str),
    None,
}

impl<'a> Display for LambdaExp<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LambdaExp::App(ref left, ref right) =>
                // ** needed until we finally get something like the "box" keyword to use in
                // pattern matching.
                match (&**left, &**right) {
                    // When functions are forced to be left associative
                    (&LambdaExp::Func(_, _), &LambdaExp::Func(_, _)) =>
                        write!(f, "({}) {}", left, right),
                    // When application is forced to be right associative
                    (_, &LambdaExp::App(_, _)) => write!(f, "{} ({})", left, right),
                    _ => write!(f, "{} {}", left, right),
                },
            LambdaExp::Func(ref arg, ref body) => write!(f, "λ{}.{}", arg, body),
            LambdaExp::Var(s) => write!(f, "{}", s),
            LambdaExp::None => write!(f, ""),
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
    let mut current_value = parse_var(tokens);
    let mut next_value = parse_var(tokens);
    while next_value != LambdaExp::None {
        current_value = LambdaExp::App(Rc::new(current_value), Rc::new(next_value));
        next_value = parse_var(tokens);
    }
    current_value
}

fn parse_var<'a, T>(tokens: &mut Peekable<T>) -> LambdaExp<'a>
        where T: Iterator<Item=Token<'a>> {
    // Moved here from parse_app because parse_app is weird in order to handle left associativity
    // properly.
    if let Some(s) = try_consume_value(tokens, TokenKind::Variable) {
        LambdaExp::Var(s)
    } else {
        parse_func(tokens)
    }
}

fn parse_func<'a, T>(tokens: &mut Peekable<T>) -> LambdaExp<'a>
        where T: Iterator<Item=Token<'a>> {
    if !try_consume(tokens, TokenKind::FuncDecStart) {
        return parse_parens(tokens);
    }
    let variable = try_consume_value(tokens, TokenKind::Variable).expect("Expected variable token.");
    if !try_consume(tokens, TokenKind::FuncDecEnd) {
        panic!("Expected function argument list ending.");
    }
    let func_body = parse_app(tokens);
    LambdaExp::Func(Rc::new(LambdaExp::Var(variable)), Rc::new(func_body))
}

fn parse_parens<'a, T>(tokens: &mut Peekable<T>) -> LambdaExp<'a>
        where T: Iterator<Item=Token<'a>> {
    if !try_consume(tokens, TokenKind::LParen) {
        return LambdaExp::None;
    }
    let out = parse_app(tokens);
    if out == LambdaExp::None {
        panic!("No empty parentheses allowed.");
    }
    if !try_consume(tokens, TokenKind::RParen) {
        panic!("Expected right parenthese.");
    }
    out
}

fn substitute<'a>(root_exp: Rc<LambdaExp<'a>>, var: Rc<LambdaExp<'a>>, val: Rc<LambdaExp<'a>>) ->
        Rc<LambdaExp<'a>> {
    match *root_exp {
        LambdaExp::App(ref left, ref right) => Rc::new(LambdaExp::App(
                substitute(left.to_owned(), var.to_owned(), val.to_owned()),
                substitute(right.to_owned(), var.to_owned(), val.to_owned())
            )),
        LambdaExp::Var(_) if *root_exp == *var => val.to_owned(),
        LambdaExp::Func(ref arg, ref body) if *arg != var =>
            Rc::new(LambdaExp::Func(
                    arg.to_owned(),
                    substitute(body.to_owned(), var.to_owned(), val.to_owned()))),
        _ => root_exp.to_owned(),
    }
}

fn simplify_once<'a>(root_exp: Rc<LambdaExp<'a>>) -> Rc<LambdaExp<'a>> {
    match *root_exp {
        LambdaExp::App(ref left, ref right) => {
            // If we have a func, apply it
            if let LambdaExp::Func(ref arg, ref body) = **left {
                substitute(body.to_owned(), arg.to_owned(), right.to_owned())
            } else {
                // Try to simplify the right
                let simple_right = simplify_once(right.to_owned());
                if simple_right != *right {
                    Rc::new(LambdaExp::App(left.to_owned(), simple_right))
                } else {
                    // Try to simplify the left
                    let simple_left = simplify_once(left.to_owned());
                    if simple_left != *left {
                        Rc::new(LambdaExp::App(simple_left, right.to_owned()))
                    } else {
                        // Can't simplify either side
                        root_exp.to_owned()
                    }
                }
            }
        },
        LambdaExp::Func(ref arg, ref body) => {
            let simple_body = simplify_once(body.to_owned());
            if simple_body != *body {
                Rc::new(LambdaExp::Func(arg.to_owned(), simple_body))
            } else {
                root_exp.to_owned()
            }
        },
        _ => root_exp.to_owned(),
    }
}

fn simplify<'a>(root_exp: Rc<LambdaExp<'a>>) -> Vec<Rc<LambdaExp<'a>>> {
    let mut steps = vec![root_exp.clone()];
    let mut prev_step = root_exp.clone();
    let mut next_step = simplify_once(root_exp);
    while next_step != prev_step {
        prev_step = next_step;
        steps.push(prev_step.clone());
        next_step = simplify_once(prev_step.clone());
    }
    steps
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
    let lambda_exp = Rc::new(parse_app(&mut tokens.peekable()));
    println!("Parsed expression: {}", lambda_exp);
    println!("Simplified expression: {}", simplify(lambda_exp).last().expect("There should be at least one step in simplification."));
}
