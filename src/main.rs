use std::io::{self, Read};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::mem;

#[derive(Debug)]
enum Token<'t> {
    Var(&'t str),
    FuncStart,
    FuncDecEnd,
    LParen,
    RParen,
}

fn is_var(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' | '0'...'9' => true,
        _ => false
    }
}

fn tokenize<'t>(input: &'t str) -> Result<Vec<Token<'t>>, &'static str> {
    let mut chars = input.char_indices().peekable();
    let mut tokens = Vec::new();
    while let Some((i, c)) = chars.next() {
        match c {
            ' ' | '\t' | '\n' => (),
            '\\' | '/' | 'λ' => tokens.push(Token::FuncStart),
            '.' => tokens.push(Token::FuncDecEnd),
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            _ if is_var(c) => {
                let start = i;
                let mut end = i;
                while let Some(&(i, c)) = chars.peek() {
                    if is_var(c) {
                        end = i;
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Var(&input[i..(i + 1)]))
            },
            _ => return Err("Unexpected character encountered."),
        }
    }
    Ok(tokens)
}

macro_rules! consume {
    ($iter:expr, $pat:pat) => {
        if let Some($pat) = $iter.next() {
            Ok(())
        } else {
            Err(concat!("Expected ", stringify!($pat), "."))
        }
    }
}

macro_rules! consume_value {
    ($iter:expr, $pat:pat, $m:ident) => {
        if let Some($pat) = $iter.next() {
            Ok($m)
        } else {
            Err(concat!("Expected ", stringify!($pat), "."))
        }
    }
}

#[derive(Debug)]
enum Expr<'t> {
    Var(&'t str),
    Func(&'t str, Vec<Expr<'t>>),
    Scope(Vec<Expr<'t>>),
}

impl<'t> Expr<'t> {
    fn with_input(input: &'t str) -> Result<Expr<'t>, &'static str> {
        fn match_func<'t>(func: (&'t str, Vec<Expr<'t>>)) -> Expr<'t> {
            match func {
                ("", body) => Expr::Scope(body),
                (arg, body) => Expr::Func(arg, body),
            }
        }
        let mut tokens = try!(tokenize(input)).into_iter();
        let mut scope_stack = Vec::new();
        let mut current_func = ("", Vec::new());
        while let Some(token) = tokens.next() {
            match token {
                Token::Var(s) => current_func.1.push(Expr::Var(s)),
                Token::FuncStart => {
                    let arg_name = try!(consume_value!(tokens, Token::Var(s), s));
                    try!(consume!(tokens, Token::FuncDecEnd));
                    let mut temp = (arg_name, Vec::new());
                    mem::swap(&mut current_func, &mut temp);
                    scope_stack.push(temp);
                },
                Token::LParen => {
                    let mut temp = ("", Vec::new());
                    mem::swap(&mut current_func, &mut temp);
                    scope_stack.push(temp);
                },
                Token::RParen => {
                    let expr = match_func(current_func);
                    if let Some((arg, mut body)) = scope_stack.pop() {
                        body.push(expr);
                        current_func = (arg, body);
                    } else {
                        return Err("Extra right parenthese encountered.");
                    }
                },
                _ => return Err("Unexpected function declaration ending.")
            }
        }
        while scope_stack.len() > 0 {
            let expr = match_func(current_func);
            if let Some((arg, mut body)) = scope_stack.pop() {
                body.push(expr);
                current_func = (arg, body);
            } else {
                return Err("Extra right parenthese encountered.");
            }
        }
        if let ("", body) = current_func {
            return Ok(Expr::Scope(body))
        } else {
            panic!("Named scope at top of stack.");
        }
    }

    fn parenthesize_into(&self, buf: &mut String) {
        match *self {
            Expr::Var(ref name) => buf.push_str(name),
            Expr::Func(ref arg, ref body) => {
                buf.push_str("(λ");
                buf.push_str(arg);
                buf.push('.');
                parenthesize_vec(buf, &body);
                buf.push(')');
            }
            Expr::Scope(ref body) => {
                buf.push('(');
                parenthesize_vec(buf, &body);
                buf.push(')');
            }
        }
    }
}

fn parenthesize_vec<'t>(buf: &mut String, vec: &Vec<Expr<'t>>) {
    for _ in 0..vec.len() {
        buf.push('(');
    }
    for exp in vec {
        exp.parenthesize_into(buf);
        buf.push(')');
    }
}

fn main() {
    println!("{:?}", Expr::with_input("λab.a (b c) a λc.d").unwrap());
}
