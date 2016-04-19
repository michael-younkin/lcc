use std::io::{self, Read};
use std::fmt;
use std::fmt::{Display, Formatter};

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
            ()
        } else {
            return Err(concat!("Expected ", stringify!($pat), "."))
        }
    }
}

enum Expr<'t> {
    Var(&'t str),
    Func(&'t str, Vec<Expr<'t>>),
    Scope(Vec<Expr<'t>>),
}

impl<'t> Expr<'t> {
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
    println!("{:?}", tokenize("λab.a (b c) a λc.d"));
}
