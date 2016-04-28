use std::io::{self, Read};
use std::fmt;
use std::mem;
use std::borrow::Cow;

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
                tokens.push(Token::Var(&input[start..(end + 1)]))
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

#[derive(Debug, Clone)]
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
                buf.push_str("λ");
                buf.push_str(arg);
                buf.push('.');
                parenthesize_vec(buf, &body);
            }
            Expr::Scope(ref body) => parenthesize_vec(buf, &body)
        }
    }

    fn is_func(&self) -> bool {
        match *self {
            Expr::Func(_, _) => true,
            _ => false,
        }
    }

    fn eval_once(&self) -> Cow<Expr<'t>> {
        match *self {
            Expr::Var(_) => Cow::Borrowed(self),
            Expr::Func(ref arg, ref body) => {
                match eval_vec(body) {
                    Cow::Borrowed(_) => Cow::Borrowed(self),
                    Cow::Owned(new_body) => Cow::Owned(Expr::Func(arg, new_body)),
                }
            },
            Expr::Scope(ref body) =>
                match eval_vec(body) {
                    Cow::Borrowed(_) => Cow::Borrowed(self),
                    Cow::Owned(mut new_body) => {
                        if new_body.len() > 1 {
                            Cow::Owned(Expr::Scope(new_body))
                        } else {
                            // if we simplified to a single result, just return that instead of a
                            // scope
                            Cow::Owned(new_body.remove(0))
                        }
                    },
                },
        }
    }
}

fn parenthesize_vec<'t>(buf: &mut String, vec: &Vec<Expr<'t>>) {
    fn parenthesize<'s>(buf: &mut String, e: &Expr<'s>) {
        if e.is_func() {
            buf.push('(');
            e.parenthesize_into(buf);
            buf.push(')');
        } else {
            e.parenthesize_into(buf);
        }
    }

    if vec.len() == 1 {
        parenthesize(buf, vec.first().expect("One element."));
    } else {
        // First element doesn't need "application" parentheses
        for _ in 0..vec.len() - 1 {
            buf.push('(');
        }
        parenthesize(buf, vec.first().expect("At least one element."));
        for e in vec[1..vec.len()].iter() {
            buf.push(' ');
            parenthesize(buf, e);
            buf.push(')');
        }
    }
}

fn replace_in_vec<'t>(input: &Vec<Expr<'t>>, i: usize, new_v: Expr<'t>) -> Vec<Expr<'t>> {
    let mut new_vec = input.clone();
    new_vec.remove(i);
    new_vec.insert(i, new_v);
    new_vec
}

fn eval_vec<'a, 't>(input: &'a Vec<Expr<'t>>) -> Cow<'a, Vec<Expr<'t>>> {
    // First look for something within our vec that we could eval
    for (i, expr) in input.iter().enumerate() {
        match *expr {
            // If we need to replace something, make a copy of the original vec, replace the
            // value, and return the new vec.
            Expr::Scope(ref body) => match eval_vec(body) {
                Cow::Owned(new_body) => {
                    return Cow::Owned(replace_in_vec(input, i, Expr::Scope(new_body)))
                },
                _ => continue
            },
            Expr::Func(arg, ref body) => match eval_vec(body) {
                Cow::Owned(new_body) => {
                    return Cow::Owned(replace_in_vec(input, i, Expr::Func(arg, new_body)))
                },
                _ => continue
            },
            _ => continue
        }
    }

    // If we haven't returned yet, then we should try to apply something in this vec. The only time
    // we can apply is if the first element is a function.
    match (input.get(0), input.get(1)) {
        (Some(&Expr::Func(arg, ref body)), Some(exp)) => {
            let mut new_body = substitute(body, arg, exp);
            let new_expr = if new_body.len() == 1 {
                new_body.remove(0)
            } else {
                Expr::Scope(new_body)
            };
            let mut new_vec = input.clone();
            // Panics if there are not two elements in the vec
            new_vec.remove(0);
            new_vec.remove(0);
            new_vec.insert(0, new_expr);
            return Cow::Owned(new_vec)
        },
        _ => (),
    }
    Cow::Borrowed(input)
}

fn substitute<'t>(body: &Vec<Expr<'t>>, param: &'t str, new_value: &Expr<'t>) -> Vec<Expr<'t>> {
    let new_body: Vec<_> = body.iter().map(|expr| {
        let v = match *expr {
            Expr::Var(name) if name == param => new_value,
            _ => expr
        };
        v.clone()
    }).collect();
    new_body
}

fn main() {
    let mut buf = String::new();
    let e =  Expr::with_input("(λa.a) (λb.b) (λc.c)").unwrap();
    e.parenthesize_into(&mut buf);
    println!("{}", buf);
    println!("{:?}", e);
    let next = e.eval_once();
    println!("{:?}", next);
}

#[cfg(test)]
mod tests {
    use super::{tokenize, Token, Expr};

    fn tokens_to_string<'t>(tokens: &Vec<Token<'t>>) -> String {
        let mut iter = tokens.iter().peekable();
        let mut buf = String::new();
        while let Some(token) = iter.next() {
            buf.push_str(match *token {
                Token::Var(s) => s,
                Token::FuncStart => "λ",
                Token::FuncDecEnd => ".",
                Token::LParen => "(",
                Token::RParen => ")",
            });
            if let Some(_) = iter.peek() {
                buf.push(' ');
            }
        }
        buf
    }

    #[test]
    fn mixed_tokens() {
        let tokens = tokens_to_string(&tokenize("\thλb.c").unwrap());
        assert_eq!(tokens, "h λ b . c");
    }

    #[test]
    fn mixed_long_tokens() {
        let tokens = tokens_to_string(&tokenize("     helloλa.batman c").unwrap());
        assert_eq!(tokens, "hello λ a . batman c");
    }

    #[test]
    fn check_parentheses() {
        let mut buf = String::new();
        Expr::with_input("λab.a (b c) d").unwrap().parenthesize_into(&mut buf);
        assert_eq!("(λab.((a (b c)) d))", buf);
    }
}
