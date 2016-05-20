use std::io::{self, Read};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::iter::Peekable;

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

pub type LambdaParseResult<T> = Result<T, &'static str>;

pub struct TokenStream<'a> {
    tokens: Box<Iterator<Item=Token<'a>> + 'a>,
    next: Option<Token<'a>>
}

impl<'a> TokenStream<'a> {
    fn new(s: &'a str) -> TokenStream<'a> {
        let mut tokens = s.split("").filter_map(|c| {
            match c {
                // split("") produces a "" at the start and end of the iterator
                "" | " " | "\n" | "\t" => None,
                "/" | "λ" => Some(Token::new(TokenKind::FuncDecStart)),
                "." => Some(Token::new(TokenKind::FuncDecEnd)),
                "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" |
                    "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
                    => Some(Token::with_value(TokenKind::Variable, c)),
                "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" =>
                    Some(Token::with_value(TokenKind::Variable, c)),
                "(" => Some(Token::new(TokenKind::LParen)),
                ")" => Some(Token::new(TokenKind::RParen)),
                _ => panic!("Unexpected token!"),
            }
        });
        let next = tokens.next();
        TokenStream {
            tokens: Box::new(tokens),
            next: next
        }
    }

    fn get_token_kind_error(kind: TokenKind) -> &'static str {
        match kind {
            TokenKind::Variable => "Unable to match variable.",
            TokenKind::FuncDecStart => "Unable to match function declaration start.",
            TokenKind::FuncDecEnd => "Unable to match function declaration end.",
            TokenKind::LParen => "Unable to match left parenthese.",
            TokenKind::RParen => "Unable to match right parenthese.",
        }
    }

    fn try_consume(&mut self, target_kind: TokenKind) -> bool {
        match self.next {
            Some(Token {kind, value: _}) if kind == target_kind => {
                self.next = self.tokens.next();
                true
            },
            _ => false
        }
    }

    fn consume(&mut self, kind: TokenKind) -> LambdaParseResult<()> {
        if self.try_consume(kind) {
            Ok(())
        } else {
            Err(TokenStream::<'a>::get_token_kind_error(kind))
        }
    }

    fn try_consume_value(&mut self, target_kind: TokenKind) -> Option<&'a str> {
        match self.next {
            Some(Token {kind, value: v}) if kind == target_kind => {
                self.next = self.tokens.next();
                Some(v.expect(
                    &format!("Expected token of kind {:?}) to have a value.", target_kind)
                    ))
            },
            _ => None
        }
    }

    fn consume_value(&mut self, target_kind: TokenKind) -> LambdaParseResult<&'a str> {
        match self.try_consume_value(target_kind) {
            Some(v) => Ok(v),
            _ => Err(TokenStream::<'a>::get_token_kind_error(target_kind))
        }
    }

    fn has_next(&self) -> bool {
        self.next.is_some()
    }
}

// App -> App Var | e
// Var -> Func | variable
// Func -> Parens | λ variable . App
// Parens -> ( App )

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LambdaExp<'a> {
    App(Rc<LambdaExp<'a>>, Rc<LambdaExp<'a>>),
    Func(Rc<LambdaExp<'a>>, Rc<LambdaExp<'a>>),
    Var(&'a str),
    None,
}

impl<'a> LambdaExp<'a> {
    fn parenthesize(&self) -> String {
        match *self {
            LambdaExp::App(ref left, ref right) =>
                format!("({} {})", &left.parenthesize(), &right.parenthesize()),
            LambdaExp::Func(ref left, ref right) =>
                format!("(λ{}.{})", &left.parenthesize(), &right.parenthesize()),
            LambdaExp::Var(ref name) => name.to_string(),
            _ => "".to_owned(),
        }
    }
}

impl<'a> Display for LambdaExp<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LambdaExp::App(ref left, ref right) =>
                // ** needed until we finally get something like the "box" keyword to use in
                // pattern matching.
                match (&**left, &**right) {
                    // When application is forced to be right associative
                    (_, &LambdaExp::App(_, _)) => write!(f, "{} ({})", left, right),
                    // When functions are forced to be left associative
                    (&LambdaExp::Func(_, _), &LambdaExp::Func(_, _)) =>
                        write!(f, "({}) ({})", left, right),
                    (&LambdaExp::App(_, _), &LambdaExp::Func(_, _)) =>
                        write!(f, "{} ({})", left, right),
                    (&LambdaExp::Func(_, _), _) =>
                        write!(f, "({}) {}", left, right),
                    _ => write!(f, "{} {}", left, right),
                },
            LambdaExp::Func(ref arg, ref body) => write!(f, "λ{}.{}", arg, body),
            LambdaExp::Var(s) => write!(f, "{}", s),
            LambdaExp::None => write!(f, ""),
        }
    }
}

pub fn parse_app<'a>(tokens: &mut TokenStream<'a>) -> LambdaParseResult<LambdaExp<'a>> {
    let mut current_value = try!(parse_var(tokens));
    let mut next_value = try!(parse_var(tokens));
    while next_value != LambdaExp::None {
        current_value = LambdaExp::App(Rc::new(current_value), Rc::new(next_value));
        next_value = try!(parse_var(tokens));
    }
    Ok(current_value)
}

fn parse_var<'a>(tokens: &mut TokenStream<'a>) -> LambdaParseResult<LambdaExp<'a>> {
    if let Some(v) = tokens.try_consume_value(TokenKind::Variable) {
        Ok(LambdaExp::Var(v))
    } else {
        parse_func(tokens)
    }
}

fn parse_func<'a>(tokens: &mut TokenStream<'a>) -> LambdaParseResult<LambdaExp<'a>> {
    if tokens.try_consume(TokenKind::FuncDecStart) {
        let arg = LambdaExp::Var(try!(tokens.consume_value(TokenKind::Variable)));
        try!(tokens.consume(TokenKind::FuncDecEnd));
        let func_body = try!(parse_app(tokens));
        Ok(LambdaExp::Func(Rc::new(arg), Rc::new(func_body)))
    } else {
        parse_parens(tokens)
    }
}

fn parse_parens<'a>(tokens: &mut TokenStream<'a>) -> LambdaParseResult<LambdaExp<'a>> {
    if tokens.try_consume(TokenKind::LParen) {
        let contents = try!(parse_app(tokens));
        if contents == LambdaExp::None {
            return Err("Expected an expression inside parentheses.")
        }
        try!(tokens.consume(TokenKind::RParen));
        Ok(contents)
    } else {
        Ok(LambdaExp::None)
    }
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
                fn try_simplify<'a>(exp: Rc<LambdaExp<'a>>) -> Option<Rc<LambdaExp<'a>>> {
                    let simplified = simplify_once(exp.clone());
                    if simplified == exp {
                        None
                    } else {
                        Some(simplified)
                    }
                }
                // Try simplifying the left and the right and then give up
                try_simplify(right.to_owned())
                    .or_else(|| try_simplify(left.to_owned()))
                    .unwrap_or_else(|| root_exp.to_owned())
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

pub fn simplify<'a>(root_exp: Rc<LambdaExp<'a>>) -> Vec<Rc<LambdaExp<'a>>> {
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

    let tokens = LE::tokenize(&buf);
    println!("{:?}", tokens);
}

#[derive(Debug, Eq, PartialEq)]
enum LEToken<'s> {
    Var(&'s str),
    FuncStart,
    FuncParamEnd,
    LParen,
    RParen,
    Err,
}

pub type LambdaExpResult<T> = Result<T, &'static str>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum LE<'s> {
    Var(&'s str),
    App(Box<LE<'s>>, Box<LE<'s>>),
    Func(&'s str, Box<LE<'s>>),
}

impl<'s> LE<'s> {
    pub fn new(exp: &'s str) -> LambdaExpResult<LE<'s>> {
        LE::parse(exp)
    }

    fn tokenize(s: &'s str) -> Vec<LEToken<'s>> {
        fn is_word(c: char) -> bool {
            match c {
                'a'...'z'
                    | 'A'...'Z'
                    | '0'...'9'
                    | '_' => true,
                _ => false,
            }
        }

        fn is_word_start(c: char) -> bool {
            match c {
                'a'...'z'
                    | 'A'...'Z'
                    | '_' => true,
                _ => false,
            }
        }

        fn is_number(c: char) -> bool {
            match c {
                '0'...'9' => true,
                _ => false,
            }
        }

        fn read_while<F, I>(predicate: F, iter: &mut Peekable<I>, s: &str) -> usize
                where F: Fn(char) -> bool, I: Iterator<Item=(usize, char)> {
            // Consume all word characters
            while let Some(&(_, c)) = iter.peek() {
                if predicate(c) {
                    iter.next();
                } else {
                    break;
                }
            }
            // I'm not sure whether or not string indices when you index into a string
            // are code point indices or byte indices (I'll have to check this at some
            // point). This code should work in either situation.
            match iter.peek() {
                Some(&(i, _)) => i - 1,
                None => s.len() - 1
            }
        }

        let mut tokens = Vec::new();
        let mut iter = s.char_indices().peekable();
        while let Some((i, c)) = iter.next() {
            println!("next!");
            match c {
                ' ' | '\t' | '\n' => (),
                '(' => tokens.push(LEToken::LParen),
                ')' => tokens.push(LEToken::RParen),
                'λ' | '/' | '\\' => tokens.push(LEToken::FuncStart),
                '.' => tokens.push(LEToken::FuncParamEnd),
                // Read a variable
                _ if is_word_start(c) => {
                    let end = read_while(is_word, &mut iter, s);
                    tokens.push(LEToken::Var(&s[i..end + 1]))
                },
                _ if is_number(c) => {
                    let end = read_while(is_number, &mut iter, s);
                    tokens.push(LEToken::Var(&s[i..end + 1]))
                },
                _ => tokens.push(LEToken::Err),
            }
        }
        tokens
    }

    fn parse(s: &'s str) -> LambdaExpResult<LE<'s>> {
        // Tokenize
        let mut tokens = LE::tokenize(s);

        fn parse_app<'s, T: Iterator<Item=LEToken<'s>>>(tokens: &mut Peekable<T>)
                -> LambdaExpResult<LE<'s>> {
            let mut left = try!(parse_var(tokens));
            loop {
                match tokens.peek() {
                    None | Some(&LEToken::RParen) => return Ok(left),
                    _ => {
                        let right = try!(parse_var(tokens));
                        left = LE::App(Box::new(left), Box::new(right));
                    },
                }
            }
        }

        fn parse_var<'s, T: Iterator<Item=LEToken<'s>>>(tokens: &mut Peekable<T>)
                 -> LambdaExpResult<LE<'s>> {
            if let Some(&LEToken::Var(s)) = tokens.peek() {
                tokens.next();
                Ok(LE::Var(s))
            } else {
                parse_func(tokens)
            }
         }

        fn parse_func<'s, T: Iterator<Item=LEToken<'s>>>(tokens: &mut Peekable<T>)
                 -> LambdaExpResult<LE<'s>> {
            match tokens.peek() {
                Some(&LEToken::FuncStart) => {tokens.next();},
                _ => return parse_parens(tokens),
            }

            let arg = match tokens.next() {
                Some(LEToken::Var(arg)) => arg,
                _ => return Err("Expected function argument."),
            };

            match tokens.next() {
                Some(LEToken::FuncParamEnd) => (),
                _ => return Err("Expected function param end."),
            }

            let body = try!(parse_app(tokens));
            Ok(LE::Func(arg, Box::new(body)))
        }

        fn parse_parens<'s, T: Iterator<Item=LEToken<'s>>>(tokens: &mut Peekable<T>)
                -> LambdaExpResult<LE<'s>> {
            match tokens.next() {
                Some(LEToken::LParen) => (),
                _ => return Err("Expected LParen"),
            }
            let body = try!(parse_app(tokens));
            match tokens.next() {
                Some(LEToken::RParen) => (),
                _ => return Err("Expected RParen"),
            }
            Ok(body)
        }

        parse_app(&mut tokens.into_iter().peekable())
    }

    pub fn reduce(&self) -> LambdaExpResult<Vec<LE<'s>>> {
        let mut steps = vec![self.to_owned()];
        let mut current = self.reduce_once();
        while &current != steps.last().expect("At least one element in the vec.") {
            steps.push(current);
            current = self.reduce_once();
        }
        Ok(steps)
    }

    fn reduce_once(&self) -> LE<'s> {
        self.to_owned()
    }
}

impl<'s> fmt::Display for LE<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LE::Var(s) => write!(f, "{}", s),
            LE::App(ref left, ref right) => write!(f, "({} {})", left, right),
            LE::Func(arg, ref body) => write!(f, "(λ{}.{})", arg, body),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::LEToken;
    use super::LE;

    #[test]
    fn tokenize_1() {
        assert_eq!(
            vec![
                LEToken::FuncStart,
                LEToken::Var("a"),
                LEToken::FuncParamEnd,
                LEToken::Var("b"),
                LEToken::Var("c"),
                LEToken::LParen,
                LEToken::FuncStart,
                LEToken::Var("apple"),
                LEToken::FuncParamEnd,
                LEToken::Var("123"),
                LEToken::Var("a234"),
                LEToken::Var("432"),
                LEToken::RParen,
                LEToken::LParen,
                LEToken::RParen,
                LEToken::FuncStart,
                LEToken::Var("x"),
                LEToken::FuncParamEnd,
                LEToken::Var("x"),
            ],
            LE::tokenize("/a.b c (λapple.123 a234 432) () \t\n\\x.x")
        );
    }

    #[test]
    fn tokenize_2() {
        assert_eq!(
            vec![
            LEToken::FuncStart,
            LEToken::Var("123"),
            LEToken::FuncParamEnd,
            LEToken::Var("123"),
            LEToken::Var("123"),
            ],
            LE::tokenize("/123.123 123")
        );
    }
}
