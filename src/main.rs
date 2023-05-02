extern crate rustyline;

use std::collections::HashMap;

use std::cmp::{Eq, PartialEq};
use std::fs;

const SKIP_SYMS: [char; 4] = ['\n', '\r', ' ', '\t'];
const OPS: [char; 7] = ['+', '-', '=', '*', '/', '>', '<'];

#[derive(Debug, PartialEq, Eq)]
enum Token {
    LPar,
    RPar,
    Num(String),
    Sym(String),
    Str(String),
    Op(String),
}

#[derive(Debug)]
struct Lexer<'a> {
    input: &'a [char],
}

impl<'a> Lexer<'a> {
    fn peek(&self) -> Option<char> {
        if self.input.is_empty() {
            return None;
        } else {
            return Some(self.input[0]);
        }
    }

    fn next(&mut self) -> char {
        let chr = self.input[0];
        self.input = &self.input[1..];
        return chr;
    }

    fn chop_while(&mut self, f: impl Fn(char) -> bool) -> String
    {
        let mut result: Vec<char> = Vec::new();
        while let Some(nxt) = self.peek() {
            if f(nxt) {
                result.push(self.next());
            } else {
                break;
            }
        }
        return result.iter().collect();
    }

    fn parse(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(_) = self.peek() {
            match self.peek() {
                Some('(') => {
                    self.next();
                    tokens.push(Token::LPar);
                }
                Some(')') => {
                    self.next();
                    tokens.push(Token::RPar);
                }
                Some(s) if s.is_alphabetic() => {
                    let token = self.chop_while(|c| c.is_alphanumeric());
                    tokens.push(Token::Sym(token))
                }
                Some(s) if s.is_numeric() => {
                    let token = self.chop_while(|c| c.is_numeric());
                    tokens.push(Token::Num(token));
                }
                Some(s) if SKIP_SYMS.contains(&s) => {
                    self.next();
                }
                Some(s) if OPS.contains(&s) => {
                    tokens.push(Token::Op(self.next().to_string()));
                }
                Some('"') => {
                    self.next();
                    tokens.push(Token::Str(self.chop_while(|ch| ch != '"')));
                    self.next();
                }
                any => {
                    println!("Unknown {any:?}");
                    todo!();
                }
            };
        }
        return tokens;
    }
}

#[derive(Debug, PartialEq, Eq)]
enum SExp {
    Id(String),
    Sym(String),
    Num(i32),
    Quote,
    Cons { car: Box<SExp>, cdr: Box<SExp> },
    Nil,
}

impl SExp {
    fn new_list(value: SExp) -> SExp {
        return SExp::Cons {
            car: Box::new(value),
            cdr: Box::new(SExp::Nil),
        };
    }

    fn is_id(&self, val: &str) -> bool {
        match self {
            Self::Id(id) => *id == String::from(val),
            _ => false,
        }
    }

    fn append_to_list(self, other: SExp) -> SExp {
        match self {
            Self::Cons { car, cdr } => {
                if *cdr == SExp::Nil {
                    let new_list = Self::Cons {
                        car: Box::new(other),
                        cdr: Box::new(SExp::Nil),
                    };
                    Self::Cons {
                        car: car,
                        cdr: Box::new(new_list),
                    }
                } else {
                    Self::Cons {
                        car: car,
                        cdr: Box::new(cdr.append_to_list(other)),
                    }
                }
            }
            _ => {
                eprintln!("ERROR: append to not list type");
                todo!();
            }
        }
    }
}

fn parse_sexp(token: &Token) -> SExp {
    match token {
        Token::Sym(str) => {
            if str == "quote" {
                SExp::Quote
            } else {
                SExp::Id(String::from(str))
            }
        }
        Token::Str(str) => SExp::Sym(String::from(str)),
        Token::Op(str) => SExp::Id(String::from(str)),
        Token::Num(str) => SExp::Num(String::from(str).parse::<i32>().unwrap()),
        any => {
            eprintln!("ERROR: Unknown token: {any:?}");
            todo!();
        }
    }
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    sexprs: Vec<SExp>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        return Parser {
            tokens: tokens,
            sexprs: Vec::new(),
        };
    }

    fn drop_tokens(&mut self, n: usize) {
        self.tokens = self.tokens.drain(n..).collect();
    }

    fn parse_tokens(&mut self) {
        let mut stack: Vec<SExp> = Vec::new();

        while !self.tokens.is_empty() {
            match self.tokens[0] {
                Token::LPar => {
                    stack.push(SExp::new_list(parse_sexp(&self.tokens[1])));
                    self.drop_tokens(2);
                }
                Token::RPar => {
                    if stack.len() == 1 {
                        self.drop_tokens(1);
                        self.sexprs.push(stack.pop().unwrap());
                    } else {
                        let curr_sexp = stack.pop().unwrap();
                        let parent = stack.pop().unwrap();
                        let new_list = parent.append_to_list(curr_sexp);
                        stack.push(new_list);
                        self.drop_tokens(1);
                    }
                }
                Token::Sym(_) | Token::Str(_) | Token::Op(_) | Token::Num(_) => {
                    let new_list = stack
                        .pop()
                        .unwrap()
                        .append_to_list(parse_sexp(&self.tokens[0]));
                    stack.push(new_list);
                    self.drop_tokens(1);
                }
            }
        }
    }
}

struct EvalEnvironment {
    met_table: HashMap<SExp, SExp>,
    var_table: HashMap<SExp, SExp>,
}

impl EvalEnvironment {
    fn new() -> Self {
        return EvalEnvironment {
            met_table: HashMap::new(),
            var_table: HashMap::new(),
        };
    }
}

// fn eval_expr(expr: SExp, env: &mut EvalEnvironment) -> SExp {
//     match expr {
//         SExp::Cons { car, cdr } if car.is_id("define") => {}
//         _ => {}
//     }
// }

fn main() {
    let args = std::env::args();

    let arguments = args.collect::<Vec<_>>();

    let file_path = &arguments[1];
    println!("Reading {file_path}");
    let content = fs::read_to_string(file_path).expect("Cant read file content");
    let chars = content.chars().into_iter().collect::<Vec<_>>();
    let mut lexer = Lexer { input: &chars };
    let tokens = lexer.parse();
    println!("Token: #{tokens:?}");
    let mut parser = Parser::new(tokens);
    parser.parse_tokens();
    println!("Parser: {parser:?}");



    // let s1 = SExp::Id("some 32".to_string());
    // let s2 = SExp::Id("some 32".to_string());

    // if s1 == s2 {
    //     println!("EQUAL!");
    // }
}

/*

define
println
join
quote
+ - * /

*/
