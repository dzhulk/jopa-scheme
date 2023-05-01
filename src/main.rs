extern crate rustyline;

// use rustyline::error::ReadlineError;
// use rustyline::DefaultEditor;
// use std::collections::LinkedList;

use std::fs;
use std::cmp::{PartialEq, Eq};

const SKIP_SYMS: [char; 4] = ['\n', '\r', ' ', '\t'];
const OPS: [char; 7] = ['+', '-', '=', '*', '/', '>', '<'];

#[derive(Debug, PartialEq, Eq)]
enum Token {
    LPAR,
    RPAR,
    NUM(String),
    SYM(String),
    STR(String),
    OP(String),
}

#[derive(Debug)]
struct Lexer<'a> {
    input: &'a [char],
}

impl<'a> Lexer<'a> {
    fn peek(&self) -> Option<char> {
        if self.input.is_empty() {
            return None
        } else {
            return Some(self.input[0])
        }
    }

    fn next(&mut self) -> char {
        let chr = self.input[0];
        self.input = &self.input[1..];
        return chr;
    }

    fn chop_while<F>(&mut self, f: F) -> String where F: Fn(char)->bool {
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
        let mut tokens : Vec<Token> = Vec::new();
        while let Some(_) = self.peek() {
            match self.peek() {
                Some('(') => {
                    self.next();
                    tokens.push(Token::LPAR);
                },
                Some(')') => {
                    self.next();
                    tokens.push(Token::RPAR);
                },
                Some(s) if s.is_alphabetic() => {
                    let token = self.chop_while(|c| c.is_alphanumeric());
                    tokens.push(Token::SYM(token))
                }
                Some(s) if s.is_numeric() => {
                    let token = self.chop_while(|c| c.is_numeric());
                    tokens.push(Token::NUM(token));
                }
                Some(s) if SKIP_SYMS.contains(&s) => {
                    self.next();
                }
                Some(s) if OPS.contains(&s) => {
                    tokens.push(Token::OP(self.next().to_string()));
                },
                Some('"') => {
                    self.next();
                    tokens.push(Token::STR(self.chop_while(|ch| ch != '"')));
                    self.next();
                },
                any => {
                    println!("Unknown {any:?}");
                    todo!();
                },
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
        return SExp::Cons { car: Box::new(value), cdr: Box::new(SExp::Nil) };
    }

    fn append_to_list(self, other: SExp) -> SExp {
        match self {
            Self::Cons { car, cdr } => {
                if *cdr == SExp::Nil {
                    let new_list = SExp::Cons { car: Box::new(other), cdr: Box::new(SExp::Nil) };
                    Self::Cons { car: car, cdr: Box::new(new_list) }
                } else {
                    Self::Cons { car: car, cdr: Box::new(cdr.append_to_list(other)) }
                }
            },
            _ => {
                eprintln!("ERROR: append to not list type");
                todo!();
            }
        }
    }
}

fn parse_sexp(token: &Token) -> SExp {
    match token {
        Token::SYM(str) => {
            if str == "quote" {
                SExp::Quote
            } else {
                SExp::Id(String::from(str))
            }
        },
        Token::STR(str) => SExp::Sym(String::from(str)),
        Token::OP(str)  => SExp::Id(String::from(str)),
        Token::NUM(str) => SExp::Num(String::from(str).parse::<i32>().unwrap()),
        any => {
            eprintln!("ERROR: Unknown token: {any:?}");
            todo!();
        },
    }
}

fn parse_list(input: &[Token]) -> SExp {
    let mut stack: Vec<SExp> = Vec::new();
    let mut tokens = input;

    while !tokens.is_empty() {
        match tokens[0] {
            Token::LPAR => {
                let curr_sexp = SExp::new_list(parse_sexp(&tokens[1]));
                stack.push(curr_sexp);
                tokens = &tokens[2..];
            },
            Token::RPAR => {
                println!("Stack len: {len}", len = stack.len());
                if stack.len() == 1 { // because we skiped first LPAR
                    println!("Left tokens: {tokens:?}"); // should be RPAR
                    break;
                } else {
                    let curr_sexp = stack.pop().unwrap();
                    let parent = stack.pop().unwrap();
                    let new_list = parent.append_to_list(curr_sexp);
                    stack.push(new_list);
                    tokens = &tokens[1..];
                }
            },
            Token::SYM(_) | Token::STR(_) | Token::OP(_) | Token::NUM(_) => {
                let curr_sexp = stack.pop().unwrap();
                let new_list = curr_sexp.append_to_list(parse_sexp(&tokens[0]));
                stack.push(new_list);
                tokens = &tokens[1..];
            },
        }
    }

    return stack.pop().unwrap();
}

struct EvalEnvironment {

}

fn main() {
    let args= std::env::args();

    let arguments = args.collect::<Vec<_>>();

    let file_path  = &arguments[1];
    println!("Reading {file_path}");
    let content = fs::read_to_string(file_path).expect("Cant read file content");
    let chars = content.chars().into_iter().collect::<Vec<_>>();
    let mut lexer = Lexer { input: &chars };
    let tokens = lexer.parse();
    println!("Token: #{tokens:?}");
    let sexp = parse_list(tokens.as_slice());
    println!("SExpr: {sexp:?}");

}

/*

define
println
join
quote
+ - * /

*/
