mod exp;

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

    fn chop_while(&mut self, f: impl Fn(char) -> bool) -> String {
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
        while let Some(sym) = self.peek() {
            match sym {
                '(' => {
                    self.next();
                    tokens.push(Token::LPar);
                }
                ')' => {
                    self.next();
                    tokens.push(Token::RPar);
                }
                s if s.is_alphabetic() => {
                    let token = self.chop_while(|c| c.is_alphanumeric());
                    tokens.push(Token::Sym(token))
                }
                s if s.is_numeric() => {
                    let token = self.chop_while(|c| c.is_numeric());
                    tokens.push(Token::Num(token));
                }
                s if SKIP_SYMS.contains(&s) => {
                    self.next();
                }
                s if OPS.contains(&s) => {
                    tokens.push(Token::Op(self.next().to_string()));
                }
                '"' => {
                    self.next();
                    tokens.push(Token::Str(self.chop_while(|ch| ch != '"')));
                    self.next();
                },
                '#' => {
                    self.next();
                    self.chop_while(|ch| ch != '\n');
                    self.next();
                },
                any => {
                    println!("Unknown {any}");
                    todo!();
                }
            };
        }
        return tokens;
    }
}

#[derive(Debug, PartialEq, Eq)]
enum EvOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl EvOp {
    fn from_string(s: &String) -> Option<Self> {
        match s.as_str() {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            _   => None,
        }
    }
}


#[derive(Debug, PartialEq, Eq)]
enum EvCmp {
    Lt,
    Gt,
    Eq
}

impl EvCmp {
    fn from_string(s: &String) -> Option<Self> {
        match s.as_str() {
            "<" => Some(Self::Lt),
            ">" => Some(Self::Gt),
            "=" => Some(Self::Eq),
            _   => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum SExp {
    Id(String),
    Op(EvOp),
    Cmp(EvCmp),
    Sym(String),
    Num(i32),
    Bool(bool),
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

    #[allow(dead_code)]
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
                    Self::Cons {
                        car: car,
                        cdr: Box::new(Self::new_list(other)),
                    }
                } else {
                    Self::Cons {
                        car: car,
                        cdr: Box::new(cdr.append_to_list(other)),
                    }
                }
            }
            _ => {
                eprintln!("ERROR: append expected list type");
                todo!();
            }
        }
    }

    fn get_list_pair(&self) -> (&SExp, &SExp) {
        match self {
            Self::Cons { car, cdr } => {
                (car, cdr)
            },
            _ => {
                eprintln!("ERROR: expected list type");
                todo!();
            }
        }
    }

    fn get_num(&self) -> i32 {
        match self {
            Self::Num(num) => *num,
            _ => {
                eprintln!("ERROR: expected Num type");
                todo!();
            }
        }
    }
}

fn parse_sexp(token: &Token) -> SExp {
    match token {
        Token::Sym(str) => {
            match str.as_str() {
                "quote" => SExp::Quote,
                "true"  => SExp::Bool(true),
                "false"  => SExp::Bool(false),
                _ => SExp::Id(String::from(str)),
            }
        }
        Token::Str(str) => SExp::Sym(String::from(str)),
        Token::Op(str) => {
            match EvOp::from_string(str) {
                Some(op) => SExp::Op(op),
                None => {
                    let cmp = EvCmp::from_string(str).expect(format!("Can't parse Op {str}").as_str());
                    SExp::Cmp(cmp)
                }
            }
        },
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

fn eval_op(op: &EvOp, expr: &SExp, env: &mut EvalEnvironment, acc: Option<i32>) -> SExp {
    println!("acc: {acc:?}");
    match expr {
        SExp::Nil => SExp::Num(acc.unwrap()),
        _ => {
            let (n_car, n_cdr) = expr.get_list_pair();
            let SExp::Num(lhs_res) = eval_expr(n_car, env) else { todo!() };
            let new_acc = match op {
                EvOp::Add => acc.map(|a| a + lhs_res).or_else(|| Some(lhs_res)),
                EvOp::Sub => acc.map(|a| a - lhs_res).or_else(|| Some(lhs_res)),
                EvOp::Mul => acc.map(|a| a * lhs_res).or_else(|| Some(lhs_res)),
                EvOp::Div => acc.map(|a| a / lhs_res).or_else(|| Some(lhs_res)),
            };
            eval_op(op, n_cdr, env, new_acc)
        }
    }
}

fn eval_cmp_2(op: &EvCmp, expr: &SExp, env: &mut EvalEnvironment, acc: Option<SExp>) -> SExp {
    println!("acc: {acc:?}");
    match expr {
        SExp::Nil => SExp::Bool(true),
        _ => {
            let (n_car, n_cdr) = expr.get_list_pair();
             match eval_expr(n_car, env) {
                SExp::Num(n) => {
                    match acc {
                        Some(acc_exp ) if acc_exp.get_num() < n => {
                            SExp::Bool(false)
                        },
                        _ => {
                            eval_cmp_2(op, n_cdr, env, Some(SExp::Num(n)))
                        },
                    }
                },
                _ => {
                    todo!("Only nums cmp");
                }
            }
        }
    }
}

// fn eval_cmp(op: &EvCmp, expr: &SExp, env: &mut EvalEnvironment, acc: bool) -> SExp {
//     println!("acc: {acc:?}");
//     match expr {
//         SExp::Nil => SExp::Bool(acc),
//         _ => {
//             let (n_car, n_cdr) = expr.get_list_pair();
//             let SExp::Num(lhs_res) = eval_expr(n_car, env) else { todo!() };
//             let new_acc = match op {
//                 EvCmp::Gt => acc.map(|a| a > lhs_res).or_else(|| Some(lhs_res)),
//                 _ => SExp::Nil
//             };
//             eval_op(op, n_cdr, env, new_acc)
//         }
//     }
// }

fn eval_expr(expr: &SExp, env: &mut EvalEnvironment) -> SExp {
    match expr {
        SExp::Cons { car, cdr } => {
            match car.as_ref() {
                SExp::Op(s)=> { eval_op(s, cdr, env, None) },
                SExp::Cmp(s)=> { eval_cmp_2(s, cdr, env, None) },
                SExp::Num(num) => {
                    // println!("found num: {num}");
                    SExp::Num(*num)
                },
                _ => { SExp::Nil }
            }
        },
        SExp::Num(num) => {
            // println!("found num: {num}");
            SExp::Num(*num)
        },
        _ => { SExp::Nil},
    }
}


fn main() {
    let args = std::env::args();
    let arguments = args.collect::<Vec<_>>();

    let file_path = &arguments[1];
    println!("Reading '{file_path}'");
    let content = fs::read_to_string(file_path).expect("Cant read file content");
    println!("Source:\n--------\n{content}\n----------", content = content.trim());
    let chars = content.chars().into_iter().collect::<Vec<_>>();
    let mut lexer = Lexer { input: &chars };
    let tokens = lexer.parse();
    println!("Tokens: #{tokens:?}");
    println!("------------");
    let mut parser = Parser::new(tokens);
    parser.parse_tokens();

    let mut env = EvalEnvironment { met_table: HashMap::new(), var_table: HashMap::new() };

    println!("Expressions:");
    for expr in parser.sexprs.iter() {
        println!("\n-> {expr:?}\n");
        pretty_print_list(expr, 0);
        let result = eval_expr(expr, &mut env);
        println!("Result: {result:?}");
    }
}


fn pretty_print_list(list: &SExp, level: usize) {
    let cis = std::iter::repeat("    ").take(level).collect::<String>();
    match list {
        SExp::Cons { car, cdr } => {
            println!("{cis}Cons: {{");
            pretty_print_list(car, level+1);
            pretty_print_list(cdr, level+1);
            println!("{cis}}}");
        },
        any => {
            println!("{cis}{any:?}");
        }

    }

    ()
}

/*

define
println
join
quote
+ - * /

*/
