mod exp;

use std::collections::HashMap;
use std::cmp::{Eq, PartialEq};
use std::fs;

const SKIP_SYMS: [char; 5] = ['\n', '\r', ' ', '\t', ';'];
const OPS: [char; 8] = ['+', '-', '=', '*', '/', '%', '>', '<'];

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
                    let token = self.chop_while(|c| c.is_alphanumeric() || c == '?' );
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
                    tokens.push(Token::Op(self.chop_while(|ch| ch != ' ')));
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum EvMat {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl EvMat {
    fn from_string(s: &String) -> Option<Self> {
        match s.as_str() {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "%" => Some(Self::Mod),
            _   => None,
        }
    }

    fn do_mat(&self, lhs: i32, rhs: i32) -> i32 {
        match self {
            Self::Add => lhs + rhs,
            Self::Sub => lhs - rhs,
            Self::Mul => lhs * rhs,
            Self::Div => lhs / rhs,
            Self::Mod => lhs % rhs,
        }
    }
}


#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum EvCmp {
    Lt,
    Gt,
    Eq,
    GtEq,
    LtEq
}

impl EvCmp {
    fn from_string(s: &String) -> Option<Self> {
        match s.as_str() {
            "<" => Some(Self::Lt),
            ">" => Some(Self::Gt),
            "=" => Some(Self::Eq),
            ">=" => Some(Self::GtEq),
            "<=" => Some(Self::LtEq),
            _   => None,
        }
    }

    fn do_cmp<T>(&self, lhs: T, rhs: T) -> bool where T: PartialOrd + PartialEq {
        match self {
            Self::Eq => lhs == rhs,
            Self::Lt => lhs < rhs,
            Self::Gt => lhs > rhs,
            Self::GtEq => lhs >= rhs,
            Self::LtEq => lhs <= rhs,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum SExp {
    Id(String),
    Op(EvMat),
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

    fn is_list(&self) -> bool {
        match self {
            Self::Cons {car, cdr}=> true,
            _ => false,
        }
    }

    fn is_id(&self) -> bool {
        match self {
            Self::Id(_) => true,
            _ => false,
        }
    }

    fn is_nil(&self) -> bool {
        match self {
            Self::Nil => true,
            _ => false,
        }
    }

    fn get_id(&self) -> String {
        match self {
            Self::Id(id) => String::from(id),
            _ => {
                panic!("ERROR: cant't get id from {self:?}");
            }
        }
    }

    fn prepend_to_list(self, other: SExp) -> SExp {
        if self.is_list() || self.is_nil() {
            return Self::Cons { car: Box::new(other), cdr: Box::new(self) }
        }
        panic!("ERROR: cant prepted to not list type {self:?}");
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
            },
            Self::Nil => {
                Self::new_list(other)
            },
            _ => {
                eprintln!("ERROR: append expected list type");
                todo!();
            }
        }
    }

    fn get_list_vec(&self) -> Vec<SExp> {
        let mut res: Vec<SExp> = Vec::new();

        if self.is_nil() {
            return res;
        }

        loop {
            let (mut car, mut cdr) = self.get_list_pair();
            res.push(car.clone());
            while cdr.is_list() {
                (car, cdr) = cdr.get_list_pair();
                res.push(car.clone());
            }
            break;
        }

        return res;
    }

    fn get_cdr(&self) -> &SExp {
        match self {
            Self::Cons { car: _, cdr } => {
                cdr
            },
            _ => {
                eprintln!("ERROR: expected list type");
                todo!();
            }
        }
    }

    fn get_car(&self) -> &SExp {
        match self {
            Self::Cons { car, cdr: _ } => {
                car
            },
            _ => {
                eprintln!("ERROR: expected list type");
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

    fn as_string(&self) -> String {
        match self {
            Self::Num(num) => num.to_string(),
            Self::Sym(str) => str.clone(),
            Self::Nil => String::from("()"),
            Self::Bool(b) => if *b { String::from("true") } else { String::from("false") }
            lst if lst.is_list() => {
                return String::from("(") +
                    lst.get_list_vec().iter().map(|l| l.as_string()).collect::<Vec<_>>().join(" ").as_str() +
                    String::from(")").as_str();
            },
            _ => {
                panic!("ERROR: Not stringable type {self:?}");
            }
        }
    }

    fn cmp(&self, other: &SExp, cmp_op: &EvCmp) -> bool {
        match (self, other) {
            (Self::Num(lhs), Self::Num(rhs)) => cmp_op.do_cmp(lhs, rhs),
            (Self::Sym(lhs), Self::Sym(rhs)) => cmp_op.do_cmp(lhs, rhs),
            _ => {
                eprintln!("ERROR: not comparable types: {self:?} and {other:?}");
                todo!()
            }
        }
    }

    fn is_true(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            _ => {
                panic!("ERROR: Not boolean type {self:?}");
            }
        }
    }
}

fn parse_sexp(token: &Token) -> SExp {
    match token {
        Token::Sym(str) => {
            match str.as_str() {
                "quote" => SExp::Quote,
                "nil" => SExp::Nil,
                "true"  => SExp::Bool(true),
                "false"  => SExp::Bool(false),
                _ => SExp::Id(String::from(str)),
            }
        }
        Token::Str(str) => SExp::Sym(String::from(str)),
        Token::Op(str) => {
            match EvMat::from_string(str) {
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

type EnvTable = HashMap<String, SExp>;

#[derive(Debug)]
struct EvalEnvironment {
    arg_table: EnvTable,
    met_table: EnvTable,
    var_table: EnvTable,
}

impl EvalEnvironment {
    fn new() -> Self {
        return EvalEnvironment {
            arg_table: HashMap::new(),
            met_table: HashMap::new(),
            var_table: HashMap::new(),
        };
    }

    fn new_env_table() -> EnvTable {
        return  HashMap::new();
    }

    fn has_var(&self, name: &str) -> bool {
        self.var_table.contains_key(name)
    }

    fn has_met(&self, name: &str) -> bool {
        self.met_table.contains_key(name)
    }

    fn get_var(&self, name: &str) -> &SExp {
        self.var_table.get(name).unwrap()
    }

    fn get_args(&self, name: &str) -> Option<&SExp> {
        self.arg_table.get(name)
    }

    fn get_met(&self, name: &str) -> &SExp {
        self.met_table.get(name).unwrap()
    }

    fn eval_expr(&mut self, expr: &SExp, loc_table: &mut EnvTable) -> SExp {
        // pretty_print_list(expr, 0);
        match expr {
            SExp::Cons { car, cdr } => {
                match car.as_ref() {
                    SExp::Id(_)=> { self.eval_func(car, cdr, None, loc_table) },
                    SExp::Op(s)=> { self.eval_mat(s, cdr, None, loc_table) },
                    SExp::Cmp(s)=> { self.eval_cmp(s, cdr, None, loc_table) },
                    SExp::Num(num) => SExp::Num(num.clone()),
                    SExp::Sym(sym) => SExp::Sym(sym.clone()),
                    SExp::Bool(b) => SExp::Bool(b.clone()),
                    SExp::Nil => SExp::Nil,
                    _ => {
                        panic!("ERROR: can't match eval expr car: {car:?}");
                    }
                }
            },
            SExp::Id(_) => self.eval_func(expr, &SExp::Nil, None, loc_table),
            SExp::Num(num) => SExp::Num(num.clone()),
            SExp::Sym(sym) => SExp::Sym(sym.clone()),
            SExp::Bool(b) => SExp::Bool(b.clone()),
            SExp::Nil => SExp::Nil,
            _ => {
                panic!("ERROR: can't match eval expr: {expr:?}");
            }
        }
    }

    fn eval_mat(&mut self, op: &EvMat, expr: &SExp, acc: Option<i32>, loc_table: &mut EnvTable) -> SExp {
        match expr {
            SExp::Nil => SExp::Num(acc.unwrap()),
            _ => {
                let (n_car, n_cdr) = expr.get_list_pair();
                let SExp::Num(lhs) = self.eval_expr(n_car, loc_table) else { panic!("Math on not Num type!") };
                let new_acc = acc.map(|v| op.do_mat(v, lhs)).or(Some(lhs));
                self.eval_mat(op, n_cdr, new_acc, loc_table)
            }
        }
    }
    fn eval_cmp(&mut self, op: &EvCmp, expr: &SExp, acc: Option<SExp>,  loc_table: &mut EnvTable) -> SExp {
        if acc.is_none() {
            // println!("Eval: {op:?} -> {expr:?}");
        }

        match expr {
            SExp::Nil => SExp::Bool(true),
            _ => {
                let (n_car, n_cdr) = expr.get_list_pair();
                let lhs = self.eval_expr(n_car, loc_table);
                let next_acc = match acc {
                    Some(a) =>
                        if a.cmp(&lhs, op) {
                            lhs
                        } else {
                            return SExp::Bool(false)
                        }
                    None => lhs
                };
                return self.eval_cmp(op, n_cdr, Some(next_acc), loc_table);
            }
        }
    }



    fn eval_func(&mut self, id: &SExp, expr: &SExp, acc: Option<SExp>, loc_table: &mut EnvTable) -> SExp {
        // println!("Evaling exp {expr:?}");
        match id.get_id().as_str() {
            "list" => {
                match expr {
                    SExp::Nil => { return acc.unwrap() },
                    SExp::Cons { car, cdr } => {
                        let res = self.eval_expr(car, loc_table);
                        let new_acc = acc.map(|a| a.append_to_list(res.clone())).or(Some(SExp::new_list(res)));
                        return self.eval_func(id, cdr, new_acc, loc_table);
                    },
                    _ => {
                        panic!("Can't parse 'do' {expr:?}");
                    }
                };
            }
            "do" => {
                match expr {
                    SExp::Nil => { return acc.unwrap() },
                    SExp::Cons { car, cdr } => {
                        let res = self.eval_expr(car, loc_table);
                        return self.eval_func(id, cdr, Some(res), loc_table);
                    },
                    _ => {
                        panic!("Can't parse 'do' {expr:?}");
                    }
                };
            },
            "if" => {
                match expr {
                    SExp::Nil => {
                        return acc.unwrap();
                    },
                    SExp::Cons { car, cdr } => {
                        let cond_res = self.eval_expr(car, loc_table);
                        let (true_branch, false_branch) = cdr.get_list_pair();
                        if cond_res.is_true() {
                            return self.eval_expr(true_branch, loc_table);
                        } else {
                            match false_branch {
                                SExp::Nil => {
                                    return SExp::Nil
                                },
                                _ => {
                                    let (false_branch, _) = false_branch.get_list_pair();
                                    return self.eval_expr(false_branch, loc_table);
                                }
                            }
                        }
                    },
                    _ => {
                        panic!("Error in if cond {expr:?}");
                    }
                };
            },
            "or"  => {
                match expr {
                    SExp::Nil => {
                        return SExp::Bool(false)
                    },
                    SExp::Cons { car, cdr } => {
                        let lhs = self.eval_expr(car, loc_table);
                        let new_acc = match acc {
                            Some(_) =>
                                if lhs.is_true() {
                                    return SExp::Bool(true)
                                } else {
                                    lhs
                                }
                            None => lhs
                        };
                        return self.eval_func(id, cdr, Some(new_acc), loc_table);
                    },
                    _ => {
                        panic!("Not joinable {expr:?}");
                    }
                };
            }
            "and" => {
                match expr {
                    SExp::Nil => {
                        return acc.unwrap();
                    },
                    SExp::Cons { car, cdr } => {
                        let lhs = self.eval_expr(car, loc_table);
                        let new_acc = match acc {
                            Some(a) =>
                                if a.is_true() && lhs.is_true() {
                                    lhs
                                } else {
                                    return SExp::Bool(false)
                                }
                            None => lhs
                        };
                        return self.eval_func(id, cdr, Some(new_acc), loc_table);
                    },
                    _ => {
                        panic!("Not joinable {expr:?}");
                    }
                };
            },
            "nil?" => {
                match expr {
                    SExp::Nil => { panic!("Nil takes 1 arg") },
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_table);
                        // println!("lhs: {lhs:?}");
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_table);
                        }
                        if lhs.is_nil() {
                            return SExp::Bool(true);
                        } else {
                            return SExp::Bool(false);
                        }
                    },
                    _ => {
                        panic!("Can't parse 'isnil' {expr:?}");
                    }
                };
            },
            "cdr" => {
                match expr {
                    SExp::Nil => { panic!("CDR from empty list") },
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_table);
                        // println!("lhs: {lhs:?}");
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_table);
                        }
                        if lhs.is_list() && !lhs.is_nil() {
                            return lhs.get_cdr().clone();
                        } else {
                            panic!("CDR works only on lists {expr:?}");
                        }
                    },
                    _ => {
                        panic!("Can't parse 'cdr' {expr:?}");
                    }
                };
            },
            "list?" => {
                match expr {
                    SExp::Nil => { panic!("list? expects 1 argument") },
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_table);
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_table);
                        }
                        return SExp::Bool(lhs.is_list() || lhs.is_nil());
                    }
                    _ => {
                        panic!("Can't parse 'list?' {expr:?}");
                    }
                };
            },
            "length" => {
                match expr {
                    SExp::Nil => { panic!("LENGTH expects 1 argument") },
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_table);
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_table);
                        }
                        if lhs.is_list() || lhs.is_nil() {
                            return SExp::Num(lhs.get_list_vec().len() as i32);
                        } else {
                            panic!("LENGTH works only on lists {expr:?}");
                        }
                    },
                    _ => {
                        panic!("Can't parse 'length' {expr:?}");
                    }
                };
            },
            "car" => {
                match expr {
                    SExp::Nil => { panic!("Car from empty list") },
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_table);
                        // println!("lhs: {lhs:?}");
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_table);
                        }
                        if lhs.is_list() && !lhs.is_nil() {
                            return lhs.get_car().clone();
                        } else {
                            panic!("Car works only on lists {expr:?}");
                        }
                    },
                    _ => {
                        panic!("Can't parse 'car' {expr:?}");
                    }
                };
            },
            "cons" => {
                match expr {
                    SExp::Nil => { return acc.unwrap() },
                    SExp::Cons { car, cdr } => {
                        // println!("Len: {len}", len=);
                        let len = expr.get_list_vec().len();
                        if len > 2 {
                            panic!("Cons takes only 2 arguments: {expr:?}");
                        }

                        // println!("cdr: {cdr:?}");
                        let first = self.eval_expr(car, loc_table);
                        // we know there are only two args, so seconds
                        // only should have car
                        let second = self.eval_expr(cdr.get_car(), loc_table);

                        // println!("first {first:?}");
                        // println!("second {second:?}");

                        if second.is_list() || second.is_nil() {
                            return second.prepend_to_list(first);
                        } else {
                            return SExp::new_list(second).prepend_to_list(first);
                        }
                    },
                    _ => {
                        panic!("Can't parse 'cons' {expr:?}");
                    }
                };
            },
            "concat" => {
                match expr {
                    SExp::Nil => {
                        let output = acc.map(|a| a.as_string()).or(Some(String::from(""))).unwrap();
                        return SExp::Sym(output)
                    },
                    SExp::Cons { car, cdr } => {
                        let lhs = self.eval_expr(car, loc_table);
                        let str = lhs.as_string();
                        let new_acc = acc.map(|a| {
                            SExp::Sym(a.as_string() + " " + &str)
                        }).or(Some(SExp::Sym(str)));
                        return self.eval_func(id, cdr, new_acc, loc_table);
                    },
                    _ => {
                        panic!("Not joinable {expr:?}");
                    }
                };
            },
            "println" => {
                match expr {
                    SExp::Nil => {
                        let output = acc.map(|a| a.as_string()).or(Some(String::from(""))).unwrap();
                        println!("{output}");
                        return SExp::Nil;
                    },
                    SExp::Cons { car, cdr } => {
                        let mut lhs = self.eval_expr(car, loc_table);
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_table);
                        }
                        let str = lhs.as_string();
                        let new_acc = acc.map(|a| {
                            SExp::Sym(a.as_string() + " " + &str)
                        }).or(Some(SExp::Sym(str)));
                        return self.eval_func(id, cdr, new_acc, loc_table);
                    },
                    _ => {
                        panic!("Not printable {expr:?}");
                    }
                };
            },
            "define" => {
                match expr {
                    SExp::Cons { car, cdr } if car.is_id() => {
                        let (n_cdr, _) = cdr.get_list_pair();
                        let var = self.eval_expr(n_cdr, loc_table);
                        self.var_table.insert(car.get_id(), var);
                        return SExp::Nil;
                    },
                    SExp::Cons { car, cdr } if car.is_list() => {
                        let (name, args) = car.get_list_pair();
                        self.arg_table.insert(name.get_id(), args.clone());
                        let (body, _) = cdr.get_list_pair();
                        self.met_table.insert(name.get_id(), body.clone());
                        return SExp::Nil;
                    },
                    _ => {
                        panic!("ERROR: invalid define {expr:?}");
                    }
                }
            },
            var if self.has_var(var) => {
                return self.get_var(var).clone();
            }
            var if loc_table.contains_key(var) => {
                return loc_table.get(var).unwrap().clone();
            }
            met if self.has_met(met) => {
                let func = self.get_met(met).clone();
                let mut new_loc_table = EvalEnvironment::new_env_table();
                if let Some(args) = self.get_args(met) {
                    let args_exprs = args.get_list_vec(); // should be all ids
                    let mut args_iter  = args_exprs.into_iter();
                    let (mut arg_f, mut rest_args_f) = expr.get_list_pair();
                    loop {
                        let arg_name_opt = args_iter.next();
                        match (arg_name_opt, arg_f) {
                            (Some(arg_name), _) => {
                                let arg_val = self.eval_expr(arg_f, loc_table);
                                new_loc_table.insert(arg_name.get_id(), arg_val);
                                if rest_args_f.is_list() {
                                    (arg_f, rest_args_f) = rest_args_f.get_list_pair();
                                } else {
                                    arg_f = &SExp::Nil;
                                }
                            },
                            (None, SExp::Nil) => {
                                break;
                            },
                            _ => {
                                panic!("ERROR: argumes arity don't match for {met}");
                            }
                        }

                    }
                }
                // FIXME: error due to local table clearing
                // pretty_print_list(&func, 0);
                // println!("Exec func '{met}  with locals {loc_table:?}");
                let result = self.eval_expr(&func, &mut new_loc_table);
                // println!("Exec func '{met}' with locals {loc_table:?} -> ret {result:?}");
                return result;
            }
            _ => {
                panic!("Unknown {id:?}");
            }
        };
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

    let mut env = EvalEnvironment::new();

    println!("Expressions:");
    for expr in parser.sexprs.iter() {
        // println!("\n-> {expr:?}\n");
        let mut loc_table: EnvTable = EvalEnvironment::new_env_table();
        let result = env.eval_expr(expr, &mut loc_table);
        println!("Result: {result:?}");
        // println!("ENV: {env:?}");
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
