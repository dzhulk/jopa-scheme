#![allow(dead_code)]

use std::cmp::{Eq, PartialEq};
use std::collections::HashMap;


const SKIP_SYMS: [char; 5] = ['\n', '\r', ' ', '\t', '#'];
const OPS: [char; 8] = ['+', '-', '=', '*', '/', '%', '>', '<'];

#[macro_export]
macro_rules! debug_log {
    ($($arg:tt)*)=> {
        match std::env::var("JOPA_TRACE") {
            Ok(v) if v == "1" => println!($($arg)*),
            _ => {}
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    LPar,
    RPar,
    Num(String),
    Sym(String),
    Str(String),
    Op(String),
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
}

impl Lexer {
    pub fn new(chars: Vec<char>) -> Self {
        Lexer { input: chars }
    }

    fn peek(&self) -> Option<char> {
        if self.input.is_empty() {
            return None;
        } else {
            return Some(self.input[0]);
        }
    }

    fn next(&mut self) -> char {
        let chr = self.input[0];
        self.input = self.input.drain(1..).collect();
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

    pub fn parse(&mut self) -> Vec<Token> {
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
                    let token = self.chop_while(|c| c.is_alphanumeric() || c == '?');
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
                }
                ';' => {
                    self.next();
                    self.chop_while(|ch| ch != '\n');
                    self.next();
                }
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
pub enum EvMat {
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
            _ => None,
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
pub enum EvCmp {
    Lt,
    Gt,
    Eq,
    GtEq,
    LtEq,
}

impl EvCmp {
    fn from_string(s: &String) -> Option<Self> {
        match s.as_str() {
            "<" => Some(Self::Lt),
            ">" => Some(Self::Gt),
            "=" => Some(Self::Eq),
            ">=" => Some(Self::GtEq),
            "<=" => Some(Self::LtEq),
            _ => None,
        }
    }

    fn do_cmp<T>(&self, lhs: T, rhs: T) -> bool
    where
        T: PartialOrd + PartialEq,
    {
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
pub enum SExp {
    Lambda(i32),
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
            Self::Cons { car: _, cdr: _ } => true,
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

    fn is_lambda(&self) -> bool {
        match self {
            Self::Lambda(_) => true,
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
            return Self::Cons {
                car: Box::new(other),
                cdr: Box::new(self),
            };
        }
        panic!("ERROR: cant prepted to not list type {self:?}");
    }

    fn append_to_list(self, other: SExp) -> SExp {
        match self {
            Self::Cons { car, cdr } => {
                if *cdr == SExp::Nil {
                    Self::Cons {
                        car,
                        cdr: Box::new(Self::new_list(other)),
                    }
                } else {
                    Self::Cons {
                        car,
                        cdr: Box::new(cdr.append_to_list(other)),
                    }
                }
            }
            Self::Nil => Self::new_list(other),
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
            Self::Cons { car: _, cdr } => cdr,
            _ => {
                eprintln!("ERROR: expected list type");
                todo!();
            }
        }
    }

    fn get_car(&self) -> &SExp {
        match self {
            Self::Cons { car, cdr: _ } => car,
            _ => {
                eprintln!("ERROR: expected list type");
                todo!();
            }
        }
    }

    fn get_list_pair(&self) -> (&SExp, &SExp) {
        match self {
            Self::Cons { car, cdr } => (car, cdr),
            _ => {
                eprintln!("ERROR: expected list type");
                todo!();
            }
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Self::Id(id) => id.to_string(),
            Self::Num(num) => num.to_string(),
            Self::Sym(str) => str.clone(),
            Self::Nil => String::from("()"),
            Self::Bool(b) => {
                if *b {
                    String::from("true")
                } else {
                    String::from("false")
                }
            }
            lst if lst.is_list() => {
                return String::from("(")
                    + lst
                        .get_list_vec()
                        .iter()
                        .map(|l| l.as_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                        .as_str()
                    + String::from(")").as_str();
            }
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

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    sexprs: Vec<SExp>,
    stack: Vec<SExp>,
    lambda_count: i32, // should be global
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        return Parser {
            tokens,
            sexprs: Vec::new(),
            stack: Vec::new(),
            lambda_count: 0,
        };
    }

    fn drop_tokens(&mut self, n: usize) {
        self.tokens = self.tokens.drain(n..).collect();
    }

    fn peek_token(&self) -> Token {
        (self.tokens[0]).clone()
    }

    pub fn parse_tokens(&mut self) {
        let mut stack: Vec<SExp> = Vec::new();

        while !self.tokens.is_empty() {
            match self.peek_token() {
                Token::LPar => {
                    self.drop_tokens(1);
                    let mut token = self.peek_token();
                    if token == Token::LPar {
                        self.drop_tokens(1);
                        token = self.peek_token();
                        stack.push(SExp::Nil);
                    }
                    stack.push(SExp::new_list(self.parse_sexp(&token)));
                    self.drop_tokens(1);
                }
                Token::RPar => {
                    self.drop_tokens(1);
                    if stack.len() == 1 {
                        self.sexprs.push(stack.pop().unwrap());
                    } else {
                        let curr_sexp = stack.pop().unwrap();
                        let parent = stack.pop().unwrap();
                        let new_list = parent.append_to_list(curr_sexp);
                        stack.push(new_list);
                    }
                }
                Token::Sym(_) | Token::Str(_) | Token::Op(_) | Token::Num(_) => {
                    let token = self.peek_token();
                    let new_list = stack.pop().unwrap().append_to_list(self.parse_sexp(&token));
                    stack.push(new_list);
                    self.drop_tokens(1);
                }
            }
        }
    }

    pub fn expressions_iterator(&self) -> std::slice::Iter<SExp> {
        return self.sexprs.iter()
    }

    fn parse_sexp(&mut self, token: &Token) -> SExp {
        match token {
            Token::Sym(str) => match str.as_str() {
                "lambda" => SExp::Lambda(self.next_lambda_id()),
                "quote" => SExp::Quote,
                "nil" => SExp::Nil,
                "true" => SExp::Bool(true),
                "false" => SExp::Bool(false),
                _ => SExp::Id(String::from(str)),
            },
            Token::Str(str) => SExp::Sym(String::from(str)),
            Token::Op(str) => match EvMat::from_string(str) {
                Some(op) => SExp::Op(op),
                None => {
                    let cmp =
                        EvCmp::from_string(str).expect(format!("Can't parse Op {str}").as_str());
                    SExp::Cmp(cmp)
                }
            },
            Token::Num(str) => SExp::Num(String::from(str).parse::<i32>().unwrap()),
            any => {
                panic!("ERROR: Unknown token: {any:?}");
            }
        }
    }

    fn next_lambda_id(&mut self) -> i32 {
        self.lambda_count += 1;
        self.lambda_count
    }
}

type EnvTable = HashMap<String, SExp>;
#[derive(Debug)]
pub struct LocalEnv {
    loc_env: EnvTable,
    lam_env: EnvTable,
}

impl LocalEnv {
    pub fn new() -> Self {
        return LocalEnv {
            loc_env: EnvTable::new(),
            lam_env: EnvTable::new(),
        };
    }
}

#[derive(Debug)]
pub struct EvalEnvironment {
    arg_table: EnvTable,
    met_table: EnvTable,
    var_table: EnvTable,
}

impl EvalEnvironment {
    pub fn new() -> Self {
        return EvalEnvironment {
            arg_table: HashMap::new(),
            met_table: HashMap::new(),
            var_table: HashMap::new(),
        };
    }

    fn new_env_table() -> EnvTable {
        return HashMap::new();
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

    fn eval_lambda_proxy(&mut self, lambda: &SExp, rhs: &SExp, loc_env: &mut LocalEnv) -> SExp {
        match lambda {
            SExp::Cons {
                car: met_name,
                cdr: args,
            } => {
                // let name = met_name.get_id().as_ref();
                // println!("prox: {args:?}");
                // if self.has_met(name) {
                //      let args_exprs = self.get_var(name).get_list_vec();
                // }
                let args_exprs = args.get_list_vec();
                let mut args_iter = args_exprs.into_iter();
                let mut rhs_ptr = args.as_ref();
                loop {
                    match args_iter.next() {
                        Some(arg_name) if rhs_ptr.is_list() => {
                        let call_arg = rhs_ptr.get_car();
                            let arg_val = self.eval_expr(call_arg, loc_env);
                            println!("Arg {arg_name:?} -> {val:?}", val=arg_val);
                            rhs_ptr = rhs_ptr.get_cdr();
                        }
                        None if rhs_ptr.is_nil() => {
                            break;
                        }
                        _ => {
                            panic!("ERROR: argumes arity don't match for lambda");
                        }
                    }
                }
                return self.eval_expr(&SExp::Nil, loc_env);
            }
            _ => {
                panic!("Lambda error")
            }
        }
    }


    fn eval_lambda(&mut self, lambda: &SExp, rhs: &SExp, loc_env: &mut LocalEnv) -> SExp {
        match lambda.get_cdr() {
            SExp::Cons {
                car: args,
                cdr: body,
            } => {
                let args_exprs = args.get_list_vec();
                let mut args_iter = args_exprs.into_iter();
                let mut rhs_ptr = rhs;
                loop {
                    match args_iter.next() {
                        Some(arg_name) if rhs_ptr.is_list() => {
                            let call_arg = rhs_ptr.get_car();
                            let arg_val = self.eval_expr(call_arg, loc_env);
                            println!("Arg {arg_name:?} -> {val:?}", arg_name=arg_name.get_id(), val=arg_val);
                            loc_env.loc_env.insert(arg_name.get_id(), arg_val.clone());
                            rhs_ptr = rhs_ptr.get_cdr();
                        }
                        None if rhs_ptr.is_nil() => {
                            break;
                        }
                        _ => {
                            panic!("ERROR: argumes arity don't match for lambda");
                        }
                    }
                }
                return self.eval_expr(body.get_car(), loc_env);
            }
            _ => {
                panic!("Lambda error")
            }
        }
    }

    pub fn eval_expr(&mut self, expr: &SExp, loc_env: &mut LocalEnv) -> SExp {
        debug_log!("{expr:#?}");
        match expr {
            SExp::Cons { car, cdr } => match car.as_ref() {
                SExp::Id(_) => self.eval_func(car, cdr, None, loc_env),
                SExp::Op(s) => self.eval_mat(s, cdr, None, loc_env),
                SExp::Cmp(s) => self.eval_cmp(s, cdr, None, loc_env),
                SExp::Num(num) => SExp::Num(*num),
                SExp::Sym(sym) => SExp::Sym(sym.to_string()),
                SExp::Bool(b) => SExp::Bool(*b),
                // SExp::Lambda(_) => self.eval_lambda(expr, loc_env),
                SExp::Cons {
                    car: lam,
                    cdr: _,
                } if lam.is_lambda() => {
                    self.eval_lambda(car, &cdr, loc_env)
                },
                SExp::Cons {
                    car: id,
                    cdr: _,
                } if id.is_id() => {
                    self.eval_lambda_proxy(car, &cdr, loc_env)
                },
                SExp::Nil => SExp::Nil,
                _ => {
                    panic!("ERROR: can't match eval expr car: {car:?}");
                }
            },
            SExp::Id(_) => self.eval_func(expr, &SExp::Nil, None, loc_env),
            SExp::Num(num) => SExp::Num(*num),
            SExp::Sym(sym) => SExp::Sym(sym.to_string()),
            SExp::Bool(b) => SExp::Bool(*b),
            SExp::Nil => SExp::Nil,
            _ => {
                panic!("ERROR: can't match eval expr: {expr:?}");
            }
        }
    }

    fn eval_mat(
        &mut self,
        op: &EvMat,
        expr: &SExp,
        acc: Option<i32>,
        loc_env: &mut LocalEnv,
    ) -> SExp {
        match expr {
            SExp::Nil => SExp::Num(acc.unwrap()),
            _ => {
                let SExp::Num(lhs) = self.eval_expr(expr.get_car(), loc_env) else { panic!("Math on not Num type!") };
                let new_acc = acc.map(|v| op.do_mat(v, lhs)).or(Some(lhs));
                self.eval_mat(op, expr.get_cdr(), new_acc, loc_env)
            }
        }
    }
    fn eval_cmp(
        &mut self,
        op: &EvCmp,
        expr: &SExp,
        acc: Option<SExp>,
        loc_env: &mut LocalEnv,
    ) -> SExp {
        if acc.is_none() {
            // println!("Eval: {op:?} -> {expr:?}");
        }

        match expr {
            SExp::Nil => SExp::Bool(true),
            _ => {
                let lhs = self.eval_expr(expr.get_car(), loc_env);
                let next_acc = match acc {
                    Some(a) => {
                        if a.cmp(&lhs, op) {
                            lhs
                        } else {
                            return SExp::Bool(false);
                        }
                    }
                    None => lhs,
                };
                return self.eval_cmp(op, expr.get_cdr(), Some(next_acc), loc_env);
            }
        }
    }

    fn eval_func(
        &mut self,
        id: &SExp,
        expr: &SExp,
        acc: Option<SExp>,
        loc_env: &mut LocalEnv,
    ) -> SExp {
        // println!("Evaling exp {expr:?}");
        match id.get_id().as_str() {
            "list" => {
                match expr {
                    SExp::Nil => return acc.unwrap(),
                    SExp::Cons { car, cdr } => {
                        let res = self.eval_expr(car, loc_env);
                        let new_acc = acc
                            .map(|a| a.append_to_list(res.clone()))
                            .or(Some(SExp::new_list(res)));
                        return self.eval_func(id, cdr, new_acc, loc_env);
                    }
                    _ => {
                        panic!("Can't parse 'do' {expr:?}");
                    }
                };
            }
            "do" => {
                match expr {
                    SExp::Nil => return acc.unwrap(),
                    SExp::Cons { car, cdr } => {
                        let res = self.eval_expr(car, loc_env);
                        return self.eval_func(id, cdr, Some(res), loc_env);
                    }
                    _ => {
                        panic!("Can't parse 'do' {expr:?}");
                    }
                };
            }
            "if" => {
                match expr {
                    SExp::Nil => {
                        return acc.unwrap();
                    }
                    SExp::Cons { car, cdr } => {
                        let cond_res = self.eval_expr(car, loc_env);
                        let (true_branch, false_branch) = cdr.get_list_pair();
                        if cond_res.is_true() {
                            return self.eval_expr(true_branch, loc_env);
                        } else {
                            match false_branch {
                                SExp::Nil => return SExp::Nil,
                                _ => {
                                    return self.eval_expr(false_branch.get_car(), loc_env);
                                }
                            }
                        }
                    }
                    _ => {
                        panic!("Error in if cond {expr:?}");
                    }
                };
            }
            "or" => {
                match expr {
                    SExp::Nil => return SExp::Bool(false),
                    SExp::Cons { car, cdr } => {
                        let lhs = self.eval_expr(car, loc_env);
                        let new_acc = match acc {
                            Some(_) => {
                                if lhs.is_true() {
                                    return SExp::Bool(true);
                                } else {
                                    lhs
                                }
                            }
                            None => lhs,
                        };
                        return self.eval_func(id, cdr, Some(new_acc), loc_env);
                    }
                    _ => {
                        panic!("Not joinable {expr:?}");
                    }
                };
            }
            "and" => {
                match expr {
                    SExp::Nil => {
                        return acc.unwrap();
                    }
                    SExp::Cons { car, cdr } => {
                        let lhs = self.eval_expr(car, loc_env);
                        let new_acc = match acc {
                            Some(a) => {
                                if a.is_true() && lhs.is_true() {
                                    lhs
                                } else {
                                    return SExp::Bool(false);
                                }
                            }
                            None => lhs,
                        };
                        return self.eval_func(id, cdr, Some(new_acc), loc_env);
                    }
                    _ => {
                        panic!("Not joinable {expr:?}");
                    }
                };
            }
            "nil?" => {
                match expr {
                    SExp::Nil => {
                        panic!("Nil takes 1 arg")
                    }
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_env);
                        // println!("lhs: {lhs:?}");
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_env);
                        }
                        if lhs.is_nil() {
                            return SExp::Bool(true);
                        } else {
                            return SExp::Bool(false);
                        }
                    }
                    _ => {
                        panic!("Can't parse 'isnil' {expr:?}");
                    }
                };
            }
            "cdr" => {
                match expr {
                    SExp::Nil => {
                        panic!("CDR from empty list")
                    }
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_env);
                        // println!("lhs: {lhs:?}");
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_env);
                        }
                        if lhs.is_list() && !lhs.is_nil() {
                            return lhs.get_cdr().clone();
                        } else {
                            panic!("CDR works only on lists {expr:?}");
                        }
                    }
                    _ => {
                        panic!("Can't parse 'cdr' {expr:?}");
                    }
                };
            }
            "list?" => {
                match expr {
                    SExp::Nil => {
                        panic!("list? expects 1 argument")
                    }
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_env);
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_env);
                        }
                        return SExp::Bool(lhs.is_list() || lhs.is_nil());
                    }
                    _ => {
                        panic!("Can't parse 'list?' {expr:?}");
                    }
                };
            }
            "length" => {
                match expr {
                    SExp::Nil => {
                        panic!("LENGTH expects 1 argument")
                    }
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_env);
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_env);
                        }
                        if lhs.is_list() || lhs.is_nil() {
                            return SExp::Num(lhs.get_list_vec().len() as i32);
                        } else {
                            panic!("LENGTH works only on lists {expr:?}");
                        }
                    }
                    _ => {
                        panic!("Can't parse 'length' {expr:?}");
                    }
                };
            }
            "car" => {
                match expr {
                    SExp::Nil => {
                        panic!("Car from empty list")
                    }
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_env);
                        // println!("lhs: {lhs:?}");
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_env);
                        }
                        if lhs.is_list() && !lhs.is_nil() {
                            return lhs.get_car().clone();
                        } else {
                            panic!("Car works only on lists {expr:?}");
                        }
                    }
                    _ => {
                        panic!("Can't parse 'car' {expr:?}");
                    }
                };
            }
            "cons" => {
                match expr {
                    SExp::Nil => return acc.unwrap(),
                    SExp::Cons { car, cdr } => {
                        let len = expr.get_list_vec().len();
                        if len > 2 {
                            panic!("Cons takes only 2 arguments: {expr:?}");
                        }

                        let first = self.eval_expr(car, loc_env);
                        // we know there are only two args, so seconds
                        // only should have car
                        let second = self.eval_expr(cdr.get_car(), loc_env);
                        if second.is_list() || second.is_nil() {
                            return second.prepend_to_list(first);
                        } else {
                            return SExp::new_list(second).prepend_to_list(first);
                        }
                    }
                    _ => {
                        panic!("Can't parse 'cons' {expr:?}");
                    }
                };
            }
            "concat" => {
                match expr {
                    SExp::Nil => {
                        let output = acc
                            .map(|a| a.as_string())
                            .or(Some(String::from("")))
                            .unwrap();
                        return SExp::Sym(output);
                    }
                    SExp::Cons { car, cdr } => {
                        let lhs = self.eval_expr(car, loc_env);
                        let str = lhs.as_string();
                        let new_acc = acc
                            .map(|a| SExp::Sym(a.as_string() + " " + &str))
                            .or(Some(SExp::Sym(str)));
                        return self.eval_func(id, cdr, new_acc, loc_env);
                    }
                    _ => {
                        panic!("Not joinable {expr:?}");
                    }
                };
            }
            "println" => {
                match expr {
                    SExp::Nil => {
                        let output = acc
                            .map(|a| a.as_string())
                            .or(Some(String::from("")))
                            .unwrap();
                        println!("{output}");
                        return SExp::Nil;
                    }
                    SExp::Cons { car, cdr } => {
                        let mut lhs = self.eval_expr(car, loc_env);
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_env);
                        }
                        let str = lhs.as_string();
                        let new_acc = acc
                            .map(|a| SExp::Sym(a.as_string() + " " + &str))
                            .or(Some(SExp::Sym(str)));
                        return self.eval_func(id, cdr, new_acc, loc_env);
                    }
                    _ => {
                        panic!("Not printable {expr:?}");
                    }
                };
            }
            "define" => match expr {
                SExp::Cons { car, cdr } if car.is_id() => {
                    let var = self.eval_expr(cdr.get_car(), loc_env);
                    self.var_table.insert(car.get_id(), var);
                    return SExp::Nil;
                }
                SExp::Cons { car, cdr } if car.is_list() => {
                    let (name, args) = car.get_list_pair();
                    self.arg_table.insert(name.get_id(), args.clone());
                    let body = cdr.get_car();
                    self.met_table.insert(name.get_id(), body.clone());
                    return SExp::Nil;
                }
                _ => {
                    panic!("ERROR: invalid define {expr:?}");
                }
            },
            var if self.has_var(var) => {
                return self.get_var(var).clone();
            }
            var if loc_env.loc_env.contains_key(var) => {
                return loc_env.loc_env.get(var).unwrap().clone();
            }
            met if self.has_met(met) => {
                let func = self.get_met(met).clone();
                let mut new_loc_env = LocalEnv::new();
                if let Some(args) = self.get_args(met) {
                    let args_exprs = args.get_list_vec(); // should be all ids
                    let mut args_iter = args_exprs.into_iter();
                    let mut expr_p = expr;
                    // println!("Args {expr:?}");
                    loop {
                        match args_iter.next() {
                            Some(arg_name) if expr_p.is_list() => {
                                let call_arg = expr_p.get_car();
                                let arg_val = self.eval_expr(call_arg, loc_env);
                                // println!("Arg {arg_name:?} -> {val:?}", arg_name=arg_name.get_id(), val=arg_val);
                                new_loc_env.loc_env.insert(arg_name.get_id(), arg_val);
                                expr_p = expr_p.get_cdr();
                            }
                            None if expr_p.is_nil() => {
                                break;
                            }
                            _ => {
                                panic!("ERROR: argumes arity don't match for {met}");
                            }
                        }
                    }
                }
                // println!("Exec func '{met}  with locals {loc_env:?}");
                let result = self.eval_expr(&func, &mut new_loc_env);
                // println!("Exec func '{met}' with locals {loc_env:?} -> ret {result:?}");
                return result;
            }
            _ => {
                panic!("Unknown {id:?}");
            }
        };
    }
}

