#![allow(dead_code)]

use anyhow::{anyhow, Context, Result, bail};
use std::cmp::{Eq, PartialEq};
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;
use std::hash::{Hasher, Hash};

const SKIP_SYMS: [char; 5] = ['\n', '\r', ' ', '\t', '#'];
const OPS: [char; 8] = ['+', '-', '=', '*', '/', '%', '>', '<'];

#[derive(Debug, Clone)]
pub struct F64 {
    value: f64,
}

impl F64 {
    fn new(value: f64) -> Self {
        return Self { value: value }
    }

    fn from_int(value: i64) -> Self {
        Self::new(value as f64)
    }
}

// from https://docs.rs/eq-float/0.1.0/src/eq_float/lib.rs.html
impl Hash for F64 {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        if self.value.is_nan() {
            0x7fc00000u32.hash(hasher); // a particular bit representation for NAN
        } else if self.value == 0.0 { // catches both positive and negative zero
            0u32.hash(hasher);
        } else {
            self.value.to_bits().hash(hasher);
        }
    }
}
impl PartialEq for F64 {
    fn eq(&self, other: &Self) -> bool {
        if self.value.is_nan() && other.value.is_nan() {
            true
        } else {
            self.value == other.value
        }
    }
}

impl Eq for F64 {}

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

    fn peek(&self) -> Option<&char> {
        self.input.first()
    }

    fn chop(&mut self) {
        if !self.input.is_empty() {
            self.input = self.input.drain(1..).collect();
        }
    }

    fn chop_while(&mut self, f: impl Fn(&char) -> bool) -> String {
        let mut result: Vec<char> = Vec::new();
        while let Some(nxt) = self.peek() {
            if f(nxt) {
                result.push(nxt.clone());
                self.chop();
            } else {
                break;
            }
        }
        let s: String = result.iter().collect();
        return String::from(s.trim());
    }

    pub fn parse(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(sym) = self.peek() {
            match sym {
                '(' => {
                    self.chop();
                    tokens.push(Token::LPar);
                }
                ')' => {
                    self.chop();
                    tokens.push(Token::RPar);
                }
                s if s.is_alphabetic() => {
                    let token = self
                        .chop_while(|c| c.is_alphanumeric() || *c == '?' || *c == '-' || *c == '_');
                    tokens.push(Token::Sym(token))
                }
                s if s.is_numeric() => {
                    let token = self.chop_while(|c| c.is_numeric() || *c == '.');
                    tokens.push(Token::Num(token));
                }
                s if SKIP_SYMS.contains(&s) => {
                    self.chop();
                }
                s if OPS.contains(&s) => {
                    tokens.push(Token::Op(self.chop_while(|ch| *ch != ' ')));
                }
                '"' => {
                    self.chop();
                    tokens.push(Token::Str(self.chop_while(|ch| *ch != '"')));
                    self.chop();
                }
                ';' => {
                    self.chop();
                    self.chop_while(|ch| *ch != '\n');
                    self.chop();
                }
                any => {
                    return Err(anyhow!("Unknown symbol {any}"));
                }
            };
        }
        return Ok(tokens);
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

    fn do_mat(&self, lhs: i64, rhs: i64) -> i64 {
        match self {
            Self::Add => lhs + rhs,
            Self::Sub => lhs - rhs,
            Self::Mul => lhs * rhs,
            Self::Div => lhs / rhs,
            Self::Mod => lhs % rhs,
        }
    }

    fn do_fmat(&self, lhs: f64, rhs: f64) -> f64 {
        match self {
            Self::Add => lhs + rhs,
            Self::Sub => lhs - rhs,
            Self::Mul => lhs * rhs,
            Self::Div => lhs / rhs,
            Self::Mod => lhs % rhs,
        }
    }
}

impl Display for EvMat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
        };
        write!(f, "{}", str)
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

impl Display for EvCmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Eq => "=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::GtEq => ">=",
            Self::LtEq => "<=",
        };
        write!(f, "{}", str)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum SExp {
    Lambda(i32),
    Id(String),
    Op(EvMat),
    Cmp(EvCmp),
    Sym(String),
    Num(i64),
    FNum(F64),
    Bool(bool),
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

    fn new_pair(car: SExp, cdr: SExp) -> SExp {
        return SExp::Cons {
            car: Box::new(car),
            cdr: Box::new(cdr),
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

    fn is_num(&self) -> bool {
        match self {
            Self::Num(_) => true,
            _ => false,
        }
    }

    fn is_fnum(&self) -> bool {
        match self {
            Self::FNum(_) => true,
            _ => false,
        }
    }

    fn get_num(&self) -> i64 {
        match self {
            Self::Num(num) => *num,
            _ => {
                panic!("ERROR: cant't get num from {self:?}");
            }
        }
    }

    fn get_fnum(&self) -> f64 {
        match self {
            Self::FNum(num) => num.value,
            Self::Num(num) => *num as f64,
            _ => {
                panic!("ERROR: cant't get fnum from {self:?}");
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
                panic!("ERROR: append expected list type but got {self:?}");
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
                panic!("ERROR: 'cdr' expects list type but got {self:?}");
            }
        }
    }

    fn get_car(&self) -> &SExp {
        match self {
            Self::Cons { car, cdr: _ } => car,
            _ => {
                panic!("ERROR: 'car'expects list type but got {self:?}");
            }
        }
    }

    fn get_list_pair(&self) -> (&SExp, &SExp) {
        match self {
            Self::Cons { car, cdr } => (car, cdr),
            _ => {
                panic!("ERROR: 'list_pair' expected list type but got {self:?}");
            }
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Self::Id(id) => id.to_string(),
            Self::Lambda(_) => "lambda".to_string(),
            Self::Num(num) => num.to_string(),
            Self::FNum(num) => num.value.to_string(),
            Self::Sym(str) => str.clone(),
            Self::Nil => String::from("()"),
            Self::Op(op) => {
                format!("{}", op)
            }
            Self::Cmp(cmp) => {
                format!("{}", cmp)
            }
            Self::Bool(b) => b.to_string(),
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
            (Self::FNum(lhs), Self::Num(rhs)) => cmp_op.do_cmp(lhs.value, *rhs as f64),
            (Self::Num(lhs), Self::FNum(rhs)) => cmp_op.do_cmp(*lhs as f64, rhs.value),
            (Self::FNum(lhs), Self::FNum(rhs)) => cmp_op.do_cmp(lhs.value, rhs.value),
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

impl Display for SExp {
    // Duplicated because of string iterpolation
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sexp_str = match self {
            Self::Id(id) => id.to_string(),
            Self::Lambda(_) => "lambda".to_string(),
            Self::Num(num) => num.to_string(),
            Self::FNum(num) => num.value.to_string(),
            Self::Sym(str) => format!("'{}'", str),
            Self::Nil => String::from("()"),
            Self::Op(op) => {
                format!("{}", op)
            }
            Self::Cmp(cmp) => {
                format!("{}", cmp)
            }
            Self::Bool(b) => b.to_string(),
            lst if lst.is_list() => {
                String::from("(")
                    + lst
                        .get_list_vec()
                        .iter()
                        .map(|l| format!("{}", l))
                        .collect::<Vec<_>>()
                        .join(" ")
                        .as_str()
                    + String::from(")").as_str()
            }
            _ => {
                panic!("not stringable {self:?}")
            }
        };
        write!(f, "{}", sexp_str)
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    sexprs: Vec<SExp>,
    lambda_count: i32, // should be global
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        return Parser {
            tokens,
            sexprs: Vec::new(),
            lambda_count: 0,
        };
    }

    fn drop_tokens(&mut self, n: usize) {
        self.tokens = self.tokens.drain(n..).collect();
    }

    fn peek_token(&self) -> Token {
        (self.tokens[0]).clone()
    }

    pub fn parse_tokens(&mut self) -> Result<()> {
        let mut stack: Vec<SExp> = Vec::new();

        while !self.tokens.is_empty() {
            match self.peek_token() {
                Token::LPar => {
                    self.drop_tokens(1);
                    loop {
                        if self.peek_token() == Token::LPar {
                            self.drop_tokens(1);
                            stack.push(SExp::Nil);
                        }
                        break;
                    }
                    let token = self.peek_token();
                    stack.push(SExp::new_list(self.parse_sexp(&token)));
                    self.drop_tokens(1);
                }
                Token::RPar => {
                    self.drop_tokens(1);
                    if stack.len() == 1 {
                        self.sexprs.push(stack.pop().context("Invalid expression")?);
                    } else {
                        let curr_sexp = stack.pop().context("Invalid expression")?;
                        let parent = stack.pop().context("Invalid expression")?;
                        let new_list = parent.append_to_list(curr_sexp);
                        stack.push(new_list);
                    }
                }
                Token::Sym(_) | Token::Str(_) | Token::Op(_) | Token::Num(_) => {
                    let token = self.peek_token();
                    let curr_sexp = stack.pop().context("Invalid expression")?;
                    let new_list = curr_sexp.append_to_list(self.parse_sexp(&token));
                    stack.push(new_list);
                    self.drop_tokens(1);
                }
            }
        }

        if stack.is_empty() {
            Ok(())
        } else {
            return Err(anyhow!("Can't parse expression"));
        }
    }

    pub fn expressions_iterator(&self) -> std::slice::Iter<SExp> {
        return self.sexprs.iter();
    }

    fn parse_sexp(&mut self, token: &Token) -> SExp {
        match token {
            Token::Sym(str) => match str.as_str() {
                "lambda" => SExp::Lambda(self.next_lambda_id()),
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
            Token::Num(str) => {
                if str.contains(".") {
                    SExp::FNum(F64::new(String::from(str).parse::<f64>().unwrap()))
                } else {
                    SExp::Num(String::from(str).parse::<i64>().unwrap())
                }
            },
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


type EnvTable = HashMap<SExp, SExp>;
type LamTable = HashMap<SExp, SExp>;
#[derive(Debug)]
pub struct LocalEnv<'a> {
    loc_env: EnvTable,
    lam_env: LamTable,
    met_table: EnvTable,
    parent_env: Option<Rc<&'a LocalEnv<'a>>>,
    tail_call: bool,
}

impl<'a> LocalEnv<'a> {
    pub fn new() -> Self {
        return LocalEnv {
            loc_env: EnvTable::new(),
            lam_env: LamTable::new(),
            met_table: EnvTable::new(),
            parent_env: None,
            tail_call: false,
        };
    }

    fn merge(&mut self, other: LocalEnv) {
        self.loc_env.extend(other.loc_env);
        self.lam_env.extend(other.lam_env);
        self.met_table.extend(other.met_table);
    }

    fn set_parent(&mut self, parent: &'a Self) {
        self.parent_env = Some(Rc::new(parent))
    }

    fn lookup_env<'b>(&self, f: impl Fn(&LocalEnv) -> bool) -> Option<&'b LocalEnv> {
        if f(&self) {
            return Some(&self)
        }

        let mut parent_opt = &self.parent_env;
        while parent_opt.is_some() {
            match parent_opt {
                Some(parent) => {
                    if f(parent) {
                        return Some(parent);
                    }
                    parent_opt = &parent.parent_env;
                }
                _ => return None
            }
        }
        None
    }

    fn has_var(&self, name: &SExp) -> bool {
        self.lookup_env(|env| env.loc_env.contains_key(name) ).is_some()
    }

    fn has_met(&self, name: &SExp) -> bool {
        self.lookup_env(|env| env.met_table.contains_key(name) ).is_some()
    }

    fn get_var(&self, name: &SExp) -> &SExp {
        match self.lookup_env(|env| env.loc_env.contains_key(name) ) {
            Some(env) => env.loc_env.get(name).unwrap(),
            None => panic!("Var {name} is not found")
        }
    }

    fn get_met(&self, name: &SExp) -> &SExp {
        match self.lookup_env(|env| env.met_table.contains_key(name) ) {
            Some(env) => env.met_table.get(name).unwrap(),
            None => panic!("Method {name} is not found")
        }
    }
}

#[derive(Debug)]
pub struct EvalEnvironment {
}

impl EvalEnvironment {
    pub fn new() -> Self {
        return EvalEnvironment {};
    }

    fn eval_lambda(&mut self, lambda: &SExp, rhs: &SExp, loc_env: &mut LocalEnv) -> SExp {
        match lambda.get_cdr() {
            SExp::Cons {
                car: args,
                cdr: body,
            } => {
                if let Some(capt) =loc_env.lam_env.get(lambda) {
                    debug_log!("lookup in env: {capt:?}");
                    for e in capt.get_list_vec().iter() {
                        loc_env
                            .loc_env
                            .insert(e.get_car().clone(), e.get_cdr().clone());
                    }

                }
                let args_exprs = args.get_list_vec();
                let mut args_iter = args_exprs.into_iter();
                let mut rhs_ptr = rhs;

                loop {
                    match args_iter.next() {
                        Some(arg_name) if rhs_ptr.is_list() => {
                            let call_arg = rhs_ptr.get_car();
                            let arg_val = self.eval_expr(call_arg, loc_env);
                            debug_log!(
                                "EvLam: Arg {arg_name} -> {val}",
                                arg_name = arg_name.get_id(),
                                val = arg_val
                            );
                            loc_env.loc_env.insert(arg_name, arg_val.clone());
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
                panic!("Not Lambda {lambda}")
            }
        }
    }

    pub fn eval_expr(&mut self, expr: &SExp, loc_env: &mut LocalEnv) -> SExp {
        if expr.is_list() || expr.is_nil() || expr.is_lambda() {
            debug_log!("Eval: {expr:?}");
        }
        match expr {
            SExp::Cons { car, cdr } => match car.as_ref() {
                SExp::Id(_) => self.eval_func(car, cdr, None, loc_env),
                SExp::Op(s) => self.eval_mat(s, cdr, None, loc_env),
                SExp::Cmp(s) => self.eval_cmp(s, cdr, None, loc_env),
                SExp::Num(num) => SExp::Num(*num),
                SExp::FNum(num) => SExp::FNum(num.clone()),
                SExp::Sym(sym) => SExp::Sym(sym.to_string()),
                SExp::Bool(b) => SExp::Bool(*b),
                SExp::Lambda(_) => { expr.clone() } // don't remember why
                SExp::Cons { car: lam, cdr: _ } if lam.is_lambda() => {
                    self.eval_lambda(car, &cdr, loc_env)
                }
                SExp::Nil => SExp::Nil,
                _ => {
                    panic!("ERROR: can't match eval expr car: {car:?}");
                }
            },
            SExp::Id(_) => self.eval_func(expr, &SExp::Nil, None, loc_env),
            SExp::Num(num) => SExp::Num(*num),
            SExp::FNum(num) => SExp::FNum(num.clone()),
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
        acc: Option<SExp>,
        loc_env: &mut LocalEnv,
    ) -> SExp {
        match expr {
            SExp::Nil => acc.unwrap(),
            _ => {
                let new_acc = match self.eval_expr(expr.get_car(), loc_env) {
                    SExp::Num(lhs) => {
                        acc.map(|v| {
                            if v.is_fnum() {
                                SExp::FNum(F64::new(op.do_fmat(v.get_fnum(), lhs as f64)))
                            } else {
                                SExp::Num(op.do_mat(v.get_num(), lhs))
                            }
                        }).or(Some(SExp::Num(lhs)))
                    },
                    SExp::FNum(lhs) => {
                        acc.map(|v| SExp::FNum(F64::new(op.do_fmat(v.get_fnum(), lhs.value)))).or(Some(SExp::FNum(lhs)))
                    },
                    _ => {
                        panic!("Math on not numeric type {expr:?}");
                    }
                };
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
        debug_log!("Evaling exp {expr}");
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
                        panic!("nil? takes 1 arg")
                    }
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_env);
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
                        panic!("Can't parse 'nil?' {expr:?}");
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
            "not" => {
                match expr {
                    SExp::Nil => {
                        panic!("'not' expects 1 argument")
                    }
                    SExp::Cons { car, cdr } if cdr.is_nil() => {
                        let mut lhs = self.eval_expr(car, loc_env);
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_env);
                        }
                        match lhs {
                            SExp::Bool(b) => return SExp::Bool(!b),
                            _ => {
                                panic!("'not' expects bool arg: {expr:?}");
                            }
                        }
                    }
                    _ => {
                        panic!("Can't parse 'not' {expr:?}");
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
                            return SExp::Num(lhs.get_list_vec().len() as i64);
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
                        while lhs.is_id() {
                            lhs = self.eval_expr(&lhs, loc_env);
                        }
                        if lhs.is_list() && !lhs.is_nil() {
                            return lhs.get_car().clone();
                        } else {
                            println!("env: {le:?}", le=loc_env.loc_env);
                            panic!("Car works only on lists {expr:?}");
                        }
                    }
                    _ => {
                        panic!("Can't parse 'car' {expr:?}");
                    }
                };
            }
            "conj" => {
                match expr {
                    SExp::Nil => return acc.unwrap(),
                    SExp::Cons { car, cdr } => {
                        let len = expr.get_list_vec().len();
                        if len > 2 {
                            panic!("Conj takes only 2 arguments: {expr:?}");
                        }

                        let first = self.eval_expr(car, loc_env);
                        // we know there are only two args, so seconds
                        // only should have car
                        let second = self.eval_expr(cdr.get_car(), loc_env);
                        if second.is_list() || second.is_nil() {
                            return second.append_to_list(first);
                        } else {
                            return SExp::new_list(second).append_to_list(first);
                        }
                    }
                    _ => {
                        panic!("Can't parse 'cons' {expr:?}");
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
                    loc_env.loc_env.insert(car.as_ref().clone(), var);
                    return SExp::Nil;
                }
                SExp::Cons { car, cdr } if car.is_list() => {
                    let (name, args) = car.get_list_pair();
                    let body = cdr.get_car();
                    loc_env.met_table.insert(name.clone(), SExp::new_pair(body.clone(), args.clone()).clone());
                    return SExp::Nil;
                }
                _ => {
                    panic!("ERROR: invalid define {expr:?}");
                }
            },
            var if loc_env.has_var(id) => {
                let val = loc_env.get_var(id).clone();
                debug_log!("glob var: {var} -> {val}");
                // Eval lambda only when it has call arguments
                if val.is_list() && val.get_car().is_lambda() && !expr.is_nil() {
                    return self.eval_lambda(&val, &expr, loc_env);
                }
                return val;
            }
            met if loc_env.has_met(id) => {
                let met_func_args = loc_env.get_met(id).clone();
                let (func, args) = met_func_args.get_list_pair();
                let mut new_loc_env = LocalEnv::new();

                // TODO: detect cyclic references
                // TODO: top level lambda bubleup its arg, fix
                if  !args.is_nil() {
                    debug_log!("args: {args}");
                    let args_exprs = args.get_list_vec(); // should be all ids
                    let mut args_iter = args_exprs.into_iter();
                    let mut expr_p = expr;
                    loop {
                        match args_iter.next() {
                            Some(arg_name) if expr_p.is_list() => {
                                let call_arg = expr_p.get_car();
                                debug_log!("arg: {arg_name} -> {call_arg}");

                                // if function arg is another function pass it next env
                                if loc_env.has_met(call_arg) {
                                    let loc_met = loc_env.get_met(call_arg).clone();
                                    new_loc_env.met_table.insert(arg_name, loc_met.clone());
                                    expr_p = expr_p.get_cdr();
                                    continue;
                                }

                                let arg_val = self.eval_expr(call_arg, loc_env);
                                debug_log!("arg evaled: {arg_name} -> {arg_val}");

                                if func.get_car().is_lambda() {
                                    debug_log!("^ arg for lambda");
                                    if loc_env.lam_env.contains_key(&func) {
                                        let new_list = new_loc_env
                                            .lam_env
                                            .remove(&func)
                                            .unwrap()
                                            .append_to_list(SExp::new_pair(arg_name, arg_val));
                                        loc_env.lam_env.insert(func.clone(), new_list);
                                    } else {
                                        loc_env.lam_env.insert(
                                            func.clone(),
                                            SExp::new_list(SExp::new_pair(arg_name, arg_val)),
                                        );
                                    }
                                } else {
                                    new_loc_env.loc_env.insert(arg_name, arg_val);
                                }
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

                if func.get_car().is_lambda() {
                    debug_log!("new loc_env: {new_loc_env:?}");
                }

                if met.ends_with("-tc") {
                    if loc_env.tail_call {
                        loc_env.merge(new_loc_env);
                        return id.clone();
                    } else {
                        // todo: detect tail call implicitly
                        // println!("func: {func:#?}");
                        new_loc_env.tail_call = true;
                        new_loc_env.set_parent(&loc_env);
                        return self.eval_tailcall(id, &func, &mut new_loc_env);
                    };
                } else {
                    new_loc_env.set_parent(&loc_env);
                    return self.eval_expr(&func, &mut new_loc_env);
                }

            }
            _ => {
                panic!("Unknown {id:?}");
            }
        };
    }

    fn eval_tailcall(&mut self, id: &SExp, body: &SExp, loc_env: &mut LocalEnv) -> SExp {
        loop {
            match self.eval_expr(body, loc_env) {
                rec_call if rec_call == *id => {},
                result => { return result },
            };
        };
    }
}

pub fn execute_source(env: &mut EvalEnvironment, loc_env: &mut LocalEnv, content: String, f: impl Fn(SExp) -> () ) -> Result<()> {
    let chars = content.chars().into_iter().collect::<Vec<_>>();
    let mut lexer = Lexer::new(chars);
    let tokens = match lexer.parse().context("Tokenization error") {
        Ok(t) => t,
        err => bail!("Tokenization failed: {err:?}"),
    };
    let mut parser = Parser::new(tokens);
    match parser.parse_tokens() {
        Ok(t) => t,
        err => bail!("AST parsing failed: {err:?}"),
    }

    for expr in parser.expressions_iterator() {
        debug_log!("\n-> {expr}\n");
        f(env.eval_expr(expr, loc_env));
        debug_log!("ENV: {loc_env:?}");
    }

    Ok(())
}
