use jop::*;

use std::cmp::{Eq, PartialEq};
use std::collections::HashMap;
use std::fs;
use std::env;


fn main() {
    let args = std::env::args();
    let arguments = args.collect::<Vec<_>>();

    let file_path = &arguments[1];
    println!("Reading '{file_path}'");
    let content = fs::read_to_string(file_path).expect("Cant read file content");
    println!(
        "Source:\n--------\n{content}\n----------",
        content = content.trim()
    );
    let chars = content.chars().into_iter().collect::<Vec<_>>();
    let mut lexer = Lexer::new(chars);
    let tokens = lexer.parse();
    debug_log!("Tokens: #{tokens:?}");
    debug_log!("------------");
    let mut parser = Parser::new(tokens);
    parser.parse_tokens();

    let mut env = EvalEnvironment::new();

    println!("Expressions:");
    for expr in parser.expressions_iterator() {
        debug_log!("\n-> {expr:?}\n");
        let mut loc_env: LocalEnv = LocalEnv::new();
        let result = env.eval_expr(expr, &mut loc_env);
        println!("{v}", v=format!("Result: {result:?}"));
        debug_log!("ENV: {env:?}");
    }
}

