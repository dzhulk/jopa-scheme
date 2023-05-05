use jop::*;

use anyhow::{bail, Context, Result};
use std::fs;

fn main() -> Result<()> {
    let args = std::env::args();
    let arguments = args.collect::<Vec<_>>();
    let mut env = EvalEnvironment::new();

    let file_path = &arguments[1];
    println!("Reading '{file_path}'");
    let content = fs::read_to_string(file_path).expect("Cant read file content");

    println!(
        "Source:\n--------\n{content}\n----------",
        content = content.trim()
    );

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

    debug_log!("Expressions:");
    let mut loc_env: LocalEnv = LocalEnv::new();
    for expr in parser.expressions_iterator() {
        debug_log!("\n-> {expr}\n");
        let result = env.eval_expr(expr, &mut loc_env);
        println!("{res}", res = result.as_string());
        debug_log!("ENV: {loc_env:?}");
    }

    Ok(())
}
