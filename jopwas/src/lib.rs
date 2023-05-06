use jop::*;
use anyhow::{bail, Context, Result};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn eval_jop(content: &str) -> String {
    let mut env = EvalEnvironment::new();
    // let content = r#"(concat "Hello")"#;
    let chars = content.chars().into_iter().collect::<Vec<_>>();
    let mut lexer = Lexer::new(chars);
    let tokens = match lexer.parse().context("Tokenization error") {
        Ok(t) => t,
        err => panic!("Tokenization failed: {err:?}"),
    };

    let mut parser = Parser::new(tokens);
    match parser.parse_tokens() {
        Ok(t) => t,
        err => panic!("AST parsing failed: {err:?}"),
    }

    debug_log!("Expressions:");
    let mut loc_env: LocalEnv = LocalEnv::new();
    let mut results: Vec<String> = Vec::new();
    for expr in parser.expressions_iterator() {
        debug_log!("\n-> {expr}\n");
        let result = env.eval_expr(expr, &mut loc_env);
        println!("{res}", res = result.as_string());
        debug_log!("ENV: {loc_env:?}");
        log(result.as_string().as_str());
        results.push(result.as_string());
    }
    return results.join("\n");
}

