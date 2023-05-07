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

use std::cell::RefCell;

#[wasm_bindgen]
pub fn eval_jop(content: &str) -> String {
    let mut env = EvalEnvironment::new();
    let mut loc_env: LocalEnv = LocalEnv::new();
    let results: RefCell<Vec<String>> = RefCell::new(Vec::new());

    execute_source(&mut env, &mut loc_env, content.to_string(), |result| {
        log(result.as_string().as_str());
        let mut bw = results.borrow_mut();
        bw.push(result.as_string());
    }).unwrap();

    let bw = results.borrow();
    return bw.join("\n")
}

