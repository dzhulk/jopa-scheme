use jop::*;

use anyhow::{Result};
use std::fs;

fn main() -> Result<()> {
    let mut env = EvalEnvironment::new();
    let mut loc_env: LocalEnv = LocalEnv::new();

    let stdlib_src = fs::read_to_string("examples/stdlib.scm").expect("Cant read stdlib");
    execute_source(&mut env, &mut loc_env, stdlib_src, |_| {}).unwrap();

    let args = std::env::args();
    let arguments = args.collect::<Vec<_>>();
    let file_path = &arguments[1];

    println!("Reading '{file_path}'");
    let content = fs::read_to_string(file_path).expect("Cant read file content");
    println!(
        "Source:\n--------\n{content}\n----------",
        content = content.trim()
    );
    execute_source(&mut env, &mut loc_env, content, |result| {
        println!("{res}", res = result.as_string())
    }).unwrap();

    Ok(())
}



