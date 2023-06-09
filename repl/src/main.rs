use jop::*;

use anyhow::{Result};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::fs;

fn main() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    let mut env = EvalEnvironment::new();
    let mut loc_env: LocalEnv = LocalEnv::new();

    let stdlib_src = fs::read_to_string("examples/stdlib.scm").expect("Cant read stdlib");
    execute_source(&mut env, &mut loc_env, stdlib_src, |_| {}).unwrap();

    loop {
        let readline = rl.readline("jop=> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                let res = execute_line(&mut env, &mut loc_env, line);

                match res {
                    Err(err) => {
                        println!("Error occured:\n\t{err}")
                    }
                    _ => {}
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Bye!");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
    Ok(())
}

fn execute_line(env: &mut EvalEnvironment, loc_env: &mut LocalEnv, line: String) -> Result<()> {
    execute_source(env, loc_env, line, |result| {
        println!("{res}", res = result.as_string())
    })
}
