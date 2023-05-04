use jop::*;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

fn main() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    let mut env = EvalEnvironment::new();

    loop {
        let readline = rl.readline("jop=> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                execute_line(&mut env, line);
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    Ok(())
}


fn execute_line(env: &mut EvalEnvironment, line: String) {
    let chars = line.chars().into_iter().collect::<Vec<_>>();
    let mut lexer = Lexer::new(chars);
    let tokens = lexer.parse();
    debug_log!("Tokens: #{tokens:?}");
    debug_log!("------------");
    let mut parser = Parser::new(tokens);
    parser.parse_tokens();

    debug_log!("Expressions:");
    for expr in parser.expressions_iterator() {
        debug_log!("\n-> {expr:?}\n");
        let mut loc_env: LocalEnv = LocalEnv::new();
        let result = env.eval_expr(expr, &mut loc_env);
        println!("{res}", res=result.as_string());
        debug_log!("ENV: {env:?}");
    }
}
