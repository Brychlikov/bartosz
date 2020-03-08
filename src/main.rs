use calculator::eval::Calculator;
use rustyline::Editor;
use rustyline::error::ReadlineError;

use error_chain::ChainedError;

fn main() {
    env_logger::init();
    let mut calc = Calculator::new();

    let mut rl = Editor::<()>::new();
    loop {
        match rl.readline("> ") {
            Ok(s) if s.chars().all(|c| c.is_whitespace()) => (),
            Ok(input) => {
                rl.add_history_entry(&input);
                match calc.eval(&input) {
                    Ok(x) => {
                        // println!("{:?}", ast);
                        // println!("{:?}", &environment);
                            println!("{}\n", x)
                    }
                    Err(e) => println!("{}\n", e.display_chain()),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(e) => {
                println!("Error reading input: {}\n", e);
            }
        }
    }
}



