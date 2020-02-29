use std::io::prelude::*;
use calculator::parser::{Parser, Calculator};

use error_chain::ChainedError;

fn main() {
    let mut calc = Calculator::new();
    loop {
        print!("\n> ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                match calc.eval(&input) {
                    Ok(x) => {
                        // println!("{:?}", ast);
                        // println!("{:?}", &environment);
                            println!("{}", x)
                    }
                    Err(e) => println!("{}", e.display_chain()),
                }
            }
            Err(e) => {
                println!("Error reading input: {}", e);
            }
        }
    }
}



