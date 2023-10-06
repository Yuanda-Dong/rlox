pub mod ast_printer;
pub mod environment;
pub mod expr;
pub mod interpreter;
pub mod lox_errors;
pub mod parser;
pub mod scanner;
pub mod token_type;

use std::{
    env, fs,
    io::{self, Write},
    process::exit,
};

use environment::Environment;
use interpreter::Exe;
use lox_errors::LoxError;
use parser::Parser;
use scanner::Scanner;

pub struct Lox {
    scanner: Scanner,
    parser: Parser,
    environment: Environment,
}

impl Lox {
    fn run(&mut self, program: String) -> Result<(), LoxError> {
        let tokens = self.scanner.scan_tokens(program)?;
        let statements = self.parser.parse(tokens);
        for stmt in statements {
            stmt?.execuate(&mut self.environment)?;
        }
        Ok(())
    }

    fn run_file(&mut self, path: &str) -> io::Result<()> {
        let program = fs::read_to_string(path)?;
        match self.run(program) {
            Ok(_) => Ok(()),
            Err(x) => {
                println!("{}", x);
                println!("exiting...");
                exit(65)
            }
        }
    }

    fn run_prompt(&mut self) -> io::Result<()> {
        let handle = io::stdin();
        let mut ohandle = io::stdout();
        loop {
            print!("> ");
            ohandle.flush()?;
            let mut line = String::new();
            let num_bytes = handle.read_line(&mut line)?;
            if num_bytes == 0 {
                return Ok(());
            }
            let _ = self.run(line).map_err(|e| println!("{}", e));
        }
    }
}

fn main() -> io::Result<()> {
    let scanner = Scanner::new();
    let parser = Parser::new();
    let environment = Environment::new(None);
    let mut lox = Lox {
        scanner,
        parser,
        environment,
    };
    let args: Vec<String> = env::args().skip(1).collect();
    match args.len() {
        0 => lox.run_prompt(),
        1 => lox.run_file(args.get(0).unwrap()),
        _ => {
            println!("Usage: rlox [script]");
            exit(64);
        }
    }?;
    Ok(())
}
