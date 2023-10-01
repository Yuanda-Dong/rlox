pub mod ast_printer;
pub mod expr;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod token_type;
pub mod lox_errors;

use std::{
    env,
    fs,
    io::{self, Write},
    process::exit,
};

use lox_errors::LoxError;
use parser::Parser;
use scanner::Scanner;

use crate::interpreter::Eval;

pub struct Lox {}

impl Lox {

    fn run(&mut self, program: String) -> Result<(),LoxError>{
        let mut scanner = Scanner::new();
        let mut parser = Parser::new();
        let tokens = scanner.scan_tokens(program)?;
        let expr = parser.parse(tokens)?;
        println!("{}",expr.evaluate()?);
        Ok(())
    }

    fn run_file(&mut self, path: &str) -> io::Result<()> {
        let program = fs::read_to_string(path)?;
        match self.run(program){
            Ok(_) =>Ok(()),
            Err(_) => {
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
            let _ = self.run(line).map_err(|e| println!("{}",e));
        }
    }
}

fn main() -> io::Result<()> {
    let mut state = Lox {};
    let args: Vec<String> = env::args().skip(1).collect();
    match args.len() {
        0 => state.run_prompt(),
        1 => state.run_file(args.get(0).unwrap()),
        _ => {
            println!("Usage: clox [script]");
            exit(64);
        }
    }?;
    Ok(())
}
