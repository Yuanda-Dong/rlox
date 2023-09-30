pub mod token_type;
pub mod scanner;
pub mod expr;
pub mod ast_printer;
pub mod parser;
pub mod interpreter;

use std::{env, process::exit, fs, error::Error, io::{self, Write}};

use interpreter::Eval;
use parser::Parser;
use scanner::Scanner;

pub struct Lox{
    had_error: bool
}

fn run(program: &str, state: &mut Lox){
    let mut scanner = Scanner::new(program.to_string(), state);
    scanner.scan_tokens();
    let tokens = scanner.tokens;
    let mut parser = Parser::new(tokens);

    match parser.expression(){
        Ok(x) => {
            match x.evaluate(){
                Ok(y) => println!("{}",y),
                Err(y) => state.error(y),
            }
        }
        Err(x) => state.error(x),
    }


}

fn run_file(state: &mut Lox, path: &str) -> io::Result<()>{
    let program = fs::read_to_string(path)?;
    run(&program, state);
    if state.had_error{
        exit(65)
    }
    Ok(())
}

fn run_prompt(state: &mut Lox) -> io::Result<()>{
    let handle = io::stdin();
    let mut ohandle = io::stdout();

    loop{
        print!("> ");
        ohandle.flush()?;
        let mut line = String::new();
        let num_bytes = handle.read_line(&mut line)?;
        if num_bytes == 0 {
            return Ok(())
        }
        run(&line, state);
        state.had_error = false;
    }
}

impl Lox {
    pub fn error(&mut self,message: String){
        println!("{}",message);
        self.had_error = true;
    }
}


fn main() -> Result<(),Box<dyn Error>>{
    let mut state = Lox{
        had_error: false
    };
    let args: Vec<String> = env::args().skip(1).collect();
    match args.len(){
        0 => {
            run_prompt(&mut state)
        },
        1 => {
            run_file(&mut state, args.get(0).unwrap())
        }
        _ => {
            println!("Usage: clox [script]");
            exit(64);
        }
    }?;
    Ok(())
}
