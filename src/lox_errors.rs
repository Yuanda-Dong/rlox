use std::fmt::Display;

use crate::{token_type::Token, interpreter::Value};

pub enum LoxError{
    ResolveError(String), ParseError(String), ScanError(String), RunTimeError(String), ReturnValue(Value)
}

pub type LoxResult<T> = Result<T,LoxError>;
impl Display for LoxError{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            LoxError::ParseError(x) => write!(f,"ParseError {}",x),
            LoxError::ScanError(x) => write!(f,"ScanError {}",x),
            LoxError::RunTimeError(x) => write!(f,"RunTimeError {}",x),
            LoxError::ReturnValue(x) => write!(f,"{}",x),
            LoxError::ResolveError(x) => write!(f,"ResolveTimeError {}",x),
        }
    }
}

pub fn run_error(tk: &Token, message: &str) -> LoxError {
    LoxError::RunTimeError(format!("[line {}, {}] {}", tk.line, tk, message))
}

pub fn parse_error(tk: &Token, message: &str) -> LoxError {
    LoxError::ParseError(format!("[line {}, {}] {}", tk.line, tk, message))
}

pub fn scan_error(tk: &Token, message: &str) -> LoxError {
    LoxError::ScanError(format!("[line {}, {}] {}", tk.line, tk, message))
}


pub fn resolve_error(tk: &Token, message: &str) -> LoxError {
    LoxError::ResolveError(format!("[line {}, {}] {}", tk.line, tk, message))
}