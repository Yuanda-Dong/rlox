use std::fmt::Display;

use crate::token_type::Token;

pub enum LoxError{
    ParseError(String), ScanError(String), RunTimeError(String)
}

pub type LoxResult<T> = Result<T,LoxError>;
impl Display for LoxError{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            LoxError::ParseError(x) => write!(f,"ParseError {}",x),
            LoxError::ScanError(x) => write!(f,"ScanError {}",x),
            LoxError::RunTimeError(x) => write!(f,"RunTimeError {}",x),
        }
    }
}

pub fn error(tk: &Token, message: &str) -> LoxError {
    LoxError::RunTimeError(format!("[line {}, {}] {}", tk.line, tk, message))
}