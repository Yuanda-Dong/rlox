use crate::{
    interpreter::Value,
    lox_errors::{LoxError, LoxResult},
    token_type::{Token, TokenType},
};
use std::{collections::HashMap, rc::Rc, cell::RefCell};

pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    pub values: HashMap<String, Value>,
}

fn error(tk: &Token, message: &str) -> LoxError {
    LoxError::RunTimeError(format!("[line {}, {}] {}", tk.line, tk, message))
}



impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn def(&mut self, name: &str, value: Value){
        self.values.insert(name.to_string(), value);
    }

    pub fn define(&mut self, name: &Token, value: Value) {
        match &name.token_type {
            TokenType::IDENTIFIER(x) => {
                self.values.insert(x.clone(), value);
            }
            _ => unreachable!(),
        }
    }

    pub fn get(&self, name: &Token) -> LoxResult<Value> {
        match &name.token_type {
            TokenType::IDENTIFIER(x) => {
                if let Some(y) = self.values.get(x) {
                    Ok(y.to_owned())
                } else if let Some(enclosing) = &self.enclosing {
                    enclosing.borrow().get(name)
                } else {
                    Err(error(name, "undefined variable"))
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn assign(&mut self, name: &Token, value: Value) -> LoxResult<()> {
        match &name.token_type {
            TokenType::IDENTIFIER(x) => {
                if let Some(y) = self.values.get_mut(x){
                    *y = value;
                    Ok(())
                } else if let Some(enclosing) = &mut self.enclosing{
                    enclosing.borrow_mut().assign(name, value)
                }else{
                    Err(error(name, "undefined variable"))
                }
            }
            _ => unreachable!(),
        }
    }
}
