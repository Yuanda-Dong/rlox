use crate::{
    interpreter::Value,
    lox_errors::{run_error, LoxResult},
    token_type::{Token, TokenType},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    pub values: HashMap<String, Value>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn def(&mut self, name: &str, value: Value) {
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

    fn get(&self, name: &Token) -> LoxResult<Value> {
        match &name.token_type {
            TokenType::IDENTIFIER(x) => {
                if let Some(y) = self.values.get(x) {
                    Ok(y.to_owned())
                } else if let Some(enclosing) = &self.enclosing {
                    enclosing.borrow().get(name)
                } else {
                    Err(run_error(name, "undefined variable"))
                }
            }
            _ => unreachable!(),
        }
    }

    // fn get_mut (&mut self, name: &Token) -> LoxResult<*mut Value> {
    //     match &name.token_type {
    //         TokenType::IDENTIFIER(x) => {
    //             if let Some(y) = self.values.get_mut(x) {
    //                 Ok(y)
    //             } else if let Some(enclosing) = &self.enclosing {
    //                 enclosing.borrow_mut().get_mut(name)
    //             } else {
    //                 Err(run_error(name, "undefined variable"))
    //             }
    //         }
    //         _ => unreachable!(),
    //     }
    // }

    pub fn get_at(&self, name: &Token, distance: Option<usize>) -> LoxResult<Value> {
        match &name.token_type {
            TokenType::IDENTIFIER(x) => {
                if let Some(d) = distance {
                    let mut env = self as *const Environment;
                    for _ in 0..d {
                        env = unsafe { (*env).enclosing.as_ref().unwrap().as_ptr() }
                    }
                    if let Some(y) = unsafe { (*env).values.get(x) } {
                        Ok(y.to_owned())
                    } else {
                        Err(run_error(name, "undefined variable"))
                    }
                } else {
                    //TODO
                    // println!("im called {} {}.", name, name.line);
                    self.get(name)
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn assign(&mut self, name: &Token, value: Value) -> LoxResult<()> {
        match &name.token_type {
            TokenType::IDENTIFIER(x) => {
                if let Some(y) = self.values.get_mut(x) {
                    *y = value;
                    Ok(())
                } else if let Some(enclosing) = &mut self.enclosing {
                    enclosing.borrow_mut().assign(name, value)
                } else {
                    Err(run_error(name, "undefined variable"))
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn assign_at(
        &mut self,
        name: &Token,
        value: Value,
        distance: Option<usize>,
    ) -> LoxResult<()> {
        match &name.token_type {
            TokenType::IDENTIFIER(x) => {
                if let Some(d) = distance {
                    let mut env = self as *mut Environment;
                    for _ in 0..d {
                        env = unsafe { (*env).enclosing.as_mut().unwrap().as_ptr() }
                    }
                    if let Some(y) = unsafe { (*env).values.get_mut(x) } {
                        *y = value;
                        Ok(())
                    } else {
                        Err(run_error(name, "undefined variable"))
                    }
                } else {
                    // println!("im called {} {}.", name, name.line);
                    // TODO
                    self.assign(name, value)
                }
            }
            _ => unreachable!(),
        }
    }
}
