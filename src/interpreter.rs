use crate::environment::Environment;
use crate::expr::*;
use crate::lox_errors::{LoxError, LoxResult};
use crate::token_type::{Token, TokenType};
use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    NULL,
    BOOLEAN(bool),
    NUMBER(f64),
    STRING(String),
}

fn error(tk: &Token, message: &str) -> LoxError {
    LoxError::RunTimeError(format!("[line {}, {}] {}", tk.line, tk, message))
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::NULL => write!(f, "nil"),
            Value::BOOLEAN(x) => write!(f, "{}", x),
            Value::NUMBER(x) => write!(f, "{}", x),
            Value::STRING(x) => write!(f, "{}", x),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (self, other) {
            return x.partial_cmp(y);
        }
        None
    }
}

impl Sub for Value {
    type Output = Result<Value, &'static str>;
    fn sub(self, rhs: Self) -> Self::Output {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (self, rhs) {
            return Ok(Value::NUMBER(x - y));
        }
        Err("Can't subtract these")
    }
}

impl Mul for Value {
    type Output = Result<Value, &'static str>;
    fn mul(self, rhs: Self) -> Self::Output {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (self, rhs) {
            return Ok(Value::NUMBER(x * y));
        }
        Err("Can't multiply these")
    }
}

impl Div for Value {
    type Output = Result<Value, &'static str>;
    fn div(self, rhs: Self) -> Self::Output {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (self, rhs) {
            return Ok(Value::NUMBER(x / y));
        }
        Err("Can't divide these")
    }
}

impl Add for Value {
    type Output = Result<Value, &'static str>;

    fn add(self, rhs: Self) -> Self::Output {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (&self, &rhs) {
            return Ok(Value::NUMBER(x + y));
        }
        if let (Value::STRING(mut x), Value::STRING(y)) = (self, rhs) {
            x.push_str(&y);
            return Ok(Value::STRING(x));
        }

        Err("Can't add these")
    }
}

impl Not for Value {
    type Output = Value;
    fn not(self) -> Self::Output {
        match self.is_truthy() {
            Value::BOOLEAN(x) => Value::BOOLEAN(!x),
            _ => unreachable!(),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, &'static str>;
    fn neg(self) -> Self::Output {
        if let Value::NUMBER(x) = self {
            return Ok(Value::NUMBER(-x));
        }
        Err("Negation works only for Number type")
    }
}

impl Value {
    fn is_truthy(&self) -> Value {
        match self {
            Value::NULL => Value::BOOLEAN(false),
            Value::BOOLEAN(x) => Value::BOOLEAN(*x),
            _ => Value::BOOLEAN(true),
        }
    }
    fn is_true(&self) -> bool {
        match self {
            Value::NULL => false,
            Value::BOOLEAN(x) => *x,
            _ => true,
        }
    }
}

pub trait Eval {
    fn evaluate(&self, env: &mut Environment) -> LoxResult<Value>;
}

pub trait Exe {
    fn execuate(&self, env: &mut Environment) -> LoxResult<()>;
}

impl Exe for Expression {
    fn execuate(&self, env: &mut Environment) -> LoxResult<()> {
        self.expression.evaluate(env)?;
        Ok(())
    }
}

impl Exe for Print {
    fn execuate(&self, env: &mut Environment) -> LoxResult<()> {
        let value = self.expression.evaluate(env)?;
        println!("{}", value);
        Ok(())
    }
}
impl Exe for Var {
    fn execuate(&self, env: &mut Environment) -> LoxResult<()> {
        let value = self.initialiser.evaluate(env)?;
        env.define(self.name.clone(), value);
        Ok(())
    }
}
impl Exe for Block {
    fn execuate(&self, env: &mut Environment) -> LoxResult<()> {
        let mut new_env = Environment::new(Some(env));
        for statement in &self.statements {
            statement.execuate(&mut new_env)?
        }
        Ok(())
    }
}

impl Exe for Conditional {
    fn execuate(&self, env: &mut Environment) -> LoxResult<()> {
        if self.condition.evaluate(env)?.is_true() {
            self.then_branch.execuate(env)?;
        } else if let Some(e) = &self.else_branch {
            e.execuate(env)?;
        }
        Ok(())
    }
}

impl Exe for While {
    fn execuate(&self, env: &mut Environment) -> LoxResult<()> {
        while self.condition.evaluate(env)?.is_true() {
            self.body.execuate(env)?
        }
        Ok(())
    }
}

impl Exe for Stmt {
    fn execuate(&self, env: &mut Environment) -> LoxResult<()> {
        match self {
            Stmt::Expression(x) => x.execuate(env)?,
            Stmt::Print(x) => x.execuate(env)?,
            Stmt::Var(x) => x.execuate(env)?,
            Stmt::Block(x) => x.execuate(env)?,
            Stmt::Conditional(x) => x.execuate(env)?,
            Stmt::While(x) => x.execuate(env)?,
        }
        Ok(())
    }
}

impl Eval for Literal {
    fn evaluate(&self, _: &mut Environment) -> LoxResult<Value> {
        match &self.value.token_type {
            TokenType::NUMBER(x) => Ok(Value::NUMBER(*x)),
            TokenType::STRING(x) => Ok(Value::STRING(x.clone())),
            TokenType::TRUE => Ok(Value::BOOLEAN(true)),
            TokenType::FALSE => Ok(Value::BOOLEAN(false)),
            TokenType::NIL => Ok(Value::NULL),
            _ => Err(error(
                &self.value,
                "a literal is a leaf node of the syntax tree",
            )),
        }
    }
}

impl Eval for Grouping {
    fn evaluate(&self, env: &mut Environment) -> LoxResult<Value> {
        self.expression.evaluate(env)
    }
}

impl Eval for Unary {
    fn evaluate(&self, env: &mut Environment) -> LoxResult<Value> {
        let right = self.right.evaluate(env)?;
        match self.operator.token_type {
            TokenType::MINUS => right.neg().map_err(|x| error(&self.operator, x)),
            TokenType::BANG => Ok(!(right.is_truthy())),
            _ => Err(error(
                &self.operator,
                "unary operator consists of only - and ! in Lox",
            )),
        }
    }
}

impl Eval for Binary {
    fn evaluate(&self, env: &mut Environment) -> LoxResult<Value> {
        let left = self.left.evaluate(env)?;
        let right = self.right.evaluate(env)?;
        match self.operator.token_type {
            TokenType::PLUS => left.add(right).map_err(|x| error(&self.operator, x)),
            TokenType::MINUS => left.sub(right).map_err(|x| error(&self.operator, x)),
            TokenType::STAR => left.mul(right).map_err(|x| error(&self.operator, x)),
            TokenType::SLASH => left.div(right).map_err(|x| error(&self.operator, x)),
            TokenType::LESS => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Less))
                .ok_or(error(&self.operator, "incomparable")),
            TokenType::LESSEQUAL => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Less || x == Ordering::Equal))
                .ok_or(error(&self.operator, "incomparable")),
            TokenType::GREATER => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Greater))
                .ok_or(error(&self.operator, "incomparable")),
            TokenType::GREATEREQUAL => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Greater || x == Ordering::Equal))
                .ok_or(error(&self.operator, "incomparable")),
            TokenType::BANGEQUAL => Ok(Value::BOOLEAN(left != right)),
            TokenType::EQUALEQUAL => Ok(Value::BOOLEAN(left == right)),
            _ => Err(error(
                &self.operator,
                "binary operator consists of only +,-,*, /, <,<=,>.>=, ==, != in Lox",
            )),
        }
    }
}

impl Eval for Variable {
    fn evaluate(&self, env: &mut Environment) -> LoxResult<Value> {
        env.get(&self.name)
    }
}

impl Eval for Assign {
    fn evaluate(&self, env: &mut Environment) -> LoxResult<Value> {
        let value = self.right.evaluate(env)?;
        env.assign(&self.left, value.clone())?;
        Ok(value)
    }
}

impl Eval for Logical {
    fn evaluate(&self, env: &mut Environment) -> LoxResult<Value> {
        let left = self.left.evaluate(env)?;
        match self.operator.token_type {
            // evaluate and return right if left is false, otherwise return left
            TokenType::OR => {
                if !left.is_true() {
                    return self.right.evaluate(env);
                }
            }
            // evaluate and return right if left is true, otherwise return right
            TokenType::AND => {
                if left.is_true() {
                    return self.right.evaluate(env);
                }
            }
            _ => unreachable!(),
        }
        Ok(left)
    }
}

impl Eval for Expr {
    fn evaluate(&self, env: &mut Environment) -> LoxResult<Value> {
        match self {
            Expr::Literal(x) => x.evaluate(env),
            Expr::Binary(x) => x.evaluate(env),
            Expr::Unary(x) => x.evaluate(env),
            Expr::Grouping(x) => x.evaluate(env),
            Expr::Variable(x) => x.evaluate(env),
            Expr::Assign(x) => x.evaluate(env),
            Expr::Logical(x) => x.evaluate(env),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Value;

    #[test]
    fn it_works() {
        let x = Value::NUMBER(20.0);
        let y = Value::STRING("20.0".to_string());
        dbg!(x.partial_cmp(&y));
    }
}
