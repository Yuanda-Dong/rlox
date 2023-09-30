use crate::expr::*;
use crate::token_type::TokenType;
use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

#[derive(PartialEq)]
pub enum Value {
    NULL,
    BOOLEAN(bool),
    NUMBER(f64),
    STRING(String),
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
    type Output = Result<Value, String>;
    fn sub(self, rhs: Self) -> Self::Output {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (self, rhs) {
            return Ok(Value::NUMBER(x - y));
        }
        Err("Can't subtract".to_string())
    }
}

impl Mul for Value {
    type Output = Result<Value, String>;
    fn mul(self, rhs: Self) -> Self::Output {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (self, rhs) {
            return Ok(Value::NUMBER(x * y));
        }
        Err("Can't multiply".to_string())
    }
}

impl Div for Value {
    type Output = Result<Value, String>;
    fn div(self, rhs: Self) -> Self::Output {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (self, rhs) {
            return Ok(Value::NUMBER(x / y));
        }
        Err("Can't divide".to_string())
    }
}

impl Add for Value {
    type Output = Result<Value, String>;

    fn add(self, rhs: Self) -> Self::Output {
        if let (Value::NUMBER(x), Value::NUMBER(y)) = (&self, &rhs) {
            return Ok(Value::NUMBER(x + y));
        }
        if let (Value::STRING(mut x), Value::STRING(y)) = (self, rhs) {
            x.push_str(&y);
            return Ok(Value::STRING(x));
        }

        Err("Can only (+ String String) or (+ Number Number)".to_string())
    }
}

impl Not for Value {
    type Output = Result<Value, String>;
    fn not(self) -> Self::Output {
        match self.is_truthy() {
            Value::BOOLEAN(x) => Ok(Value::BOOLEAN(!x)),
            _ => Err("self.is_truthy must produce Value::BOOLEAN".to_string()),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, String>;
    fn neg(self) -> Self::Output {
        if let Value::NUMBER(x) = self {
            return Ok(Value::NUMBER(-x));
        }
        Err("Negation works only for Number type".to_string())
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
}

pub trait Eval {
    fn evaluate(self) -> Result<Value, String>;
}

impl Eval for Literal {
    fn evaluate(self) -> Result<Value, String> {
        match self.value.token_type {
            TokenType::NUMBER(x) => Ok(Value::NUMBER(x)),
            TokenType::STRING(x) => Ok(Value::STRING(x)),
            TokenType::TRUE => Ok(Value::BOOLEAN(true)),
            TokenType::FALSE => Ok(Value::BOOLEAN(false)),
            TokenType::NIL => Ok(Value::NULL),
            _ => Err("a literal is a leaf node of the syntax tree".to_string()),
        }
    }
}

impl Eval for Grouping {
    fn evaluate(self) -> Result<Value, String> {
        self.expression.evaluate()
    }
}

impl Eval for Unary {
    fn evaluate(self) -> Result<Value, String> {
        let right = self.right.evaluate()?;
        match self.operator.token_type {
            TokenType::MINUS => right.neg(),
            TokenType::BANG => !(right.is_truthy()),
            _ => Err("unary operator consists of only - and ! in Lox".to_string()),
        }
    }
}

impl Eval for Binary {
    fn evaluate(self) -> Result<Value, String> {
        let left = self.left.evaluate()?;
        let right = self.right.evaluate()?;
        match self.operator.token_type {
            TokenType::PLUS => left.add(right),
            TokenType::MINUS => left.sub(right),
            TokenType::STAR => left.mul(right),
            TokenType::SLASH => left.div(right),
            TokenType::LESS => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Less))
                .ok_or("incomparable".to_string()),
            TokenType::LESSEQUAL => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Less || x == Ordering::Equal))
                .ok_or("incomparable".to_string()),
            TokenType::GREATER => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Greater))
                .ok_or("incomparable".to_string()),
            TokenType::GREATEREQUAL => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Greater || x == Ordering::Equal))
                .ok_or("incomparable".to_string()),
            TokenType::BANGEQUAL => Ok(Value::BOOLEAN(left != right)),
            TokenType::EQUALEQUAL => Ok(Value::BOOLEAN(left == right)),
            _ => Err(
                "binary operator consists of only +,-,*, /, <,<=,>.>=, ==, != in Lox".to_string(),
            ),
        }
    }
}

impl Eval for Expr {
    fn evaluate(self) -> Result<Value, String> {
        match self {
            Expr::Literal(x) => x.evaluate(),
            Expr::Binary(x) => x.evaluate(),
            Expr::Unary(x) => x.evaluate(),
            Expr::Grouping(x) => x.evaluate(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Value;

    #[test]
    fn it_works() {
        let x = Value::NUMBER(20.0);
        let y = Value::STRING("WTF".to_string());
        dbg!(x.partial_cmp(&y));
        // assert!(x==y)
    }
}
