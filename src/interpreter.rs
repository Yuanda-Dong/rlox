use crate::environment::Environment;
use crate::expr::*;
use crate::lox_errors::{error, LoxResult};
use crate::token_type::TokenType;
use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};


#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    NULL,
    BOOLEAN(bool),
    NUMBER(f64),
    STRING(String),
    NativeFn(NativeFn),
    LoxFn(LoxFn),
    NOTAVALUE
}

impl Default for Value {
    fn default() -> Self {
        Value::NOTAVALUE
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::NULL => write!(f, "nil"),
            Value::BOOLEAN(x) => write!(f, "{}", x),
            Value::NUMBER(x) => write!(f, "{}", x),
            Value::STRING(x) => write!(f, "{}", x),
            Value::NativeFn(x) => write!(f, "{}", x),
            Value::LoxFn(x) => write!(f, "{}", x),
            Value::NOTAVALUE => write!(f,"Not a value"),
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
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value>;
}

pub trait Exe {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()>;
}

impl Exe for Expression {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()> {
        self.expression.evaluate(env)?;
        Ok(())
    }
}

impl Exe for Print {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()> {
        let value = self.expression.evaluate(env)?;
        println!("{}", value);
        Ok(())
    }
}
impl Exe for Var {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()> {
        let value = self.initialiser.evaluate(env)?;
        env.define(std::mem::take(&mut self.name), value);
        Ok(())
    }
}
impl Exe for Block {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()> {
        let mut new_env = Environment::new(Some(env));
        for statement in &mut self.statements {
            statement.execuate(&mut new_env)?
        }
        Ok(())
    }
}

impl Exe for Conditional {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()> {
        if self.condition.evaluate(env)?.is_true() {
            self.then_branch.execuate(env)?;
        } else if let Some(e) = &mut self.else_branch {
            e.execuate(env)?;
        }
        Ok(())
    }
}

impl Exe for While {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()> {
        while self.condition.evaluate(env)?.is_true() {
            self.body.execuate(env)?
        }
        Ok(())
    }
}

impl Exe for Function {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()> {
        env.define(
            std::mem::take(&mut self.name),
            Value::LoxFn(LoxFn {
                arity: self.params.len(),
                name: self.name.to_string(),
                declaration: std::mem::take(self)
            }),
        );
        Ok(())
    }
}

impl Exe for Stmt {
    fn execuate(&mut self, env: &mut Environment) -> LoxResult<()> {
        match self {
            Stmt::Expression(x) => x.execuate(env)?,
            Stmt::Print(x) => x.execuate(env)?,
            Stmt::Var(x) => x.execuate(env)?,
            Stmt::Block(x) => x.execuate(env)?,
            Stmt::Conditional(x) => x.execuate(env)?,
            Stmt::While(x) => x.execuate(env)?,
            Stmt::Function(x) => x.execuate(env)?,
        }
        Ok(())
    }
}

impl Eval for Literal {
    fn evaluate(&mut self, _: &mut Environment) -> LoxResult<Value> {
        match &mut self.value.token_type {
            TokenType::NUMBER(x) => Ok(Value::NUMBER(*x)),
            TokenType::STRING(x) => Ok(Value::STRING(std::mem::take(x))),
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
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value> {
        self.expression.evaluate(env)
    }
}

impl Eval for Unary {
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value> {
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
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value> {
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
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value> {
        env.get(&self.name)
    }
}

impl Eval for Assign {
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value> {
        let value = self.right.evaluate(env)?;
        env.assign(&self.left, value.clone())?;
        Ok(value)
    }
}

impl Eval for Logical {
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value> {
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

// interface LoxCallable {
//   int arity();
//   Object call(Interpreter interpreter, List<Object> arguments);
// }

// trait LoxCallable {
//     fn call(args: &Vec<Value>) -> Value;
//     fn to_string() -> String;
// }

#[derive(PartialEq, Clone, Debug)]
pub struct NativeFn {
    pub arity: usize,
    pub name: String,
    pub f: fn(Option<Vec<Value>>) -> Option<Value>,
}

impl Display for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LoxFn {
    pub arity: usize,
    pub name: String,
    pub declaration: Function,
}

impl Display for LoxFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
impl Eval for Call {
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value> {
        let callee = self.callee.evaluate(env)?;
        // let args: Vec<Value> = self.args.iter().map(|x| x.evaluate(env)?).collect();
        let mut args = Vec::new();
        for arg in &mut self.args {
            args.push(arg.evaluate(env)?);
        }

        match callee {
            Value::NativeFn(callable) => {
                if args.len() != callable.arity {
                    return Err(error(
                        &self.paren,
                        &format!(
                            "expected {} args, but got {} args",
                            callable.arity,
                            args.len()
                        ),
                    ));
                }

                let f = callable.f;
                Ok(f(Some(args)).unwrap())
            }
            Value::LoxFn(mut x) => {
                if args.len() != x.arity {
                    return Err(error(
                        &self.paren,
                        &format!("expected {} args, but got {} args", x.arity, args.len()),
                    ));
                }
                let mut environment = Environment::new(Some(env));
                // for param in &*x.declaration.params{
                //     environment.native_def(&param.to_string(), args[i])
                // }
                for (i,mut v) in args.iter_mut().enumerate().take(x.arity){
                    if let TokenType::IDENTIFIER(name) =
                        &x.declaration.params.get(i).unwrap().token_type
                    {
                        environment.native_def(name, std::mem::take(&mut v));
                    }else{
                        return Err(error(&self.paren,"WTF IS THIS"));
                    }

                }

                for statement in &mut x.declaration.body {
                    statement.execuate(&mut environment)?
                }
                Ok(Value::NULL)
            }
            _ => Err(error(&self.paren, "only callable can be called"))
        }
    }
}

impl Eval for Expr {
    fn evaluate(&mut self, env: &mut Environment) -> LoxResult<Value> {
        match self {
            Expr::Literal(x) => x.evaluate(env),
            Expr::Binary(x) => x.evaluate(env),
            Expr::Unary(x) => x.evaluate(env),
            Expr::Grouping(x) => x.evaluate(env),
            Expr::Variable(x) => x.evaluate(env),
            Expr::Assign(x) => x.evaluate(env),
            Expr::Logical(x) => x.evaluate(env),
            Expr::Call(x) => x.evaluate(env),
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
