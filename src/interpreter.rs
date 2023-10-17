use crate::environment::Environment;
use crate::expr::*;
use crate::lox_errors::{run_error, LoxError, LoxResult};
use crate::token_type::TokenType;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub enum Value {
    NULL,
    BOOLEAN(bool),
    NUMBER(f64),
    STRING(String),
    NativeFn(NativeFn),
    LoxFn(Rc<RefCell<LoxFn>>),
    LoxClass(Rc<RefCell<LoxClass>>),
    LoxInstance(Rc<RefCell<LoxInstance>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::NULL => write!(f, "nil"),
            Value::BOOLEAN(x) => write!(f, "{}", x),
            Value::NUMBER(x) => write!(f, "{}", x),
            Value::STRING(x) => write!(f, "{}", x),
            Value::NativeFn(x) => write!(f, "{}", x),
            Value::LoxFn(x) => write!(f, "{}", x.borrow()),
            Value::LoxClass(x) => write!(f, "{}", x.borrow()),
            Value::LoxInstance(x) => write!(f, "{}", x.borrow()),
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
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value>;
}

pub trait Exe {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()>;
}

impl Exe for Expression {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        self.expression.evaluate(env)?;
        Ok(())
    }
}

impl Exe for Print {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        let value = self.expression.evaluate(env)?;
        println!("{}", value);
        Ok(())
    }
}
impl Exe for Var {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        let value = self.initialiser.evaluate(env)?;
        env.borrow_mut().define(&self.name, value);
        Ok(())
    }
}
impl Exe for Block {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        let new_env = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        for statement in &self.statements {
            statement.execuate(&new_env)?
        }
        Ok(())
    }
}

impl Exe for Conditional {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        if self.condition.evaluate(env)?.is_true() {
            self.then_branch.execuate(env)?;
        } else if let Some(e) = &self.else_branch {
            e.execuate(env)?;
        }
        Ok(())
    }
}

impl Exe for While {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        while self.condition.evaluate(env)?.is_true() {
            self.body.execuate(env)?
        }
        Ok(())
    }
}

impl Exe for Rc<RefCell<Function>> {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        env.borrow_mut().define(
            &self.borrow().name,
            Value::LoxFn(Rc::new(RefCell::new(LoxFn {
                arity: self.borrow().params.len(),
                name: self.borrow().name.to_string(),
                declaration: self.clone(),
                closure: env.clone(),
            }))),
        );
        Ok(())
    }
}

impl Exe for Return {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        if let Some(v) = &self.value {
            Err(LoxError::ReturnValue(v.evaluate(env)?))
        } else {
            Err(LoxError::ReturnValue(Value::NULL))
        }
    }
}

impl Exe for Class {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        env.borrow_mut().define(&self.name, Value::NULL);
        let mut methods = HashMap::new();
        for method in &self.methods {
            methods.insert(
                method.borrow().name.to_string(),
                Value::LoxFn(Rc::new(RefCell::new(LoxFn::new(method, env)))),
            );
        }
        let klass = Value::LoxClass(Rc::new(RefCell::new(LoxClass {
            name: self.name.to_string(),
            methods,
        })));
        env.borrow_mut().assign(&self.name, klass)?;
        Ok(())
    }
}

impl Exe for Stmt {
    fn execuate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<()> {
        match self {
            Stmt::Expression(x) => x.execuate(env)?,
            Stmt::Print(x) => x.execuate(env)?,
            Stmt::Var(x) => x.execuate(env)?,
            Stmt::Block(x) => x.execuate(env)?,
            Stmt::Conditional(x) => x.execuate(env)?,
            Stmt::While(x) => x.execuate(env)?,
            Stmt::Function(x) => x.execuate(env)?,
            Stmt::Return(x) => x.execuate(env)?,
            Stmt::Class(x) => x.execuate(env)?,
        }
        Ok(())
    }
}

impl Eval for Literal {
    fn evaluate(&self, _: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        match &self.value.token_type {
            TokenType::NUMBER(x) => Ok(Value::NUMBER(*x)),
            TokenType::STRING(x) => Ok(Value::STRING(Rc::new(RefCell::new(x.to_string())))),
            TokenType::TRUE => Ok(Value::BOOLEAN(true)),
            TokenType::FALSE => Ok(Value::BOOLEAN(false)),
            TokenType::NIL => Ok(Value::NULL),
            _ => Err(run_error(
                &self.value,
                "a literal is a leaf node of the syntax tree",
            )),
        }
    }
}

impl Eval for Grouping {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        self.expression.evaluate(env)
    }
}

impl Eval for Unary {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        let right = self.right.evaluate(env)?;
        match self.operator.token_type {
            TokenType::MINUS => right.neg().map_err(|x| run_error(&self.operator, x)),
            TokenType::BANG => Ok(!(right.is_truthy())),
            _ => Err(run_error(
                &self.operator,
                "unary operator consists of only - and ! in Lox",
            )),
        }
    }
}

impl Eval for Binary {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        let left = self.left.evaluate(env)?;
        let right = self.right.evaluate(env)?;
        match self.operator.token_type {
            TokenType::PLUS => left.add(right).map_err(|x| run_error(&self.operator, x)),
            TokenType::MINUS => left.sub(right).map_err(|x| run_error(&self.operator, x)),
            TokenType::STAR => left.mul(right).map_err(|x| run_error(&self.operator, x)),
            TokenType::SLASH => left.div(right).map_err(|x| run_error(&self.operator, x)),
            TokenType::LESS => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Less))
                .ok_or(run_error(&self.operator, "incomparable")),
            TokenType::LESSEQUAL => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Less || x == Ordering::Equal))
                .ok_or(run_error(&self.operator, "incomparable")),
            TokenType::GREATER => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Greater))
                .ok_or(run_error(&self.operator, "incomparable")),
            TokenType::GREATEREQUAL => left
                .partial_cmp(&right)
                .map(|x| Value::BOOLEAN(x == Ordering::Greater || x == Ordering::Equal))
                .ok_or(run_error(&self.operator, "incomparable")),
            TokenType::BANGEQUAL => Ok(Value::BOOLEAN(left != right)),
            TokenType::EQUALEQUAL => Ok(Value::BOOLEAN(left == right)),
            _ => Err(run_error(
                &self.operator,
                "binary operator consists of only +,-,*, /, <,<=,>.>=, ==, != in Lox",
            )),
        }
    }
}

impl Eval for Variable {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        env.borrow().get_at(&self.name, self.hop)
    }
}

impl Eval for Assign {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        let value = self.right.evaluate(env)?;
        env.borrow_mut()
            .assign_at(&self.left, value.clone(), self.hops)?;
        Ok(value)
    }
}

impl Eval for Logical {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
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

#[derive(PartialEq, Clone)]
pub struct NativeFn {
    pub arity: usize,
    pub name: String,
    pub f: fn(Option<Vec<Value>>) -> Option<Value>,
}

impl Display for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn {}>", self.name)
    }
}

#[derive(Clone)]
pub struct LoxFn {
    pub arity: usize,
    pub name: String,
    pub declaration: Rc<RefCell<Function>>,
    pub closure: Rc<RefCell<Environment>>,
}

impl LoxFn {
    pub fn new(f: &Rc<RefCell<Function>>, env: &Rc<RefCell<Environment>>) -> LoxFn {
        LoxFn {
            arity: f.borrow().params.len(),
            name: f.borrow().name.to_string(),
            declaration: f.clone(),
            closure: env.clone(),
        }
    }
    pub fn bind(&mut self, instance: Rc<RefCell<LoxInstance>>) -> LoxFn {
        let mut environment = Environment::new(Some(self.closure.clone()));
        environment.def("this", Value::LoxInstance(instance));
        return LoxFn::new(&self.declaration, &Rc::new(RefCell::new(environment)));
    }
}

#[derive(Clone, PartialEq)]
pub struct LoxClass {
    pub name: String,
    pub methods: HashMap<String, Value>,
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl LoxClass {
    fn find_method(&self, name: &str) -> Option<Value> {
        self.methods.get(name).cloned()
    }
}

#[derive(Clone, PartialEq)]
pub struct LoxInstance {
    pub lox_class: Rc<RefCell<LoxClass>>,
    pub fields: HashMap<String, Value>,
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.lox_class.borrow())
    }
}

impl PartialEq for LoxFn {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity
            && self.name == other.name
            && Rc::ptr_eq(&self.declaration, &other.declaration)
    }
}

impl Display for LoxFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Eval for Call {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        let callee = self.callee.evaluate(env)?;
        let mut args = Vec::new();
        for arg in &self.args {
            args.push(arg.evaluate(env)?);
        }

        match callee {
            Value::LoxClass(x) => {
                if !args.is_empty() {
                    return Err(run_error(&self.paren, "Expects 0 args"));
                }
                let instance = LoxInstance {
                    lox_class: x.clone(),
                    fields: HashMap::new(),
                };
                Ok(Value::LoxInstance(Rc::new(RefCell::new(instance))))
            }
            Value::NativeFn(x) => {
                if args.len() != x.arity {
                    return Err(run_error(
                        &self.paren,
                        &format!("expected {} args, but got {} args", x.arity, args.len()),
                    ));
                }

                let f = x.f;
                Ok(f(Some(args)).unwrap())
            }
            Value::LoxFn(x) => {
                if args.len() != x.borrow().arity {
                    return Err(run_error(
                        &self.paren,
                        &format!(
                            "expected {} args, but got {} args",
                            x.borrow().arity,
                            args.len()
                        ),
                    ));
                }
                let environment = Rc::new(RefCell::new(Environment::new(Some(
                    x.borrow().closure.clone(),
                ))));

                for (i, v) in args.iter().enumerate().take(x.borrow().arity) {
                    if let TokenType::IDENTIFIER(name) = &x
                        .borrow()
                        .declaration
                        .borrow()
                        .params
                        .get(i)
                        .unwrap()
                        .token_type
                    {
                        environment.borrow_mut().def(name, v.clone());
                    } else {
                        return Err(run_error(&self.paren, "WTF IS THIS"));
                    }
                }

                for statement in &x.borrow().declaration.borrow().body {
                    match statement.execuate(&environment) {
                        Ok(_) => {}
                        Err(x) => match x {
                            LoxError::ReturnValue(y) => return Ok(y),
                            _ => return Err(x),
                        },
                    }
                }
                Ok(Value::NULL)
            }
            _ => Err(run_error(&self.paren, "only callable can be called")),
        }
    }
}
impl Eval for Get {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        let object = self.object.evaluate(env)?;
        match object {
            Value::LoxInstance(x) => {
                if let Some(y) = x.borrow().fields.get(self.name.get_id()?).cloned() {
                    Ok(y)
                } else if let Some(y) = x
                    .borrow()
                    .lox_class
                    .borrow()
                    .find_method(self.name.get_id()?)
                {
                    match y {
                        Value::LoxFn(z) => {
                            z.borrow_mut().bind(x.clone());
                            Ok(Value::LoxFn(z))
                        }
                        _ => unreachable!(),
                    }
                } else {
                    Err(run_error(
                        &self.name,
                        &format!("Undefeind property {}", self.name),
                    ))
                }
            }
            _ => Err(run_error(&self.name, "Only instances have properties")),
        }
    }
}

impl Eval for Set {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        let object = self.object.evaluate(env)?;
        match object {
            Value::LoxInstance(x) => {
                let value = self.value.evaluate(env)?;
                x.borrow_mut()
                    .fields
                    .insert(self.name.to_string(), value.clone());
                Ok(value)
            }
            _ => Err(run_error(&self.name, "Only instances have properties")),
        }
    }
}
impl Eval for This {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        env.borrow().borrow_mut().get_at(&self.keyword, self.hops)
    }
}

impl Eval for Expr {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> LoxResult<Value> {
        match self {
            Expr::Literal(x) => x.evaluate(env),
            Expr::Binary(x) => x.evaluate(env),
            Expr::Unary(x) => x.evaluate(env),
            Expr::Grouping(x) => x.evaluate(env),
            Expr::Variable(x) => x.evaluate(env),
            Expr::Assign(x) => x.evaluate(env),
            Expr::Logical(x) => x.evaluate(env),
            Expr::Call(x) => x.evaluate(env),
            Expr::Get(x) => x.evaluate(env),
            Expr::Set(x) => x.evaluate(env),
            Expr::This(x) => x.evaluate(env),
        }
    }
}
