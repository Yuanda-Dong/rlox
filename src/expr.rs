use std::{rc::Rc, cell::RefCell};

use crate::token_type::Token;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Grouping(Grouping),
    Variable(Variable),
    Assign(Assign),
    Logical(Logical),
    Call(Call),
    Get(Get),
    Set(Set),
    This(This),
    Super(Super),
}

#[derive(Debug)]
pub struct Super{
    pub keyword: Token,
    pub method: Token,
    pub hops: Option<usize>
}

impl Super {
    pub fn new(keyword: Token, method: Token) -> Self { Self { keyword, method, hops: None } }
}

#[derive(Debug)]
pub struct Get{
    pub object: Box<Expr>,
    pub name: Token,
}
#[derive(Debug)]
pub struct This{
    pub keyword: Token,
    pub hops: Option<usize>,
}

impl This {
    pub fn new(keyword: Token) -> Self { Self { keyword, hops: None } }
}

#[derive(Debug)]
pub struct Set{
    pub object: Box<Expr>,
    pub name: Token,
    pub value: Box<Expr>,
}

impl Set {
    pub fn new(object: Box<Expr>, name: Token, value: Expr) -> Self { Self { object, name, value: Box::new(value) } }
    
}

impl Get {
    pub fn new(object: Expr, name: Token) -> Self { Self { object: Box::new(object), name } }
}

#[derive(Debug)]
pub struct Call{
    pub callee: Box<Expr>,
    pub paren: Token,
    pub args: Vec<Expr>,
}

impl Call {
    pub fn new(callee: Expr, paren: Token, args: Vec<Expr>) -> Self { Self { callee: Box::new(callee), paren, args } }
}

#[derive(Debug)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Binary {
   pub fn new(left: Expr, operator: Token, right: Expr) -> Self{
         Self{left: Box::new(left), operator, right: Box::new(right)}
   } 
}


#[derive(Debug)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Logical {
   pub fn new(left: Expr, operator: Token, right: Expr) -> Self{
        Self{left: Box::new(left), operator, right: Box::new(right)}
   } 
}

#[derive(Debug)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

impl Grouping {
    pub fn new(expression: Expr) -> Self{
        Self{ expression: Box::new(expression)}
    }
}

#[derive(Debug)]
pub struct Literal {
    pub value: Token,
}

impl Literal {
    pub fn new(value: Token) -> Self { Self { value } }
}

#[derive(Debug)]
pub struct Variable {
    pub name: Token,
    pub hop: Option<usize>,
}

impl Variable {
    pub fn new(name: Token) -> Self { Self { name,hop:None } }
    pub fn get_name(&self) -> &str{
        match &self.name.token_type{
            crate::token_type::TokenType::IDENTIFIER(x) => x,
            _ => unreachable!()
        }
        
    }
}

#[derive(Debug)]
pub struct Assign {
    pub left: Token,
    pub right: Box<Expr>,
    pub hops: Option<usize>,
}

impl Assign {
    pub fn new(left: Token, right: Expr) -> Self { Self { left, right: Box::new(right), hops: None } }
}

#[derive(Debug)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Unary {
    pub fn new(operator: Token, right: Expr) -> Self { Self { operator, right: Box::new(right) } }
}

#[derive(Debug)]
pub enum Stmt {
    Expression(Expression),
    Print(Print),
    Return(Return),
    Var(Var),
    Block(Block),
    Conditional(Conditional),
    While(While),
    Function(Rc<RefCell<Function>>),
    Class(Class),
}

#[derive(Debug)]
pub struct Class{
    pub name: Token,
    pub methods: Vec<Rc<RefCell<Function>>>,
    pub super_class: Option<Variable>,
}

impl Class {
    pub fn new(name: Token, methods: Vec<Rc<RefCell<Function>>>, super_class: Option<Variable>) -> Self { Self { name, methods, super_class } }
}

#[derive(Debug)]
pub struct Return {
    pub keyword: Token,
    pub value: Option<Expr>,
}

impl Return {
    pub fn new(keyword: Token, value: Option<Expr>) -> Self { Self { keyword, value } }
}

#[derive(Debug)]
pub struct Function{
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl Function {
    pub fn new(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self { Self { name, params, body } }
}


#[derive(Debug)]
pub struct While {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

impl While {
    pub fn new(condition: Expr, body: Stmt) -> Self { Self { condition, body: Box::new(body) } }
}

#[derive(Debug)]
pub struct Expression {
    pub expression: Expr,
}

impl Expression {
    pub fn new(expression: Expr) -> Self { Self { expression } }
}

#[derive(Debug)]
pub struct Print {
    pub expression: Expr,
}

impl Print {
    pub fn new(expression: Expr) -> Self { Self { expression } }
}

#[derive(Debug)]
pub struct Var {
    pub name: Token,
    pub initialiser: Expr,
}

impl Var {
    pub fn new(name: Token, initialiser: Expr) -> Self { Self { name, initialiser } }
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

impl Block {
    pub fn new(statements: Vec<Stmt>) -> Self { Self { statements } }
}

#[derive(Debug)]
pub struct Conditional {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl Conditional {
    pub fn new(condition: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>>) -> Self { Self { condition, then_branch, else_branch } }
}
