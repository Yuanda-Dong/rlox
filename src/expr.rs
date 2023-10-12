use crate::token_type::Token;

#[derive(PartialEq,Clone,Debug)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Grouping(Grouping),
    Variable(Variable),
    Assign(Assign),
    Logical(Logical),
    Call(Call),
}
#[derive(PartialEq,Clone,Debug)]
pub struct Call{
    pub callee: Box<Expr>,
    pub paren: Token,
    pub args: Vec<Expr>,
}

impl Call {
    pub fn new(callee: Expr, paren: Token, args: Vec<Expr>) -> Self { Self { callee: Box::new(callee), paren, args } }
}

#[derive(PartialEq,Clone,Debug)]
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

#[derive(PartialEq,Clone,Debug)]
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

#[derive(PartialEq,Clone,Debug)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

impl Grouping {
    pub fn new(expression: Expr) -> Self{
        Self{ expression: Box::new(expression)}
    }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Literal {
    pub value: Token,
}

impl Literal {
    pub fn new(value: Token) -> Self { Self { value } }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Variable {
    pub name: Token,
}

impl Variable {
    pub fn new(name: Token) -> Self { Self { name } }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Assign {
    pub left: Token,
    pub right: Box<Expr>,
}

impl Assign {
    pub fn new(left: Token, right: Expr) -> Self { Self { left, right: Box::new(right) } }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Unary {
    pub fn new(operator: Token, right: Expr) -> Self { Self { operator, right: Box::new(right) } }
}

#[derive(PartialEq,Clone,Debug)]
pub enum Stmt {
    Expression(Expression),
    Print(Print),
    Var(Var),
    Block(Block),
    Conditional(Conditional),
    While(While),
    Function(Function)
}

#[derive(PartialEq,Clone,Debug,Default)]
pub struct Function{
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}



impl Function {
    pub fn new(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self { Self { name, params, body } }
}


#[derive(PartialEq,Clone,Debug)]
pub struct While {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

impl While {
    pub fn new(condition: Expr, body: Stmt) -> Self { Self { condition, body: Box::new(body) } }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Expression {
    pub expression: Expr,
}

impl Expression {
    pub fn new(expression: Expr) -> Self { Self { expression } }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Print {
    pub expression: Expr,
}

impl Print {
    pub fn new(expression: Expr) -> Self { Self { expression } }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Var {
    pub name: Token,
    pub initialiser: Expr,
}

impl Var {
    pub fn new(name: Token, initialiser: Expr) -> Self { Self { name, initialiser } }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

impl Block {
    pub fn new(statements: Vec<Stmt>) -> Self { Self { statements } }
}

#[derive(PartialEq,Clone,Debug)]
pub struct Conditional {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl Conditional {
    pub fn new(condition: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>>) -> Self { Self { condition, then_branch, else_branch } }
}
