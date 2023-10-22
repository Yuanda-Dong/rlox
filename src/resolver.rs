// A block statement introduces a new scope for the statements it contains.
// A function declaration introduces a new scope for its body and binds its parameters in that scope.
// A variable declaration adds a new variable to the current scope.
// Variable and assignment expressions need to have their variables resolved.

use std::collections::HashMap;

use crate::{
    expr::*,
    lox_errors::{resolve_error, LoxResult},
    token_type::{Token, TokenType},
};

#[derive(Copy, Clone, PartialEq, Debug)]
enum FunctionType {
    None,
    FUNCTION,
    INITIALIZER,
    METHOD,
}

#[derive(Copy, Clone, PartialEq)]
enum ClassType {
    NONE,
    CLASS,
    SUBCLASS,
}

pub struct Resolver {
    pub scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            current_function: FunctionType::None,
            current_class: ClassType::NONE,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    // declaration adds the variable to the innermost scope so that it shadows any outer one and so that we know the variable exists. We mark it as "not ready yet" by binding its name to `false`.
    // the value associated with a key in the scope map represents whether or not we have finished resolving that variable's initialiser.
    fn declare(&mut self, name: &Token) -> LoxResult<()> {
        if self.scopes.len() > 1 && self.scopes.last().unwrap().contains_key(&name.to_string()) {
            return Err(resolve_error(
                name,
                "Already a variable with this name in this scope.",
            ));
        }

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), false);
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), true);
        }
    }

    fn resolve_local(&mut self, expr: &mut dyn Resolvable) {
        // here i is the number of scope we need to jump out to find the binding. If we walked through all of the block scopes and never find the variable, we leave it unresolved and assume it's global.
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(expr.get_name()) {
                expr.resolve_hops(i);
                break;
            }
        }
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

pub trait Resolvable {
    fn resolve_hops(&mut self, hops: usize);
    fn get_name(&self) -> &str;
}

impl Resolvable for Variable {
    fn resolve_hops(&mut self, hops: usize) {
        self.hop = Some(hops);
    }

    fn get_name(&self) -> &str {
        match &self.name.token_type {
            TokenType::IDENTIFIER(x) => x,
            _ => unreachable!(),
        }
    }
}

impl Resolvable for Assign {
    fn resolve_hops(&mut self, hops: usize) {
        self.hops = Some(hops);
    }

    fn get_name(&self) -> &str {
        match &self.left.token_type {
            TokenType::IDENTIFIER(x) => x,
            _ => unreachable!(),
        }
    }
}

impl Resolvable for This {
    fn resolve_hops(&mut self, hops: usize) {
        self.hops = Some(hops);
    }

    fn get_name(&self) -> &str {
        "this"
    }
}

impl Resolvable for Super {
    fn resolve_hops(&mut self, hops: usize) {
        self.hops = Some(hops);
    }

    fn get_name(&self) -> &str {
        "super"
    }
}

pub trait Resolve {
    // scopes keep track of the stack of scopes. keys are variables names.
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()>;
}

impl Resolve for Stmt {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        match self {
            Stmt::Expression(x) => x.resolve(resolver),
            Stmt::Print(x) => x.resolve(resolver),
            Stmt::Return(x) => x.resolve(resolver),
            Stmt::Var(x) => x.resolve(resolver),
            Stmt::Block(x) => x.resolve(resolver),
            Stmt::Conditional(x) => x.resolve(resolver),
            Stmt::While(x) => x.resolve(resolver),
            Stmt::Function(x) => x.borrow_mut().resolve(resolver, FunctionType::FUNCTION),
            Stmt::Class(x) => x.resolve(resolver),
        }
    }
}

impl Resolve for Expr {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        match self {
            Expr::Literal(x) => x.resolve(resolver),
            Expr::Binary(x) => x.resolve(resolver),
            Expr::Unary(x) => x.resolve(resolver),
            Expr::Grouping(x) => x.resolve(resolver),
            Expr::Variable(x) => x.resolve(resolver),
            Expr::Assign(x) => x.resolve(resolver),
            Expr::Logical(x) => x.resolve(resolver),
            Expr::Call(x) => x.resolve(resolver),
            Expr::Get(x) => x.resolve(resolver),
            Expr::Set(x) => x.resolve(resolver),
            Expr::This(x) => x.resolve(resolver),
            Expr::Super(x) => x.resolve(resolver),
        }
    }
}

impl Resolve for Super {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        match resolver.current_class {
            ClassType::NONE => Err(resolve_error(
                &self.keyword,
                "Can't use 'super' outside a class.",
            )),
            ClassType::CLASS => Err(resolve_error(&self.keyword, "Can't use 'super' in a class with no superclass.")),
            ClassType::SUBCLASS => {
                resolver.resolve_local(self);
                Ok(())
            }
        }
    }
}

impl Resolve for This {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        if resolver.current_class == ClassType::NONE {
            return Err(resolve_error(
                &self.keyword,
                "Can't use 'this' outside of a class",
            ));
        }
        resolver.resolve_local(self);
        Ok(())
    }
}

impl Resolve for Set {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.value.resolve(resolver)?;
        self.object.resolve(resolver)
    }
}

impl Resolve for Get {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.object.resolve(resolver)
    }
}

impl Resolve for Class {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        let enclosing_class = resolver.current_class;
        resolver.current_class = ClassType::CLASS;
        resolver.declare(&self.name)?;
        resolver.define(&self.name);
        if let Some(x) = &mut self.super_class {
            if self.name.to_string() == x.get_name() {
                return Err(resolve_error(
                    &self.name,
                    "A class can't inherit from itself.",
                ));
            }
            resolver.current_class = ClassType::SUBCLASS;
            x.resolve(resolver)?;
        }
        if let Some(_) = &mut self.super_class {
            resolver.begin_scope();
            resolver
                .scopes
                .last_mut()
                .unwrap()
                .insert("super".to_string(), true);
        }
        resolver.begin_scope();
        resolver
            .scopes
            .last_mut()
            .unwrap()
            .insert("this".to_string(), true);
        for method in &mut self.methods {
            if method.borrow().name.get_id()? == "init" {
                method
                    .borrow_mut()
                    .resolve(resolver, FunctionType::INITIALIZER)?;
            } else {
                method
                    .borrow_mut()
                    .resolve(resolver, FunctionType::METHOD)?;
            }
        }
        resolver.end_scope();

        if let Some(_) = &mut self.super_class {
            resolver.end_scope();
        }
        resolver.current_class = enclosing_class;
        Ok(())
    }
}

impl Resolve for Block {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        resolver.begin_scope();
        for statement in &mut self.statements {
            statement.resolve(resolver)?;
        }
        resolver.end_scope();
        Ok(())
    }
}

// vardecl statement
impl Resolve for Var {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        resolver.declare(&self.name)?;
        self.initialiser.resolve(resolver)?;
        resolver.define(&self.name);
        Ok(())
    }
}

// variable expression
impl Resolve for Variable {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        if resolver.scopes.len() > 1 {
            if let Some(scope) = resolver.scopes.last() {
                if let Some(v) = scope.get(self.get_name()) {
                    if !v {
                        return Err(resolve_error(
                            &self.name,
                            "Can't read local variable in its own initialiser",
                        ));
                    }
                }
            }
        }
        resolver.resolve_local(self);
        Ok(())
    }
}

// assignment expression
impl Resolve for Assign {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.right.resolve(resolver)?;
        resolver.resolve_local(self);
        Ok(())
    }
}

// INVARIANT: 1.need to recurisvely call .resolve(resolver) on sub-expression and statement
//            2.need to call resolver.resolve_local on Resolvable, variable and assignment
//            expressions are resolvable

impl Function {
    fn resolve(&mut self, resolver: &mut Resolver, tp: FunctionType) -> LoxResult<()> {
        resolver.declare(&self.name)?;
        resolver.define(&self.name);
        let enclosing_function = resolver.current_function;
        resolver.current_function = tp;
        resolver.begin_scope();
        for param in &self.params {
            resolver.declare(param)?;
            resolver.define(param);
        }
        for stmt in &mut self.body {
            stmt.resolve(resolver)?;
        }
        resolver.end_scope();
        resolver.current_function = enclosing_function;
        Ok(())
    }
}

impl Resolve for Expression {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.expression.resolve(resolver)
    }
}

impl Resolve for Conditional {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.condition.resolve(resolver)?;
        self.then_branch.resolve(resolver)?;
        if let Some(else_branch) = &mut self.else_branch {
            else_branch.resolve(resolver)?;
        }
        Ok(())
    }
}

impl Resolve for Print {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.expression.resolve(resolver)
    }
}

impl Resolve for Return {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        if resolver.current_function == FunctionType::None {
            return Err(resolve_error(
                &self.keyword,
                "Can't return from top-level code.",
            ));
        }
        if let Some(v) = &mut self.value {
            if resolver.current_function == FunctionType::INITIALIZER {
                return Err(resolve_error(
                    &self.keyword,
                    "Can't return from an initialiser",
                ));
            }

            v.resolve(resolver)?;
        }
        Ok(())
    }
}

impl Resolve for While {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.condition.resolve(resolver)?;
        self.body.resolve(resolver)
    }
}

impl Resolve for Binary {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.left.resolve(resolver)?;
        self.right.resolve(resolver)
    }
}

impl Resolve for Call {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.callee.resolve(resolver)?;
        for arg in &mut self.args {
            arg.resolve(resolver)?;
        }
        Ok(())
    }
}

impl Resolve for Grouping {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.expression.resolve(resolver)
    }
}

impl Resolve for Literal {
    fn resolve(&mut self, _resolver: &mut Resolver) -> LoxResult<()> {
        Ok(())
    }
}

impl Resolve for Logical {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.left.resolve(resolver)?;
        self.right.resolve(resolver)
    }
}

impl Resolve for Unary {
    fn resolve(&mut self, resolver: &mut Resolver) -> LoxResult<()> {
        self.right.resolve(resolver)
    }
}
