use crate::expr::{
    Assign, Block, Call, Conditional, Expression, Grouping, Logical, Print, Stmt, Var, Variable,
    While, Function,
};
use crate::lox_errors::{error, LoxResult};
use crate::token_type::TokenType::*;
use crate::{
    expr::{Binary, Expr, Literal, Unary},
    token_type::{Token, TokenType},
};
use std::mem::swap;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize, // points to the next token to be parsed
}

// Each method for parsing a grammar rule produces a syntax tree for that rule and returns it to
// the caller.

// program        → declaration* EOF

// declaration    → funDecl | varDecl | statement;
// funDecl        → "fun" function
// varDecl        → "var" IDENTIFIER ("=" expression)? ";"
// function       → IDENTIFIER "(" parameters? ")" block
// parameters     → IDENTIFIER ("," IDENTIFIER)*
// statement      → exprStmt | forStmt | ifStmt | printStmt | whileStmt | block
// whileStmt      → "while" "(" expression ")" statement
// ifStmt         → "if" "(" expression ")" statement ("else" statement)?
// block          → "{" declaration* "}"
// forStmt        → "for" "(" (valDecl | exprStmt | ";") # initialiser
//                  expression? ";"                      # condition
//                  expression ? ")" statement          # increment, body
// exprStmt       → expression ";"
// printStmt      → "print" expression ";"
// expression     → assignment asdfadfsadf
// assignment     → IDENTIFIER "=" assignment | logic_or
// logic_or       → logic_and ( "or" logic_and )*
// logic_and      → equality ( "and" equality )*
// equality       → comparison ( ( "!=" | "==" ) comparison )*
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )*
// term           → factor ( ( "-" | "+" ) factor )*
// factor         → unary ( ( "/" | "*" ) unary )*
// unary          → ( "!" | "-" ) unary | call
// call           → primary("(" arguments? ")")*
// arguments      → expression ("," expression)*
// primary        → NUMBER | STRING | "true" | "false" | "nil"| "(" expression ")" | IDENTIFIER

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            tokens: Vec::new(),
            current: 0,
        }
    }

    // program → declaration* EOF
    pub fn parse(&mut self, tokens: Vec<Token>) -> Vec<LoxResult<Stmt>> {
        self.current = 0;
        self.tokens = tokens;
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration().map_err(|x| {
                self.sync();
                x
            }))
        }
        statements
    }

    // declaration → funDecl | varDecl | statement;
    fn declaration(&mut self) -> LoxResult<Stmt> {
        if self.mat(&[FUN]){
            return self.function("function")
        }
        else if self.mat(&[VAR]) {
            return self.var_decl();
        }
        self.statement()
    }

    fn function(&mut self, kind: &str) -> LoxResult<Stmt>{
        let name = self.consume_ident(&format!("Expect {} name.",kind))?;
        self.consume(LEFTPAREN, &format!("Expect '(' after {} name.",kind))?;
        let mut parameters = Vec::new();
        if !self.check(&RIGHTPAREN) {
            loop{
                parameters.push(self.consume_ident("Expect parameter name")?);
                if parameters.len() >= 255 {
                    return Err(error(self.peek(), "Can't have more than 255 arguments."));
                }
                if !self.mat(&[COMMA]) {
                    break;
                }
            }
        }
        self.consume(RIGHTPAREN, "Expect ')' after parameters. ")?;
        self.consume(LEFTBRACE, &format!("Expect {{ before {} body.",kind))?;
        let body = self.block()?;
        Ok(Stmt::Function(Function::new(name, parameters, body)))
    }

    // varDecl → "var" IDENTIFIER ("=" expression)? ";"
    fn var_decl(&mut self) -> LoxResult<Stmt> {
        let name = self.consume_ident("Expected variable name.")?;
        let mut initialiser = Expr::Literal(Literal {
            value: Token {
                token_type: NIL,
                line: name.line,
            },
        });
        if self.mat(&[EQUAL]) {
            initialiser = self.expression()?;
        }
        self.consume(SEMICOLON, "Exepcted ';' after variable declaration.")?;
        Ok(Stmt::Var(Var::new(name, initialiser)))
    }

    // statement → exprStmt | forStmt | ifStmt | printStmt | block
    fn statement(&mut self) -> LoxResult<Stmt> {
        if self.mat(&[PRINT]) {
            return self.print_stmt();
        } else if self.mat(&[WHILE]) {
            return self.while_stmt();
        } else if self.mat(&[FOR]) {
            return self.for_stmt();
        } else if self.mat(&[IF]) {
            return self.if_stmt();
        } else if self.mat(&[LEFTBRACE]) {
            return Ok(Stmt::Block(Block::new(self.block()?)));
        }
        self.expr_stmt()
    }

    // whileStmt → "while" "(" expression ")" statement
    fn while_stmt(&mut self) -> LoxResult<Stmt> {
        self.consume(LEFTPAREN, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(RIGHTPAREN, "Expect ')' after condition.")?;
        let body = self.statement()?;
        Ok(Stmt::While(While::new(condition, body)))
    }

    // forStmt → "for" "(" (valDecl | exprStmt | ";") # initialiser
    //                  expression? ";"                      # condition
    //                  expression? ")" statement           # increment, body
    fn for_stmt(&mut self) -> LoxResult<Stmt> {
        self.consume(LEFTPAREN, "Expect '(' after 'for'.")?;
        let initialiser = if self.mat(&[SEMICOLON]) {
            None
        } else if self.mat(&[VAR]) {
            Some(self.var_decl()?)
        } else {
            Some(self.expr_stmt()?)
        };

        let mut condition: Option<Expr> = None;
        if !self.check(&SEMICOLON) {
            condition = Some(self.expression()?);
        }
        self.consume(SEMICOLON, "Expect ';' after loop condition.")?;

        let mut increment: Option<Expr> = None;
        if !self.check(&RIGHTPAREN) {
            increment = Some(self.expression()?);
        }
        self.consume(RIGHTPAREN, "Expct ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = Stmt::Block(Block {
                statements: vec![body, Stmt::Expression(Expression { expression: inc })],
            })
        }

        if let Some(condition) = condition {
            body = Stmt::While(While {
                condition,
                body: Box::new(body),
            })
        } else {
            body = Stmt::While(While {
                condition: Expr::Literal(Literal {
                    value: Token::new(TRUE, 0),
                }),
                body: Box::new(body),
            })
        }

        if let Some(initialiser) = initialiser {
            body = Stmt::Block(Block {
                statements: vec![initialiser, body],
            })
        }

        Ok(body)
    }

    // ifStmt → "if" "(" expression ")" statement ("else" statement)?
    fn if_stmt(&mut self) -> LoxResult<Stmt> {
        self.consume(LEFTPAREN, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(RIGHTPAREN, "Expect ')' after if condition.")?;
        let then_branch = Box::new(self.statement()?);
        let mut else_branch: Option<Box<Stmt>> = None;
        if self.mat(&[ELSE]) {
            else_branch = Some(Box::new(self.statement()?));
        }
        Ok(Stmt::Conditional(Conditional::new(
            condition,
            then_branch,
            else_branch,
        )))
    }

    // block → "{" declaration* "}"
    fn block(&mut self) -> LoxResult<Vec<Stmt>> {
        let mut statements = Vec::new();
        while !self.check(&RIGHTBRACE) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        // current token is RIGHTBRACE or is at the end

        self.consume(RIGHTBRACE, "Expected '}' after block.")?;
        Ok(statements)
    }

    // exprStmt → expression ";"
    fn expr_stmt(&mut self) -> LoxResult<Stmt> {
        let value = self.expression()?;
        self.consume(SEMICOLON, "Expect ';' after value.")?;
        Ok(Stmt::Expression(Expression::new(value)))
    }

    // printStmt → "print" expression ";"
    fn print_stmt(&mut self) -> LoxResult<Stmt> {
        let value = self.expression()?;
        self.consume(SEMICOLON, "Expect ';' after value.")?;
        Ok(Stmt::Print(Print::new(value)))
    }

    // returns true if the current token is of the given type, it never consumes the token
    fn check(&self, tp: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == *tp
    }

    fn check_str_num(&self) -> bool {
        if self.is_at_end() {
            return false;
        }
        matches!(self.peek().token_type, STRING(_) | NUMBER(_))
    }

    fn check_ident(&self) -> bool {
        if self.is_at_end() {
            return false;
        }
        matches!(self.peek().token_type, IDENTIFIER(_))
    }

    // returns and consumes the most recently visited item
    fn previous(&mut self) -> Token {
        let mut temp = Token {
            token_type: BANG,
            line: 0,
        };
        swap(&mut temp, &mut self.tokens[self.current - 1]);
        temp
    }

    // consumes the current token and returns it
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    // returns the current token we have yet to consume
    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    // checks if current token is EOF
    fn is_at_end(&self) -> bool {
        self.peek().token_type == EOF
    }

    // checks to see if current token matches any of the given types, advanced self.current but
    // doesn't consume
    fn mat(&mut self, types: &[TokenType]) -> bool {
        for tp in types {
            if self.check(tp) {
                if !self.is_at_end() {
                    self.current += 1;
                }
                return true;
            }
        }
        false
    }

    fn mat_num_str(&mut self) -> bool {
        if self.check_str_num() {
            if !self.is_at_end() {
                self.current += 1;
            }
            return true;
        }
        false
    }

    fn mat_ident(&mut self) -> bool {
        if self.check_ident() {
            if !self.is_at_end() {
                self.current += 1;
            }
            return true;
        }
        false
    }

    // expression → assignment
    pub fn expression(&mut self) -> LoxResult<Expr> {
        self.assignment()
    }

    // assignment → IDENTIFIER "=" assignment | logic_or;
    pub fn assignment(&mut self) -> LoxResult<Expr> {
        let expr = self.logic_or()?;

        if self.mat(&[EQUAL]) {
            let equals = self.previous();
            let value = self.assignment()?;
            if let Expr::Variable(v) = expr {
                return Ok(Expr::Assign(Assign::new(v.name, value)));
            }
            return Err(error(&equals, "Invalid assignment target."));
        }
        Ok(expr)
    }

    // logic_or → logic_and ( "or" logic_and )*
    pub fn logic_or(&mut self) -> LoxResult<Expr> {
        let mut expr = self.logic_and()?;
        while self.mat(&[OR]) {
            let operator = self.previous();
            let right = self.logic_and()?;
            expr = Expr::Logical(Logical::new(expr, operator, right))
        }
        Ok(expr)
    }

    // logic_and → equality ( "and" equality )*
    pub fn logic_and(&mut self) -> LoxResult<Expr> {
        let mut expr = self.equality()?;
        while self.mat(&[OR]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical(Logical::new(expr, operator, right))
        }
        Ok(expr)
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )*
    fn equality(&mut self) -> LoxResult<Expr> {
        let mut expr = self.comparison()?;
        while self.mat(&[BANGEQUAL, EQUALEQUAL]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Binary::new(expr, operator, right));
        }
        Ok(expr)
    }

    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
    fn comparison(&mut self) -> LoxResult<Expr> {
        let mut expr = self.term()?;
        while self.mat(&[GREATER, GREATEREQUAL, LESS, LESSEQUAL]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary(Binary::new(expr, operator, right))
        }
        Ok(expr)
    }

    // term → factor ( ( "-" | "+" ) factor )*
    fn term(&mut self) -> LoxResult<Expr> {
        let mut expr = self.factor()?;
        while self.mat(&[MINUS, PLUS]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Binary::new(expr, operator, right))
        }
        Ok(expr)
    }

    // factor → unary ( ( "/" | "*" ) unary )*
    fn factor(&mut self) -> LoxResult<Expr> {
        let mut expr = self.unary()?;
        while self.mat(&[SLASH, STAR]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Binary::new(expr, operator, right))
        }
        Ok(expr)
    }

    // unary → ( "!" | "-" ) unary | call
    fn unary(&mut self) -> LoxResult<Expr> {
        if self.mat(&[BANG, MINUS]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary::new(operator, right)));
        }
        self.call()
    }

    // call → primary("(" arguments? ")")*
    fn call(&mut self) -> LoxResult<Expr> {
        let mut expr = self.primary()?;
        loop {
            if self.mat(&[LEFTPAREN]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // arguments → expression ("," expression)*
    fn finish_call(&mut self, callee: Expr) -> LoxResult<Expr> {
        let mut args = Vec::new();
        if !self.check(&RIGHTPAREN) {
            loop {
                args.push(self.expression()?);
                if args.len() >= 255 {
                    return Err(error(self.peek(), "Can't have more than 255 arguments."));
                }
                if !self.mat(&[COMMA]) {
                    break;
                }
            }
        }
        let paren = self.consume(RIGHTPAREN, "Expect ')' after arguments. ")?;
        Ok(Expr::Call(Call::new(callee, paren, args)))
    }

    // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER
    fn primary(&mut self) -> LoxResult<Expr> {
        if self.mat(&[FALSE, TRUE, NIL]) || self.mat_num_str() {
            return Ok(Expr::Literal(Literal::new(self.previous())));
        }

        if self.mat_ident() {
            return Ok(Expr::Variable(Variable::new(self.previous())));
        }

        if self.mat(&[LEFTPAREN]) {
            let expr = self.expression()?;
            self.consume(RIGHTPAREN, "Expected ')' after expression.")?;

            return Ok(Expr::Grouping(Grouping::new(expr)));
        }

        Err(error(self.peek(), "Expected expression."))
    }

    // consumes the token only if it matches
    fn consume(&mut self, tp: TokenType, message: &str) -> LoxResult<Token> {
        if self.check(&tp) {
            return Ok(self.advance());
        }
        let cur = self.peek();
        Err(error(cur, message))
    }

    fn consume_ident(&mut self, message: &str) -> LoxResult<Token> {
        if self.check_ident() {
            return Ok(self.advance());
        }
        let cur = self.peek();
        Err(error(cur, message))
    }

    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == SEMICOLON {
                return;
            }
            match self.peek().token_type {
                CLASS => return,
                FUN => return,
                VAR => return,
                IF => return,
                WHILE => return,
                PRINT => return,
                RETURN => return,
                _ => {}
            }
            self.advance();
        }
    }
}
