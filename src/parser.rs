use std::mem::swap;

use crate::expr::Grouping;
use crate::token_type::TokenType::*;
use crate::{
    expr::{Binary, Expr, Literal, Unary},
    token_type::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize, // points to the next token to be parsed
}

// Each method for parsing a grammar rule produces a syntax tree for that rule and returns it to
// the caller.

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Expr,String>{
        self.expression()
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

    // expression → equality ;
    pub fn expression(&mut self) -> Result<Expr,String> {
        self.equality()
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr,String> {
        let mut expr = self.comparison()?;
        while self.mat(&[BANGEQUAL, EQUALEQUAL]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }
        Ok(expr)
    }

    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr,String> {
        let mut expr = self.term()?;
        while self.mat(&[GREATER, GREATEREQUAL, LESS, LESSEQUAL]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }
        Ok(expr)
    }

    // term → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr,String> {
        let mut expr = self.factor()?;
        while self.mat(&[MINUS, PLUS]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }
        Ok(expr)
    }

    // factor → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr,String> {
        let mut expr = self.unary()?;
        while self.mat(&[SLASH, STAR]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }
        Ok(expr)
    }

    // unary → ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<Expr,String> {
        if self.mat(&[BANG, MINUS]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary {
                operator,
                right: Box::new(right),
            }));
        }
        self.primary()
    }

    // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr,String> {
        if self.mat(&[FALSE, TRUE, NIL]) || self.mat_num_str() {
            return Ok(Expr::Literal(Literal {
                value: self.previous(),
            }));
        }
        if self.mat(&[LEFTPAREN]){
            let expr = self.expression()?;
            self.consume(RIGHTPAREN, "Expected ')' after expression.")?;
            return Ok(Expr::Grouping(Grouping{ expression: Box::new(expr) }));
        }

        Err("Expect expression.".to_string())
    }

    // consumes the token only if it matches
    fn consume(&mut self, tp: TokenType, message: &'static str) -> Result<Token,String>{
        if self.check(&tp){
            return Ok(self.advance());
        }
        let cur = self.peek();
        Err(format!("{}: {}",cur,message))

    }

    #[allow(dead_code)]
    fn sync(&mut self){
        self.advance();

        while !self.is_at_end(){
            if self.previous().token_type == SEMICOLON{
                return;
            }
            match self.peek().token_type{
                CLASS => return,
                FUN => return,
                VAR => return,
                IF => return,
                WHILE => return,
                PRINT => return,
                RETURN => return,
                _ =>{}
            }
            self.advance();
        }

    }
}

