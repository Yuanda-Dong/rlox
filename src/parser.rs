use std::mem::swap;

use crate::{token_type::{Token, TokenType}, expr::{Expr, Binary, Unary, Literal}};
use crate::token_type::TokenType::{*};

pub struct Parser{
    tokens: Vec<Token>,
    current: usize, // points to the next token to be parsed
}


// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

// Each method for parsing a grammar rule produces a syntax tree for that rule and returns it to
// the caller. 

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser{
        Parser{
            tokens,
            current: 0,
        }
    }
    // returns true if the current token is of the given type, it never consumes the token
    fn check(&self, tp: &TokenType) -> bool{
        if self.is_at_end(){
            return false;
        }
        self.peek().token_type == *tp
    }
    
    // returns and consumes the most recently visited item
    fn previous(&mut self) -> Token{
        let mut temp = Token{
            token_type: BANG,
            line: 0,
        };
        swap(&mut temp, &mut self.tokens[self.current -1]);
        temp
    }

    // consumes the current token and returns it
    fn advance(&mut self) -> Token{
        if !self.is_at_end(){
            self.current += 1;
        }
        self.previous()
    }

    // returns the current token we have yet to consume
    fn peek(&self) -> &Token{
        self.tokens.get(self.current).unwrap()
    }

    // checks if current token is EOF
    fn is_at_end(&self) -> bool{
        self.peek().token_type == EOF
    }

    // checks to see if current token matches any of the given types, advanced self.current but
    // doesn't consume 
    fn mat(&mut self, types: &[TokenType]) -> bool{
        for tp in types{
            if self.check(tp){
                if !self.is_at_end(){
                    self.current += 1;
                }
                return true;
            }
        }
        false
    }

    fn mat_num_str(&mut self) -> bool{

    }

    // expression     → equality ;
    fn expression(&mut self) -> Expr{
        self.equality()
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Expr{
        let mut expr = self.comparison();
        while self.mat(&[BANGEQUAL,BANG]){
            let operator = self.previous();
            let right = self.comparison();
            expr = Expr::Binary(Binary{ left: Box::new(expr), operator, right: Box::new(right) })
        }
        expr
    }

    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Expr{
        let mut expr = self.term();
        while self.mat(&[GREATER,GREATEREQUAL,LESS,LESSEQUAL]){
            let operator = self.previous();
            let right = self.term();
            expr = Expr::Binary(Binary{ left: Box::new(expr), operator, right: Box::new(right) })
        }
        expr
    }

    fn term(&mut self) -> Expr{
        let mut expr = self.factor();
        while self.mat(&[MINUS,PLUS]){
            let operator = self.previous();
            let right = self.factor();
            expr = Expr::Binary(Binary{ left: Box::new(expr), operator, right: Box::new(right) })
        }
        expr
    }

    fn factor(&mut self) -> Expr{
        let mut expr = self.unary();
        while self.mat(&[SLASH,STAR]){
            let operator = self.previous();
            let right = self.unary();
            expr = Expr::Binary(Binary{ left: Box::new(expr), operator, right: Box::new(right) })
        }
        expr
    }

    fn unary(&mut self) -> Expr{
        if self.mat(&[BANG, MINUS]){
            let operator = self.previous();
            let right = self.unary();
            return Expr::Unary(Unary{ operator, right: Box::new(right) });
        }
        return self.primary();
    }

    fn primary(&mut self) -> Expr{
        if self.mat(&[FALSE,TRUE,NIL]){
            return Expr::Literal(Literal{ value: self.previous() });
        }

        unimplemented!()
    }
}




























