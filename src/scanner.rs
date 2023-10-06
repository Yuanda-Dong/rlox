use crate::lox_errors::{LoxError, LoxResult};
use crate::token_type::Token;
use crate::token_type::TokenType::{self, *};
use std::collections::HashMap;
use std::{char, mem};

pub struct Scanner {
    keywords: HashMap<String, TokenType>,
    source: Vec<u8>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Default for Scanner {
    fn default() -> Self {
        Self::new()
    }
}

fn hacky_string(value: &[u8]) -> String {
    String::from_utf8(value.to_vec()).unwrap()
}

fn is_alpha(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

impl Scanner {
    pub fn new() -> Scanner {
        let mut keywords = HashMap::new();
        keywords.insert("and".to_string(), AND);
        keywords.insert("class".to_string(), CLASS);
        keywords.insert("else".to_string(), ELSE);
        keywords.insert("false".to_string(), FALSE);
        keywords.insert("for".to_string(), FOR);
        keywords.insert("fun".to_string(), FUN);
        keywords.insert("if".to_string(), IF);
        keywords.insert("nil".to_string(), NIL);
        keywords.insert("or".to_string(), OR);
        keywords.insert("print".to_string(), PRINT);
        keywords.insert("return".to_string(), RETURN);
        keywords.insert("super".to_string(), SUPER);
        keywords.insert("this".to_string(), THIS);
        keywords.insert("true".to_string(), TRUE);
        keywords.insert("var".to_string(), VAR);
        keywords.insert("while".to_string(), WHILE);
        Scanner {
            keywords,
            source: Vec::new(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }
    // main loop for scanning tokens
    pub fn scan_tokens(&mut self, source: String) -> LoxResult<Vec<Token>>{
        self.start = 0;
        self.current = 0;
        self.line = 1;
        self.source = source.as_bytes().to_vec();
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.add_token(EOF);
        Ok(mem::take(&mut self.tokens))
    }

    // invariant is self.current < self.source.len()
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
    // consumes and returns the current char in source
    fn advance(&mut self) -> char {
        let c = self.source[self.current] as char;
        self.current += 1;
        c
    }
    // consumes the current char only if it matches, returns bool
    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        };
        if self.source[self.current] as char != expected {
            return false;
        };
        self.current += 1;
        true
    }
    // returns but does not consume the next char
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source[self.current] as char
    }
    // returns but does not consume the 2nd next char
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        };
        self.source[self.current + 1] as char
    }
    // creates and add the token
    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(token_type, self.line));
    }
    // scans string token
    fn string(&mut self) -> Result<(), LoxError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        // current char is " or current char is at end
        if self.is_at_end() {
            return Err(LoxError::ScanError(format!(
                "[line {}] {}",
                self.line, "Unterminated string."
            )));
        }
        self.advance(); // closing "
        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token(TokenType::STRING(hacky_string(value)));
        Ok(())
    }
    // scans number token
    fn number(&mut self) {
        while self.peek().is_numeric() {
            self.advance();
        }
        // look for a fractional part.
        if self.peek() == '.' && self.peek_next().is_numeric() {
            // Consume the '.'
            self.advance();

            while self.peek().is_numeric() {
                self.advance();
            }
        }
        let num = hacky_string(&self.source[self.start..self.current]);
        let parsednum: f64 = num.parse().unwrap();
        self.add_token(NUMBER(parsednum));
    }
    // scan the next keyword or identifier
    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }
        let text = hacky_string(&self.source[self.start..self.current]);
        if let Some(keyword) = self.keywords.get(&text) {
            self.add_token(keyword.clone());
        } else {
            self.add_token(IDENTIFIER(text));
        }
    }

    pub fn scan_token(&mut self) -> LoxResult<()> {
        let c = self.advance();
        match c {
            // SINGLE-CHARACTER TOKENS
            '(' => self.add_token(LEFTPAREN),
            ')' => self.add_token(RIGHTPAREN),
            '{' => self.add_token(LEFTBRACE),
            '}' => self.add_token(RIGHTBRACE),
            ',' => self.add_token(COMMA),
            '.' => self.add_token(DOT),
            '-' => self.add_token(MINUS),
            '+' => self.add_token(PLUS),
            ';' => self.add_token(SEMICOLON),
            '*' => self.add_token(STAR),
            // ONE OR TWO CHARACTER TOKENS
            '!' => {
                if self.match_next('=') {
                    self.add_token(BANGEQUAL);
                } else {
                    self.add_token(BANG);
                }
            }
            '=' => {
                if self.match_next('=') {
                    self.add_token(EQUALEQUAL);
                } else {
                    self.add_token(EQUAL);
                }
            }
            '<' => {
                if self.match_next('=') {
                    self.add_token(LESSEQUAL);
                } else {
                    self.add_token(LESS);
                }
            }
            '>' => {
                if self.match_next('=') {
                    self.add_token(GREATEREQUAL);
                } else {
                    self.add_token(GREATER);
                }
            }
            '/' => {
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(SLASH);
                }
            }
            // ignore white spaces
            ' ' => {}
            '\r' => {}
            '\t' => {}
            '\n' => self.line += 1,
            '"' => self.string()?,
            _ => {
                if c.is_numeric() {
                    self.number();
                } else if is_alpha(c) {
                    self.identifier()
                } else {
                    return Err(LoxError::ScanError(format!(
                        "[line {}, {}] {}",
                        self.line, c, "Unexpected character."
                    )));
                }
            }
        }
        Ok(())
    }
}
