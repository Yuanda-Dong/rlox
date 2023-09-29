use std::fmt::{self, Display};

#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::upper_case_acronyms)]
pub enum TokenType {
    // SINGLE-CHARACTER TOKENS
    LEFTPAREN, RIGHTPAREN, LEFTBRACE, RIGHTBRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    // ONE OR TWO CHARACTER TOKENS
    BANG, BANGEQUAL, EQUAL, EQUALEQUAL, GREATER, GREATEREQUAL, LESS, LESSEQUAL,

    // LITERALS
    IDENTIFIER(String),STRING(String),NUMBER(f64),

    // KEYWORDS
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, EOF,
}
impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LEFTPAREN => write!(f, "(")?,
            TokenType::RIGHTPAREN => write!(f, ")")?,
            TokenType::LEFTBRACE => write!(f, "{{")?,
            TokenType::RIGHTBRACE => write!(f, "}}")?,
            TokenType::COMMA => write!(f, ",")?,
            TokenType::DOT => write!(f, ".")?,
            TokenType::MINUS => write!(f, "-")?,
            TokenType::PLUS => write!(f, "+")?,
            TokenType::SEMICOLON => write!(f, ";")?,
            TokenType::SLASH => write!(f, "/")?,
            TokenType::STAR => write!(f, "*")?,
            TokenType::BANG => write!(f, "!")?,
            TokenType::BANGEQUAL => write!(f, "!=")?,
            TokenType::EQUAL => write!(f, "=")?,
            TokenType::EQUALEQUAL => write!(f, "==")?,
            TokenType::GREATER => write!(f, ">")?,
            TokenType::GREATEREQUAL => write!(f, ">=")?,
            TokenType::LESS => write!(f, "<")?,
            TokenType::LESSEQUAL => write!(f, "<=")?,
            TokenType::IDENTIFIER(x) => write!(f, "ident: {}", x)?,
            TokenType::STRING(x) => write!(f, "\"{}\"", x)?,
            TokenType::NUMBER(x) => write!(f, "{}", x)?,
            TokenType::AND => write!(f, "AND")?,
            TokenType::CLASS => write!(f, "CLASS")?,
            TokenType::ELSE => write!(f, "ELSE")?,
            TokenType::FALSE => write!(f, "FALSE")?,
            TokenType::FUN => write!(f, "FUN")?,
            TokenType::FOR => write!(f, "FOR")?,
            TokenType::IF => write!(f, "IF")?,
            TokenType::NIL => write!(f, "NIL")?,
            TokenType::OR => write!(f, "OR")?,
            TokenType::PRINT => write!(f, "PRINT")?,
            TokenType::RETURN => write!(f, "RETURN")?,
            TokenType::SUPER => write!(f, "SUPER")?,
            TokenType::THIS => write!(f, "THIS")?,
            TokenType::TRUE => write!(f, "TRUE")?,
            TokenType::VAR => write!(f, "VAR")?,
            TokenType::WHILE => write!(f, "WHILE")?,
            TokenType::EOF => write!(f, "EOF")?,
        };
        Ok(())
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize) -> Token {
        Token { token_type, line }
    }
}

impl Display for Token{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write!(f,"Line: {} {}",self.line, self.token_type)?;
        write!(f,"{}",self.token_type)?;
        Ok(())
    }
}
