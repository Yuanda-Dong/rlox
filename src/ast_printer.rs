use std::fmt::Display;

use crate::expr::{Binary, Expr, Grouping, Literal, Unary, Variable, Assign, Logical};

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.operator, self.left, self.right)
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.operator, self.right)
    }
}

impl Display for Grouping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(group {})", self.expression)
    }
}
impl Display for Variable{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Variable: {}",self.name)
    }
}
impl Display for Assign{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Assign: {} = {}",self.left,self.right)
    }
}

impl Display for Logical{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Logical: {} {} {}",self.left,self.operator,self.right)
    }
}


impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(x) => write!(f, "{}", x)?,
            Expr::Binary(x) => write!(f, "{}", x)?,
            Expr::Unary(x) => write!(f, "{}", x)?,
            Expr::Grouping(x) => write!(f, "{}", x)?,
            Expr::Variable(x) => write!(f, "{}",x)?,
            Expr::Assign(x) => write!(f,"{}",x)?,
            Expr::Logical(x) => write!(f,"{}",x)?,
            Expr::Call(_) => todo!(),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::token_type::TokenType::*;
    use crate::{expr::*, token_type::Token};

    #[test]
    fn it_works() {
        let unary = Expr::Unary(Unary {
            operator: Token::new(MINUS, 1),
            right: Box::new(Expr::Literal(Literal {
                value: Token::new(NUMBER(123.0), 1),
            })),
        });
        let grouping = Expr::Grouping(Grouping {
            expression: Box::new(Expr::Literal(Literal {
                value: Token::new(NUMBER(45.67), 1),
            })),
        });
        let expression = Expr::Binary(Binary {
            left: Box::new(unary),
            operator: Token::new(STAR, 1),
            right: Box::new(grouping),
        });
        println!("{}", expression);
    }
}
