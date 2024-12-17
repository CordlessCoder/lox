use std::fmt::{Display, Write};

use super::expr::*;

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Literal::*;
        match self {
            Nil => f.write_str("nil"),
            Bool(x) => x.fmt(f),
            String(text) => write!(f, "{text:?}"),
            Float(x) => x.fmt(f),
            Integer(x) => x.fmt(f),
        }
    }
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOperator::*;
        let operator = match self {
            Mul => "*",
            Div => "/",
            Minus => "-",
            Plus => "+",
            LessThan => "<",
            LessThanOrEqual => "<=",
            GreaterThan => ">",
            GreaterThanOrEqual => ">=",
            Equal => "==",
            NotEqual => "!=",
        };
        f.write_str(operator)
    }
}

impl Display for BinaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            left,
            operator,
            right,
        } = self;
        write!(f, "({operator} {left} {right})")
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnaryOperator::*;
        let operator = match self {
            Minus => '-',
            Not => '!',
        };
        f.write_char(operator)
    }
}

impl Display for UnaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { operator, expr } = self;
        write!(f, "({operator} {expr})")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(literal) => literal.fmt(f),
            Expr::Grouped(expr) => write!(f, "(group {expr})"),
            Expr::Binary(binary) => binary.fmt(f),
            Expr::Unary(unary) => unary.fmt(f),
        }
    }
}
