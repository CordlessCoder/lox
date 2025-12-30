use crate::*;
use std::fmt::{Display, Write};

impl Display for LiteralExpression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LiteralExpression::*;
        match self {
            Nil => f.write_str("nil"),
            Bool(x) => x.fmt(f),
            Str(text) => write!(f, "{text:?}"),
            Char(c) => write!(f, "{c:?}"),
            Float(x) => x.fmt(f),
            Int(x) => x.fmt(f),
        }
    }
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOperator::*;
        let operator = match self {
            Mul => "*",
            Div => "/",
            Sub => "-",
            Add => "+",
            Lt => "<",
            Le => "<=",
            Gt => ">",
            Ge => ">=",
            Eq => "==",
            Ne => "!=",
            And => "and",
            Or => "or",
        };
        f.write_str(operator)
    }
}

impl Display for BinaryExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { lhs, op, rhs } = self;
        write!(f, "({op} {lhs} {rhs})")
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnaryOperator::*;
        let operator = match self {
            Neg => '-',
            Not => '!',
        };
        f.write_char(operator)
    }
}

impl Display for UnaryExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { op, val } = self;
        write!(f, "({op} {val})")
    }
}

impl Display for Assignment<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { target, val } = self;
        write!(f, "(= {target} {val})")
    }
}

impl Display for Call<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { callee, arguments } = self;
        write!(f, "(call {callee} [")?;
        let mut arguments = arguments.iter();
        if let Some(first) = arguments.next() {
            write!(f, "{first}")?;
        }
        for arg in arguments {
            write!(f, ", {arg}")?;
        }
        f.write_str("])")
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => i.fmt(f),
            Expr::Lit(literal) => literal.fmt(f),
            Expr::Grouped(expr) => write!(f, "(group {expr})"),
            Expr::Binary(binary) => binary.fmt(f),
            Expr::Unary(unary) => unary.fmt(f),
            Expr::Assignment(assignment) => assignment.fmt(f),
            Expr::Call(call) => call.fmt(f),
        }
    }
}
