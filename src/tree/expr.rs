#[derive(Debug, Clone)]
pub enum Literal {
    /// 2
    Integer(i64),
    /// 2.0
    Float(f64),
    /// "foo"
    String(String),
    /// nil
    Nil,
    /// true
    /// false
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Mul,
    /// /
    Div,
    /// ==
    Equal,
    /// !=
    NotEqual,
    /// <
    LessThan,
    /// <=
    LessThanOrEqual,
    /// >
    GreaterThan,
    /// >=
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    /// !
    Not,
    /// -
    Minus,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Grouped(Box<Expr>),
}
