use std::borrow::Cow;
mod display;
pub mod tree;

#[derive(Debug, Clone)]
pub struct Program<'s> {
    pub declarations: Vec<Decl<'s>>,
}

#[derive(Debug, Clone)]
pub enum Decl<'s> {
    Stmt(Stmt<'s>),
    VarDecl {
        name: &'s str,
        init: Option<Expr<'s>>,
    },
}

#[derive(Debug, Clone)]
pub enum LiteralExpression<'s> {
    Str(Cow<'s, str>),
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    // and
    And,
    // or
    Or,
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
    /// ==
    Eq,
    /// !=
    Ne,
    /// <
    Lt,
    /// <=
    Le,
    /// >
    Gt,
    /// >=
    Ge,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr<'s> {
    pub lhs: Expr<'s>,
    pub op: BinaryOperator,
    pub rhs: Expr<'s>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    /// !
    Not,
    /// -
    Neg,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr<'s> {
    pub op: UnaryOperator,
    pub val: Expr<'s>,
}

#[derive(Debug, Clone)]
pub enum Expr<'s> {
    Lit(LiteralExpression<'s>),
    Ident(&'s str),
    Binary(Box<BinaryExpr<'s>>),
    Unary(Box<UnaryExpr<'s>>),
    Grouped(Box<Self>),
    Assignment(Box<Assignment<'s>>),
}

#[derive(Debug, Clone)]
pub struct MemberAccess<'s> {
    pub object: Expr<'s>,
    pub member: &'s str,
}

#[derive(Debug, Clone, Default)]
pub struct Block<'s>(pub Vec<Decl<'s>>);

#[derive(Debug, Clone)]
pub enum Stmt<'s> {
    Expr(Expr<'s>),
    Return(Option<Expr<'s>>),
    Break,
    Continue,
    While {
        cond: Expr<'s>,
        body: Block<'s>,
    },
    Block(Block<'s>),
    Print {
        value: Expr<'s>,
    },
    // /// A function definition with parameter names
    //     Function(Function<'s>),
    //     Use {
    //         module: &'s str,
    //         alias: Option<&'s str>,
    //     },
    //     VarDecl {
    //         name: &'s str,
    //         ty: Type<'s>,
    //         init: Option<Expr<'s>>,
    //         public: bool,
    //         mutable: bool,
    //     },
    If {
        cond: Expr<'s>,
        then_body: Block<'s>,
        // TODO: Add else if
        // elifs: Vec<Elif<'s>>,
        else_body: Option<Block<'s>>,
    },
    For(Box<For<'s>>),
    //     TypeDecl {
    //         ty: TypeDecl<'s>,
    //         public: bool,
    //         name: &'s str,
    //     },
    //     Loop {
    //         cond: Option<Expr<'s>>,
    //         initializers: Block<'s>,
    //         post_ops: Block<'s>,
    //         body: Block<'s>,
    //     },
    //     Defer(Box<Stmt<'s>>),
    //     Switch {
    //         value: Expr<'s>,
    //         cases: Vec<SwitchCase<'s>>,
    //         default: Option<Box<Stmt<'s>>>,
    //     },
}

#[derive(Debug, Clone)]
pub struct For<'s> {
    pub initializer: Option<Decl<'s>>,
    pub cond: Option<Expr<'s>>,
    pub increment: Option<Expr<'s>>,
    pub body: Block<'s>,
}

#[derive(Debug, Clone)]
pub struct Assignment<'s> {
    pub target: &'s str,
    pub val: Expr<'s>,
}
//

// #[derive(Debug, Clone)]
// pub enum BinOpKind {
//     /// +
//     Add,
//     /// -
//     Sub,
//     /// *
//     Mul,
//     /// /
//     Div,
//     /// %
//     Mod,
//     /// **
//     Pow,
//     /// ==
//     Eq,
//     /// !=
//     Ne,
//     /// <
//     Lt,
//     /// <=
//     Le,
//     /// >
//     Gt,
//     /// >=
//     Ge,
//     /// &&
//     And,
//     /// ||
//     Or,
//     /// &
//     BitAnd,
//     /// |
//     BitOr,
//     /// ^
//     BitXor,
//     /// <<
//     Shl,
//     /// >>
//     Shr,
//     /// ..
//     Range,
// }

// #[derive(Debug, Clone, Copy)]
// pub enum UnaryOpKind {
//     /// !
//     Not,
//     /// -
//     Neg,
//     /// +
//     Pos,
//     /// ~
//     BitNot,
//     /// ++x
//     PreInc,
//     /// --x
//     PreDec,
//     /// x++
//     PostInc,
//     /// x--
//     PostDec,
//     /// *x
//     Deref,
//     /// &x
//     Addr,
// }

// #[derive(Debug, Clone)]
// pub struct Function<'s> {
//     pub params: Vec<(Type<'s>, &'s str)>,
//     pub ret: Type<'s>,
//     pub body: Block<'s>,
// }
//
// #[derive(Debug, Clone)]
// pub enum Type<'s> {
//     Void,
//     Bool,
//     Char,
//     Int,
//     UInt,
//     Float,
//     Double,
//     Str,
//     Named(&'s str),
//     Pointer { pointee: Box<Type<'s>> },
//     Array(Box<Type<'s>>, Expr<'s>),
//     Function(Box<Function<'s>>),
// }
//
// #[derive(Debug, Clone)]
// pub struct StructField<'s> {
//     pub name: &'s str,
//     pub ty: Type<'s>,
//     pub public: bool,
// }
//
// #[derive(Debug, Clone)]
// pub enum TypeDecl<'s> {
//     Struct { fields: Vec<StructField<'s>> },
//     Enum { members: Vec<&'s str> },
// }
//
// #[derive(Debug, Clone)]
// pub struct Elif<'s> {
//     pub cond: Expr<'s>,
//     pub body: Block<'s>,
// }

//
// #[derive(Debug, Clone)]
// pub struct SwitchCase<'s> {
//     pub body: Stmt<'s>,
//     pub cases: Vec<Expr<'s>>,
// }
//
//
// #[derive(Debug, Clone)]
// pub struct Ternary<'s> {
//     pub then_val: Expr<'s>,
//     pub else_val: Expr<'s>,
//     pub cond: Expr<'s>,
// }
//
//
// #[derive(Debug, Clone)]
// pub struct UnaryOp<'s> {
//     pub val: Expr<'s>,
//     pub op: UnaryOpKind,
// }
//
// #[derive(Debug, Clone)]
// pub struct Call<'s> {
//     pub callee: Box<Expr<'s>>,
//     pub args: Vec<Expr<'s>>,
// }
//

// #[derive(Debug, Clone)]
// pub struct NamespaceAccess<'s> {
//     pub object: Expr<'s>,
//     pub member: &'s str,
// }
//
// #[derive(Debug, Clone)]
// pub enum SizeOf<'s> {
//     Val(Expr<'s>),
//     Type(Type<'s>),
// }
//
// #[derive(Debug, Clone)]
// pub enum Intrinsic<'s> {
//     Index {
//         target: Expr<'s>,
//         index: Expr<'s>,
//     },
//     Memcpy {
//         from: Expr<'s>,
//         to: Expr<'s>,
//         size: Expr<'s>,
//     },
//     Deref {
//         addr: Expr<'s>,
//     },
//     Addr {
//         of: Expr<'s>,
//     },
//     Alloc {
//         size: Expr<'s>,
//     },
//     Free {
//         ptr: Expr<'s>,
//     },
//     Cast {
//         ty: Type<'s>,
//         val: Expr<'s>,
//     },
//     SizeOf(SizeOf<'s>),
// }
//
// #[derive(Debug, Clone)]
// pub enum Expr<'s> {
//     Lit(LiteralExpression<'s>),
//     BinOp(Box<BinOp<'s>>),
//     UnaryOp(Box<UnaryOp<'s>>),
//     Ternary(Box<Ternary<'s>>),
//     Call(Call<'s>),
//     Assignment(Box<Assignment<'s>>),
//     MemberAccess(Box<MemberAccess<'s>>),
//     NamespaceAccess(Box<NamespaceAccess<'s>>),
//     Group(Box<Expr<'s>>),
//     Array(Vec<Expr<'s>>),
//     Intrinsic(Box<Intrinsic<'s>>),
// }
//
//
//
// #[derive(Debug, Clone)]
// pub struct Module<'s> {
//     pub body: Block<'s>,
//     pub name: &'s str,
// }
