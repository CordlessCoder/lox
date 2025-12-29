#![expect(unused)]
use ast::{
    Assignment, BinaryExpr, BinaryOperator, Block, Decl, Expr, For, LiteralExpression, Program,
    Stmt, UnaryExpr, UnaryOperator,
};
use diagnostics::{AggregateError, ErrorComponent};
use lexer::{SToken, Token};
use source::SourceFile;
use std::collections::VecDeque;

// use crate::expr::BindingPower;

mod basic_ops;
mod expr;

pub struct Parser<'s, Tokens: Iterator> {
    tokens: Tokens,
    source: SourceFile,
    peeked: VecDeque<SToken<'s>>,
    lexer_errors: AggregateError,
    errors: AggregateError,
}

// If a parsing function returns None, an error occurred and we must synchronize to try to
// continue parsing
impl<'s, Tokens: Iterator<Item = Result<SToken<'s>, ErrorComponent>>> Parser<'s, Tokens> {
    // pub(crate) fn parse_module_header(&mut self) -> Option<&'s str> {
    //     if !self.consume_if(|t| matches!(t, Token::At("module"))) {
    //         let (next, span) = self.peek_next_split();
    //         let msg = format!("Expected @module declaration, found {next:?}");
    //         self.new_parse_error(span, msg);
    //         return None;
    //     }
    //     let name = self.expect_ident(" module name after @module declaration")?;
    //     Some(name)
    // }
    // pub(crate) fn parse_use(&mut self) -> Option<Stmt<'s>> {
    //     self.expect(&Token::At("use"))?;
    //     let module = self.expect_ident(" name after @use")?;
    //     let mut alias = None;
    //     if self.consume_if(|t| matches!(t, Token::As)) {
    //         alias = Some(self.expect_ident(" alias after @use _ as")?);
    //     }
    //     Some(Stmt::Use { module, alias })
    // }
    // pub(crate) fn parse_return(&mut self) -> Option<Stmt<'s>> {
    //     self.expect(&Token::Return)?;
    //
    //     let val = if !self.consume_if(|t| matches!(t, Token::Semicolon)) {
    //         let val = Some(self.parse_expr(BindingPower::Lowest)?);
    //         self.expect(&Token::Semicolon)?;
    //         val
    //     } else {
    //         None
    //     };
    //     Some(Stmt::Return(val))
    // }
    // pub(crate) fn parse_fn_param(&mut self) -> Option<(ast::Type<'s>, &'s str)> {
    //     let name = self.expect_ident(" in function parameters")?;
    //     self.expect(&Token::Colon)?;
    //     let ty = self.parse_type()?;
    //     Some((ty, name))
    // }
    // pub(crate) fn parse_fn_decl(&mut self) -> Option<ast::Function<'s>> {
    //     self.expect(&Token::Fn)?;
    //     self.expect(&Token::LParen)?;
    //     let params = self.delimited_list_with_terminator(
    //         Self::parse_fn_param,
    //         &Token::Comma,
    //         &Token::RParen,
    //     );
    //     let params: Vec<(ast::Type<'s>, &'s str)> = params.collect();
    //     let ret = self.parse_type()?;
    //     let body = self.parse_block()?;
    //     Some(ast::Function { params, ret, body })
    // }
    // pub(crate) fn parse_struct_field(&mut self) -> Option<ast::StructField<'s>> {
    //     let public = self.consume_if(|t| matches!(t, Token::Pub));
    //     let name = self.expect_ident(" in struct field")?;
    //     // name = fn...
    //     // struct method
    //     if self.consume_if(|t| matches!(t, Token::Eq)) {
    //         let func = self.parse_fn_decl()?;
    //         return Some(ast::StructField {
    //             name,
    //             ty: ast::Type::Function(Box::new(func)),
    //             public,
    //         });
    //     }
    //     // field
    //     self.expect(&Token::Colon)?;
    //     let ty = self.parse_type()?;
    //     Some(ast::StructField { name, public, ty })
    // }
    // pub(crate) fn parse_enum_decl(&mut self) -> Option<ast::TypeDecl<'s>> {
    //     self.expect(&Token::Enum)?;
    //     self.expect(&Token::LBrace)?;
    //
    //     let members = self.delimited_list_with_terminator(
    //         |p| p.expect_ident(" in enum declaration"),
    //         &Token::Comma,
    //         &Token::RBrace,
    //     );
    //     let members: Vec<&str> = members.collect();
    //     self.expect(&Token::Semicolon)?;
    //     Some(ast::TypeDecl::Enum { members })
    // }
    // pub(crate) fn parse_struct_decl(&mut self) -> Option<ast::TypeDecl<'s>> {
    //     self.expect(&Token::Struct)?;
    //     self.expect(&Token::LBrace)?;
    //     let fields = self.delimited_list_with_terminator(
    //         |p| p.parse_struct_field(),
    //         &Token::Comma,
    //         &Token::RBrace,
    //     );
    //     let fields: Vec<ast::StructField<'s>> = fields.collect();
    //     self.expect(&Token::Semicolon)?;
    //     Some(ast::TypeDecl::Struct { fields })
    // }
    // pub(crate) fn parse_var_decl(&mut self, public: bool) -> Option<Stmt<'s>> {
    //     let (kind, _) = self.advance_if_split(|t| matches!(t, Token::Let | Token::Const));
    //     // SAFETY: This function should only be called when the next token is var, const or public
    //     let kind = kind.unwrap();
    //     let mutable = kind == Token::Let;
    //     let name = self.expect_ident(" after {kind} token in variable declaration")?;
    //     // Explicit type
    //     if self.consume_if(|t| matches!(t, Token::Colon)) {
    //         let ty = self.parse_type()?;
    //         if self.consume_if(|t| t == &Token::Semicolon) {
    //             return Some(Stmt::VarDecl {
    //                 name,
    //                 ty,
    //                 init: None,
    //                 public,
    //                 mutable,
    //             });
    //         }
    //         self.expect(&Token::Eq)?;
    //         let init = self.parse_expr(BindingPower::Lowest)?;
    //         self.expect(&Token::Semicolon)?;
    //         return Some(Stmt::VarDecl {
    //             name,
    //             ty,
    //             init: Some(init),
    //             public,
    //             mutable,
    //         });
    //     }
    //     self.expect(&Token::Eq)?;
    //     let (next, span) = self.peek_next_split();
    //     // NOTE: Currently ignores mutability, maybe this should change?
    //     match next {
    //         Some(Token::Fn) => self.parse_fn_decl().map(ast::Stmt::Function),
    //         Some(Token::Struct) => {
    //             self.parse_struct_decl()
    //                 .map(|ty| Stmt::TypeDecl { ty, public, name })
    //         }
    //         Some(Token::Enum) => {
    //             self.parse_enum_decl()
    //                 .map(|ty| Stmt::TypeDecl { ty, public, name })
    //         }
    //         unknown => {
    //             let msg = format!("Expected fn, struct or enum in struct field, found {unknown:?}");
    //             self.new_parse_error(span, msg);
    //             None
    //         }
    //     }
    // }
    // pub(crate) fn parse_block(&mut self) -> Option<Block<'s>> {
    // }
    // pub(crate) fn parse_if(&mut self) -> Option<Stmt<'s>> {
    //     self.expect(&Token::If)?;
    //     self.expect(&Token::LParen)?;
    //     let cond = self.parse_expr(BindingPower::Lowest)?;
    //     self.expect(&Token::RParen)?;
    //     let then_body = self.parse_block()?;
    //     let mut elifs = Vec::new();
    //     while self.consume_if(|t| matches!(t, Token::Elif)) {
    //         self.expect(&Token::LParen)?;
    //         let cond = self.parse_expr(BindingPower::Lowest)?;
    //         self.expect(&Token::RParen)?;
    //         let body = self.parse_block()?;
    //         elifs.push(Elif { cond, body });
    //     }
    //
    //     let mut else_body = None;
    //     if self.consume_if(|t| matches!(t, Token::Else)) {
    //         else_body = Some(self.parse_block()?);
    //     }
    //     Some(Stmt::If {
    //         cond,
    //         then_body,
    //         elifs,
    //         else_body,
    //     })
    // }
    //
    // pub(crate) fn parse_loop_init(&mut self) -> Option<Stmt<'s>> {
    //     let name = self.expect_ident(" after loop [")?;
    //     self.expect(&Token::Colon)?;
    //     let ty = self.parse_type()?;
    //     self.expect(&Token::Eq)?;
    //     let init = self.parse_expr(BindingPower::Lowest)?;
    //
    //     Some(Stmt::VarDecl {
    //         name,
    //         ty,
    //         init: Some(init),
    //         public: false,
    //         mutable: true,
    //     })
    // }
    // pub(crate) fn parse_loop(&mut self) -> Option<Stmt<'s>> {
    //     self.expect(&Token::Loop)?;
    //
    //     // Infinite loop
    //     // loop {body}
    //     if self
    //         .peek_next_split()
    //         .0
    //         .is_some_and(|t| matches!(t, Token::LBrace))
    //     {
    //         let body = self.parse_block()?;
    //         return Some(Stmt::Loop {
    //             cond: None,
    //             initializers: Block::default(),
    //             post_ops: Block::default(),
    //             body,
    //         });
    //     }
    //
    //     // For loop
    //     // loop [int a = 2, int b = 3] (a < 2) {body}
    //     // loop [int a = 0, int b = 3] (a < b): (a++) {body}
    //     if self.consume_if(|t| matches!(t, Token::LBracket)) {
    //         let inits = self.delimited_list_with_terminator(
    //             Self::parse_loop_init,
    //             &Token::Comma,
    //             &Token::RBracket,
    //         );
    //         let inits: Vec<Stmt<'s>> = inits.collect();
    //         self.expect(&Token::LParen)?;
    //         let cond = self.parse_expr(BindingPower::Lowest)?;
    //         self.expect(&Token::RParen)?;
    //
    //         let mut post_ops = Vec::new();
    //         while self.consume_if(|t| matches!(t, Token::Colon)) {
    //             self.expect(&Token::LParen)?;
    //             let op = self.parse_expr(BindingPower::Lowest)?;
    //             self.expect(&Token::RParen)?;
    //             post_ops.push(Stmt::Expr(op));
    //         }
    //
    //         let body = self.parse_block()?;
    //
    //         return Some(Stmt::Loop {
    //             cond: Some(cond),
    //             initializers: Block(inits),
    //             post_ops: Block(post_ops),
    //             body,
    //         });
    //     }
    //
    //     // While loop
    //     // loop (cond) {body}
    //     // loop (cond): (op) {body}
    //     self.expect(&Token::LParen)?;
    //     let cond = self.parse_expr(BindingPower::Lowest)?;
    //     self.expect(&Token::RParen)?;
    //
    //     let mut post_ops = Vec::new();
    //     while self.consume_if(|t| matches!(t, Token::Colon)) {
    //         self.expect(&Token::LParen)?;
    //         let op = self.parse_stmt()?;
    //         self.expect(&Token::RParen)?;
    //         post_ops.push(op);
    //     }
    //
    //     let body = self.parse_block()?;
    //
    //     Some(Stmt::Loop {
    //         cond: Some(cond),
    //         initializers: Block::default(),
    //         post_ops: Block(post_ops),
    //         body,
    //     })
    // }
    // // Ok means the value is a normal case, Err means the value is a default case
    // pub(crate) fn parse_case(&mut self) -> Option<Result<ast::SwitchCase<'s>, ast::Stmt<'s>>> {
    //     let first_case = self.parse_expr(BindingPower::Lowest)?;
    //     let mut cases = vec![first_case];
    //     while self.consume_if(|t| matches!(t, Token::Comma)) {
    //         let case = self.parse_expr(BindingPower::Lowest)?;
    //         cases.push(case);
    //     }
    //     self.expect(&Token::RArrow)?;
    //     let body = self.parse_stmt()?;
    //
    //     if matches!(cases[..], [Expr::Ident("_")]) {
    //         // Default case
    //         return Some(Err(body));
    //     }
    //
    //     Some(Ok(ast::SwitchCase { body, cases }))
    // }
    // pub(crate) fn parse_switch(&mut self) -> Option<Stmt<'s>> {
    //     self.expect(&Token::Switch)?;
    //     self.expect(&Token::LParen)?;
    //     let value = self.parse_expr(BindingPower::Lowest)?;
    //     self.expect(&Token::RParen)?;
    //
    //     self.expect(&Token::LBrace)?;
    //
    //     let mut default = None;
    //     let mut cases = Vec::new();
    //     while self
    //         .peek_next_split()
    //         .0
    //         .is_some_and(|t| !matches!(t, Token::RBrace))
    //     {
    //         let case = self.parse_case()?;
    //         match case {
    //             Err(d) => {
    //                 default = default.or(Some(Box::new(d)));
    //             }
    //             Ok(c) => cases.push(c),
    //         }
    //     }
    //     self.expect(&Token::RBrace)?;
    //
    //     Some(Stmt::Switch {
    //         value,
    //         cases,
    //         default,
    //     })
    // }
    pub fn post_error_sync(&mut self) {
        let Some(mut prev) = self.advance() else {
            return;
        };
        while let Some(tok) = self.advance() {
            use Token::*;
            if matches!(prev.inner, Token::Semicolon)
                || matches!(tok.inner, Fun | Var | For | If | While | Print | Return)
            {
                self.put_back(tok);
                return;
            }
            prev = tok;
        }
    }
    pub fn expression(&mut self) -> Option<Expr<'s>> {
        self.assignment()
    }
    fn assignment(&mut self) -> Option<Expr<'s>> {
        let expr = self.or()?;
        if let Some(eq) = self.advance_if_eq(&Token::Eq) {
            let val = self.assignment()?;
            match expr {
                Expr::Ident(target) => {
                    return Some(Expr::Assignment(Box::new(Assignment { target, val })));
                }
                invalid => {
                    self.new_parse_error(eq.span, format!("Invalid assignment target: {invalid}"));
                    return None;
                }
            }
        }

        Some(expr)
    }
    fn or(&mut self) -> Option<Expr<'s>> {
        let mut lhs = self.and()?;
        while self.consume_if_eq(&Token::Or) {
            let rhs = self.and()?;
            lhs = Expr::Binary(Box::new(BinaryExpr {
                lhs,
                op: BinaryOperator::Or,
                rhs,
            }));
        }
        Some(lhs)
    }
    fn and(&mut self) -> Option<Expr<'s>> {
        let mut lhs = self.equality()?;
        while self.consume_if_eq(&Token::And) {
            let rhs = self.and()?;
            lhs = Expr::Binary(Box::new(BinaryExpr {
                lhs,
                op: BinaryOperator::And,
                rhs,
            }));
        }
        Some(lhs)
    }
    fn equality(&mut self) -> Option<Expr<'s>> {
        let mut expr = self.comparison()?;
        while let Some((op, op_span)) = self.try_map(|tok| {
            Ok(match tok {
                Token::EqEq => BinaryOperator::Eq,
                Token::Ne => BinaryOperator::Ne,
                _ => return Err(tok),
            })
        }) {
            let Some(rhs) = self.comparison() else {
                self.new_parse_error(
                    op_span,
                    format!("Missing right-hand-side of binary operator {op:?}"),
                );
                return None;
            };
            expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }));
        }
        Some(expr)
    }
    fn comparison(&mut self) -> Option<Expr<'s>> {
        let mut expr = self.term()?;
        while let Some((op, op_span)) = self.try_map(|tok| {
            Ok(match tok {
                Token::Gt => BinaryOperator::Gt,
                Token::Ge => BinaryOperator::Ge,
                Token::Lt => BinaryOperator::Lt,
                Token::Le => BinaryOperator::Le,
                _ => return Err(tok),
            })
        }) {
            let Some(rhs) = self.term() else {
                self.new_parse_error(
                    op_span,
                    format!("Missing right-hand-side of binary operator {op:?}"),
                );
                return None;
            };
            expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }));
        }
        Some(expr)
    }
    fn term(&mut self) -> Option<Expr<'s>> {
        let mut expr = self.factor()?;
        while let Some((op, op_span)) = self.try_map(|tok| {
            Ok(match tok {
                Token::Minus => BinaryOperator::Sub,
                Token::Plus => BinaryOperator::Add,
                _ => return Err(tok),
            })
        }) {
            let Some(right) = self.factor() else {
                self.new_parse_error(
                    op_span,
                    format!("Missing right-hand-side of binary operator {op:?}"),
                );
                return None;
            };
            expr = Expr::Binary(Box::new(BinaryExpr {
                lhs: expr,
                op,
                rhs: right,
            }));
        }
        Some(expr)
    }
    fn factor(&mut self) -> Option<Expr<'s>> {
        let mut expr = self.unary()?;
        while let Some((op, op_span)) = self.try_map(|tok| {
            Ok(match tok {
                Token::Slash => BinaryOperator::Div,
                Token::Star => BinaryOperator::Mul,
                _ => return Err(tok),
            })
        }) {
            let Some(right) = self.unary() else {
                self.new_parse_error(
                    op_span,
                    format!("Missing right-hand-side of binary operator {op:?}"),
                );
                return None;
            };
            expr = Expr::Binary(Box::new(BinaryExpr {
                lhs: expr,
                op,
                rhs: right,
            }));
        }
        Some(expr)
    }
    fn unary(&mut self) -> Option<Expr<'s>> {
        use crate::Token::*;
        let peeked = self.peek_next()?;
        let op = match peeked.inner {
            Minus => UnaryOperator::Neg,
            Not => UnaryOperator::Not,
            _ => return self.primary(),
        };
        let span = peeked.span.clone();
        // consume the value we just peeked
        _ = self.advance();
        Some(match self.unary() {
            None => {
                self.new_parse_error(span, format!("Unterminated {op:?}"));
                return None;
            }
            Some(expr) => Expr::Unary(Box::new(UnaryExpr { op, val: expr })),
        })
    }
    fn primary(&mut self) -> Option<Expr<'s>> {
        use crate::Token::*;
        let SToken { inner, span } = self.advance()?;
        Some(match inner {
            IntLit(n) => Expr::Lit(LiteralExpression::Int(n)),
            FloatLit(f) => Expr::Lit(LiteralExpression::Float(f)),
            StringLit(s) => Expr::Lit(LiteralExpression::Str(s)),
            BoolLit(b) => Expr::Lit(LiteralExpression::Bool(b)),
            Nil => Expr::Lit(LiteralExpression::Nil),
            Ident(name) => Expr::Ident(name),
            LParen => {
                let Some(expr) = self.expression() else {
                    self.new_parse_error(span, "Unterminated (".to_string());
                    return None;
                };
                self.expect(&Token::RParen, "")?;
                Expr::Grouped(Box::new(expr))
            }
            unexpected => {
                self.new_parse_error(span, format!("Unexpected token {unexpected:?}"));
                return None;
            }
        })
    }
    pub(crate) fn parse_block(&mut self) -> Option<Block<'s>> {
        let mut block = Vec::new();
        while self
            .peek_next_split()
            .0
            .is_some_and(|t| !matches!(t, Token::RBrace))
        {
            let stmt = self.parse_decl()?;
            block.push(stmt);
        }
        self.expect(&Token::RBrace, " to terminate a block")?;
        Some(ast::Block(block))
    }
    pub(crate) fn parse_if(&mut self) -> Option<Stmt<'s>> {
        let cond = self.expression()?;
        self.expect(&Token::LBrace, " after if statement condition")?;
        let then_body = self.parse_block()?;
        let mut else_body = None;
        if self.consume_if_eq(&Token::Else) {
            self.expect(&Token::LBrace, " after else")?;
            else_body = Some(self.parse_block()?);
        }
        Some(Stmt::If {
            cond,
            then_body,
            else_body,
        })
    }
    pub(crate) fn parse_while(&mut self) -> Option<Stmt<'s>> {
        let cond = self.expression()?;
        self.expect(&Token::LBrace, " after while condition")?;
        let body = self.parse_block()?;
        Some(Stmt::While { cond, body })
    }
    pub(crate) fn parse_for(&mut self) -> Option<Stmt<'s>> {
        self.expect(&Token::LParen, " after for")?;
        let initializer = if self.consume_if_eq(&Token::Semicolon) {
            None
        } else if self.consume_if_eq(&Token::Var) {
            Some(self.parse_var_decl()?)
        } else {
            let expr = self.expression()?;
            self.expect(&Token::Semicolon, " after for loop initializer")?;
            Some(Decl::Stmt(Stmt::Expr(expr)))
        };
        let cond = if self.consume_if_eq(&Token::Semicolon) {
            None
        } else {
            let expr = self.expression()?;
            self.expect(&Token::Semicolon, " after for loop initializer")?;
            Some(expr)
        };
        let increment = if self.consume_if_eq(&Token::RParen) {
            None
        } else {
            let expr = self.expression()?;
            self.expect(&Token::RParen, " after for loop initializer")?;
            Some(expr)
        };
        self.expect(&Token::LBrace, " after for loop")?;
        let body = self.parse_block()?;
        Some(Stmt::For(Box::new(For {
            initializer,
            cond,
            increment,
            body,
        })))
    }
    pub(crate) fn parse_stmt(&mut self) -> Option<Stmt<'s>> {
        if self.consume_if_eq(&Token::Print) {
            let value = self.expression()?;
            self.expect(&Token::Semicolon, "");
            return Some(Stmt::Print { value });
        }
        if self.consume_if_eq(&Token::LBrace) {
            return Some(Stmt::Block(self.parse_block()?));
        }
        if self.consume_if_eq(&Token::If) {
            return self.parse_if();
        }
        if self.consume_if_eq(&Token::While) {
            return self.parse_while();
        }
        if self.consume_if_eq(&Token::For) {
            return self.parse_for();
        }
        let value = self.expression()?;
        self.expect(&Token::Semicolon, "");
        Some(Stmt::Expr(value))
        //     use Token::*;
        // let tok = self.peek_next()?;
        //     match tok.inner {
        //         Const | Let => self.parse_var_decl(public),
        //         _ if public => {
        //             let msg = format!(
        //                 "public can only be followed by let or const, not {tok}",
        //                 tok = tok.inner
        //             );
        //             let span = tok.as_span();
        //             self.new_parse_error(span, "Invalid token after public")
        //                 .set_long_message(msg);
        //             None
        //         }
        //         At("use") => self.parse_use(),
        //         Return => self.parse_return(),
        //         LBrace => self.parse_block().map(Stmt::Block),
        //         Loop => self.parse_loop(),
        //         If => self.parse_if(),
        //         Break => {
        //             _ = self.advance();
        //             self.expect(&Token::Semicolon)?;
        //             Some(Stmt::Break)
        //         }
        //         Continue => {
        //             _ = self.advance();
        //             self.expect(&Token::Semicolon)?;
        //             Some(Stmt::Continue)
        //         }
        //         Defer => {
        //             self.advance();
        //             let stmt = self.parse_stmt()?;
        //             Some(Stmt::Defer(Box::new(stmt)))
        //         }
        //         ref call @ (Output | Outputln) => {
        //             let newline = call == &Outputln;
        //             // Consume name
        //             _ = self.advance();
        //             self.expect(&Token::LParen)?;
        //             let args = self.delimited_list_with_terminator(
        //                 |p| p.parse_expr(BindingPower::Lowest),
        //                 &Token::Comma,
        //                 &Token::RParen,
        //             );
        //             let values: Vec<Expr<'s>> = args.collect();
        //             self.expect(&Token::Semicolon)?;
        //             Some(ast::Stmt::Print { values, newline })
        //         }
        //         Switch => self.parse_switch(),
        //         _ => {
        //             let expr = self.parse_expr(BindingPower::Lowest)?;
        //             self.expect(&Token::Semicolon)?;
        //             Some(Stmt::Expr(expr))
        //         }
        //     }
    }
    pub(crate) fn parse_var_decl(&mut self) -> Option<Decl<'s>> {
        let name = self.expect_ident(" in variable declaration")?;

        let mut init = None;
        if self.consume_if_eq(&Token::Eq) {
            init = Some(self.expression()?);
        }
        self.expect(&Token::Semicolon, " after variable declaration");
        Some(Decl::VarDecl { name, init })
    }
    pub(crate) fn parse_decl(&mut self) -> Option<Decl<'s>> {
        if self.consume_if_eq(&Token::Var) {
            return self.parse_var_decl();
        }
        let stmt = self.parse_stmt()?;
        Some(Decl::Stmt(stmt))
    }
    pub fn parse(&mut self) -> (Program<'s>, AggregateError) {
        //     let name = self.parse_module_header().unwrap_or("UNSPECIFIED");
        let mut declarations = Vec::new();
        while self.peek_next().is_some() {
            let Some(stmt) = self.parse_decl() else {
                continue;
            };
            declarations.push(stmt);
        }
        let components: Vec<ErrorComponent> = self
            .lexer_errors
            .components
            .drain(..)
            .chain(self.errors.components.drain(..))
            .collect();
        (Program { declarations }, AggregateError { components })
    }
}
