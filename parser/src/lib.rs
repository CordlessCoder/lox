#![expect(unused)]
use ast::{
    Assignment, BinaryExpr, BinaryOperator, Block, Call, Decl, Expr, For, Fun, LiteralExpression,
    LogicalExpr, LogicalOperator, Program, Stmt, UnaryExpr, UnaryOperator,
};
use diagnostics::{AggregateError, ErrorComponent};
use lexer::{SToken, Token};
use source::SourceFile;
use std::{collections::VecDeque, rc::Rc};

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
    pub fn post_error_sync(&mut self) {
        let Some(mut prev) = self.advance() else {
            return;
        };
        while let Some(tok) = self.advance() {
            use Token::*;
            if matches!(prev.inner, Token::Semicolon)
                || matches!(tok.inner, Fun | Var | For | If | While | Return)
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
            lhs = Expr::Logical(Box::new(LogicalExpr {
                lhs,
                op: LogicalOperator::Or,
                rhs,
            }));
        }
        Some(lhs)
    }
    fn and(&mut self) -> Option<Expr<'s>> {
        let mut lhs = self.equality()?;
        while self.consume_if_eq(&Token::And) {
            let rhs = self.and()?;
            lhs = Expr::Logical(Box::new(LogicalExpr {
                lhs,
                op: LogicalOperator::And,
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
            _ => return self.call(),
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
    fn call(&mut self) -> Option<Expr<'s>> {
        let mut expr = self.primary()?;
        while self.consume_if_eq(&Token::LParen) {
            // NOTE: Should there be a limit on the number of arguments that can be passed to a
            // procedure?
            let mut arguments = Vec::new();
            while self
                .peek_next()
                .is_some_and(|tok| !matches!(tok.inner, Token::RParen))
            {
                arguments.push(self.expression()?);
                if !self.consume_if_eq(&Token::Comma) {
                    break;
                }
            }
            self.expect(&Token::RParen, " after call arguments")?;
            expr = Expr::Call(Box::new(Call {
                callee: expr,
                arguments,
            }));
        }
        Some(expr)
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
        let else_body = if self.consume_if_eq(&Token::Else) {
            self.expect(&Token::LBrace, " after else")?;
            Some(self.parse_block()?)
        } else {
            None
        };
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
    pub(crate) fn parse_return(&mut self) -> Option<Stmt<'s>> {
        if self.consume_if_eq(&Token::Semicolon) {
            return Some(Stmt::Return(None));
        }
        let ret = self.expression()?;
        self.expect(&Token::Semicolon, " after return expression")?;
        Some(Stmt::Return(Some(ret)))
    }
    pub(crate) fn parse_stmt(&mut self) -> Option<Stmt<'s>> {
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
        if self.consume_if_eq(&Token::Return) {
            return self.parse_return();
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
    pub(crate) fn parse_fun_decl(&mut self) -> Option<Decl<'s>> {
        let name = self.expect_ident(" in function declaration")?;
        // NOTE: Should there be a limit on the number of arguments that can be passed to a
        // procedure?
        let mut parameters = Vec::new();
        self.expect(&Token::LParen, " after function name")?;
        while self
            .peek_next()
            .is_some_and(|tok| !matches!(tok.inner, Token::RParen))
        {
            parameters.push(self.expect_ident(" in function parameters")?);
            if !self.consume_if_eq(&Token::Comma) {
                break;
            }
        }
        self.expect(&Token::RParen, " after function parameters")?;

        self.expect(&Token::LBrace, " after function parameters")?;
        let body = self.parse_block()?;
        Some(Decl::Fun(Rc::new(Fun {
            name,
            parameters,
            body,
        })))
    }
    pub(crate) fn parse_var_decl(&mut self) -> Option<Decl<'s>> {
        let name = self.expect_ident(" in variable declaration")?;

        let init = if self.consume_if_eq(&Token::Eq) {
            Some(self.expression()?)
        } else {
            None
        };
        self.expect(&Token::Semicolon, " after variable declaration");
        Some(Decl::VarDecl { name, init })
    }
    pub(crate) fn parse_decl(&mut self) -> Option<Decl<'s>> {
        if self.consume_if_eq(&Token::Fun) {
            return self.parse_fun_decl();
        }
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
                self.post_error_sync();
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
