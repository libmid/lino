use ast::{L1Ast, L1Block, L1Expression, L1ExpressionInner, L1NamedExpr, L1Type};

pub(crate) fn patch_method_calls(ast: &mut L1Ast) {
    for (_, sym) in &mut ast.symbols {
        match sym {
            ast::Symbol::Fn(lfn) => {
                patch_method_calls_block(&mut lfn.body);
            }
            _ => {}
        }
    }
}

fn patch_method_calls_block(block: &mut L1Block) {
    for statement in &mut block.statements {
        match statement {
            ast::L1Statement::Block(l1_block) => patch_method_calls_block(l1_block),
            ast::L1Statement::Declaration { var: _, value } => {
                if let Some(value) = value {
                    patch_method_calls_expr(value);
                }
            }
            ast::L1Statement::FnDef(_) => unreachable!(),
            ast::L1Statement::ExternFnDeclr(_) => unreachable!(),
            ast::L1Statement::StructDef(_) => unreachable!(),
            ast::L1Statement::EnumDef(_) => unreachable!(),
            ast::L1Statement::MethodDef { on: _, defs: _ } => unreachable!(),
            ast::L1Statement::Assign { lhs, rhs } => {
                patch_method_calls_expr(lhs);
                patch_method_calls_expr(rhs);
            }
            ast::L1Statement::While(l1_while) => {
                patch_method_calls_block(&mut l1_while.body);
            }
            ast::L1Statement::If(l1_if) => {
                patch_method_calls_expr(&mut l1_if.if_cond);
                patch_method_calls_block(&mut l1_if.if_block);
                if let Some(else_block) = &mut l1_if.else_block {
                    patch_method_calls_block(else_block);
                }
            }
            ast::L1Statement::Return(l1_expression) => {
                if let Some(ret_expr) = l1_expression {
                    patch_method_calls_expr(ret_expr);
                }
            }
            ast::L1Statement::Expr(l1_expression) => {
                patch_method_calls_expr(l1_expression);
            }
        }
    }
}

fn patch_method_calls_expr(expr: &mut L1Expression) {
    match &mut expr.expr {
        ast::L1ExpressionInner::Array(l1_expressions) => {
            for expr in l1_expressions {
                patch_method_calls_expr(expr);
            }
        }
        ast::L1ExpressionInner::FnCall { name: _, args } => {
            for arg in args {
                patch_method_calls_expr(&mut arg.expr);
            }
        }
        ast::L1ExpressionInner::ArrayAccess { name: _, index } => {
            patch_method_calls_expr(index);
        }
        ast::L1ExpressionInner::BinOp { lhs, op, rhs } => {
            patch_method_calls_expr(lhs);
            patch_method_calls_expr(rhs);
        }
        ast::L1ExpressionInner::StructInit { name: _, fields } => {
            for field in fields {
                patch_method_calls_expr(&mut field.expr);
            }
        }
        ast::L1ExpressionInner::FieldAccess { expr: expr1, field } => {
            match (&expr1.ty, field.expr.clone()) {
                (
                    L1Type::Ty(box L1Type::Struct(st_name)),
                    L1ExpressionInner::FnCall {
                        name: fn_name,
                        args,
                    },
                ) => {
                    expr.expr = L1ExpressionInner::FnCall {
                        name: format!("{st_name}_{fn_name}"),
                        args,
                    }
                }
                (
                    L1Type::Struct(st_name),
                    L1ExpressionInner::FnCall {
                        name: fn_name,
                        mut args,
                    },
                ) => {
                    args.insert(
                        0,
                        L1NamedExpr {
                            name: Some("self".to_string()),
                            expr: L1Expression {
                                ty: L1Type::Struct(st_name.clone()),
                                expr: L1ExpressionInner::Variable(
                                    expr1.to_var_name().unwrap().clone(),
                                ),
                            },
                        },
                    );
                    expr.expr = L1ExpressionInner::FnCall {
                        name: format!("{st_name}_{fn_name}"),
                        args,
                    }
                }
                _ => {
                    patch_method_calls_expr(field);
                }
            }
        }
        ast::L1ExpressionInner::Deref(l1_expression) => patch_method_calls_expr(l1_expression),
        ast::L1ExpressionInner::Ref(l1_expression) => patch_method_calls_expr(l1_expression),
        _ => {}
    }
}
