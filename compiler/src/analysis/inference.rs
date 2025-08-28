use std::collections::HashMap;

use ast::{L1ArgField, L1Ast, L1Block, L1Expression, L1ExpressionInner, L1Struct, L1Type};
use indexmap::IndexMap;

#[derive(Debug)]
pub enum InferenceError {
    DefWithoutType,
    ArrayImparity,
    BinopImparity,
    ZeroArray,
    FieldAccessTypeImparity,
    NoSymbol,
    UndeclaredVariable,
    DerefNoPtr,
    InvalidFieldAccess,
}

pub struct Inference {
    ty_table: HashMap<String, L1Type>,
    st_lookup: HashMap<String, L1Struct>,
}

impl Inference {
    pub fn new(ast: &mut L1Ast) -> Self {
        let mut map = HashMap::new();
        let mut st_lookup = HashMap::new();
        for (_, symbol) in &ast.symbols {
            match symbol {
                ast::Symbol::Struct(l1_struct) => {
                    map.insert(
                        l1_struct.name.clone(),
                        L1Type::Struct(l1_struct.name.clone()),
                    );
                    st_lookup.insert(l1_struct.name.clone(), l1_struct.clone());
                }
                ast::Symbol::Enum(l1_enum) => {
                    map.insert(l1_enum.name.clone(), L1Type::Enum(l1_enum.name.clone()));
                }
                ast::Symbol::FnDeclr(l1_fn_declr) => {
                    map.insert(
                        l1_fn_declr.name.clone(),
                        L1Type::Fn {
                            name: l1_fn_declr.name.clone(),
                            args: l1_fn_declr
                                .args
                                .clone()
                                .into_iter()
                                .map(|x| L1ArgField {
                                    name: x.name,
                                    ty: x.ty,
                                })
                                .collect(),
                            ret: l1_fn_declr.ret.clone().into(),
                        },
                    );
                }
                ast::Symbol::Fn(l1_fn) => {
                    map.insert(
                        l1_fn.name.clone(),
                        L1Type::Fn {
                            name: l1_fn.name.clone(),
                            args: l1_fn
                                .args
                                .clone()
                                .into_iter()
                                .map(|x| L1ArgField {
                                    name: x.name,
                                    ty: x.ty,
                                })
                                .collect(),
                            ret: l1_fn.ret.clone().into(),
                        },
                    );
                }
            }
        }
        Self {
            ty_table: map,
            st_lookup,
        }
    }

    pub fn infer_types(&self, ast: &mut L1Ast) -> Result<(), InferenceError> {
        for (_, symbol) in &mut ast.symbols {
            match symbol {
                ast::Symbol::Struct(_) | ast::Symbol::Enum(_) | ast::Symbol::FnDeclr(_) => {}
                ast::Symbol::Fn(l1_fn) => {
                    let mut stack = IndexMap::new();
                    for arg in &l1_fn.args {
                        stack.insert(arg.name.clone(), arg.ty.clone());
                    }
                    self.infer_block_ty(&mut l1_fn.body, &mut stack)?;
                }
            }
        }

        Ok(())
    }

    fn infer_block_ty(
        &self,
        block: &mut L1Block,
        stack: &mut IndexMap<String, L1Type>,
    ) -> Result<(), InferenceError> {
        let mut scoped_pushes = 0;
        for statement in &mut block.statements {
            match statement {
                ast::L1Statement::Block(l1_block) => self.infer_block_ty(l1_block, stack)?,
                ast::L1Statement::Declaration { var, value } => {
                    if let Some(expr) = value {
                        if let Err(err) = self.infer_expr_ty(expr, stack) {
                            match err {
                                InferenceError::ZeroArray => {
                                    expr.ty = var.ty.clone();
                                }
                                _ => return Err(err),
                            }
                        }
                        if var.ty == L1Type::Unknown {
                            var.ty = expr.ty.clone();
                        }
                    } else if var.ty == L1Type::Unknown {
                        return Err(InferenceError::DefWithoutType);
                    }

                    let redeclr = stack.insert(var.name.clone(), var.ty.clone()).is_some();
                    if !redeclr {
                        scoped_pushes += 1;
                    }
                }
                ast::L1Statement::FnDef(_) => unreachable!(),
                ast::L1Statement::ExternFnDeclr(_) => unreachable!(),
                ast::L1Statement::StructDef(_) => unreachable!(),
                ast::L1Statement::EnumDef(_) => unreachable!(),
                ast::L1Statement::Assign { lhs, rhs } => {
                    self.infer_expr_ty(rhs, stack)?;
                    self.infer_expr_ty(lhs, stack)?;
                }
                ast::L1Statement::While(l1_while) => {
                    self.infer_expr_ty(&mut l1_while.condition, stack)?;
                    self.infer_block_ty(&mut l1_while.body, stack)?;
                }
                ast::L1Statement::If(l1_if) => {
                    self.infer_expr_ty(&mut l1_if.if_cond, stack)?;
                    self.infer_block_ty(&mut l1_if.if_block, stack)?;
                    if let Some(el) = &mut l1_if.else_block {
                        self.infer_block_ty(el, stack)?;
                    }
                }
                ast::L1Statement::Return(l1_expression) => {
                    if let Some(expr) = l1_expression {
                        self.infer_expr_ty(expr, stack)?
                    }
                }
                ast::L1Statement::Expr(l1_expression) => {
                    self.infer_expr_ty(l1_expression, stack)?
                }
            }
        }
        stack.truncate(stack.len() - scoped_pushes);
        Ok(())
    }

    fn infer_expr_ty(
        &self,
        expr: &mut L1Expression,
        stack: &IndexMap<String, L1Type>,
    ) -> Result<(), InferenceError> {
        let ty = match &mut expr.expr {
            ast::L1ExpressionInner::Int(_) => L1Type::I64,
            ast::L1ExpressionInner::Float(_) => L1Type::F64,
            ast::L1ExpressionInner::Str(_) => L1Type::Str,
            ast::L1ExpressionInner::Bool(_) => L1Type::Bool,
            ast::L1ExpressionInner::This => todo!(),
            ast::L1ExpressionInner::Array(l1_expressions) => {
                for expr in l1_expressions.iter_mut() {
                    self.infer_expr_ty(expr, stack)?;
                }

                let first = l1_expressions.first();
                if let Some(first) = first {
                    if !l1_expressions.iter().all(|elem| elem.ty == first.ty) {
                        return Err(InferenceError::ArrayImparity);
                    }
                } else {
                    return Err(InferenceError::ZeroArray);
                }

                first.unwrap().ty.clone()
            }
            ast::L1ExpressionInner::FnCall { name, args } => {
                for arg in args {
                    self.infer_expr_ty(&mut arg.expr, stack)?;
                }

                if let Some(L1Type::Fn {
                    name: _,
                    args: _,
                    ret,
                }) = self.ty_table.get(name)
                {
                    *ret.clone()
                } else {
                    return Err(InferenceError::NoSymbol);
                }
            }
            ast::L1ExpressionInner::Variable(var) => {
                if let Some(ty) = stack.get(var) {
                    ty.clone()
                } else {
                    return Err(InferenceError::UndeclaredVariable);
                }
            }
            ast::L1ExpressionInner::ArrayAccess { name, index } => {
                self.infer_expr_ty(index, stack)?;

                if let Some(ty) = stack.get(name) {
                    ty.clone()
                } else {
                    return Err(InferenceError::UndeclaredVariable);
                }
            }
            ast::L1ExpressionInner::BinOp { lhs, op: _, rhs } => {
                self.infer_expr_ty(lhs, stack)?;
                self.infer_expr_ty(rhs, stack)?;

                if let Some(ty) = L1Type::allows_binop(&lhs.ty, &rhs.ty) {
                    ty
                } else {
                    return Err(InferenceError::BinopImparity);
                }
            }
            ast::L1ExpressionInner::StructInit { name, fields } => {
                for field in fields {
                    self.infer_expr_ty(&mut field.expr, stack)?;
                }

                if let Some(ty) = self.ty_table.get(name) {
                    ty.clone()
                } else {
                    return Err(InferenceError::NoSymbol);
                }
            }
            ast::L1ExpressionInner::Field(_) => todo!(),
            ast::L1ExpressionInner::FieldAccess { expr, field } => {
                self.infer_expr_ty(expr, stack)?;

                match expr.ty.clone() {
                    L1Type::Struct(ref s) => {
                        let st = self.st_lookup.get(s).unwrap();

                        match &field.expr {
                            L1ExpressionInner::Field(v) => {
                                for f in &st.fields {
                                    if &f.name == v {
                                        field.ty = f.ty.clone();
                                        break;
                                    }
                                }
                                if field.ty == L1Type::Unknown {
                                    return Err(InferenceError::InvalidFieldAccess);
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    L1Type::Ptr(ty) => match *ty {
                        L1Type::Struct(ref s) => {
                            let st = self.st_lookup.get(s).unwrap();

                            match &field.expr {
                                L1ExpressionInner::Field(v) => {
                                    for f in &st.fields {
                                        if &f.name == v {
                                            field.ty = f.ty.clone();
                                            break;
                                        }
                                    }
                                    if field.ty == L1Type::Unknown {
                                        return Err(InferenceError::InvalidFieldAccess);
                                    }
                                }
                                _ => todo!(),
                            }
                        }
                        _ => todo!(),
                    },
                    t => todo!("{:?}", t),
                }

                field.ty.clone()
            }
            ast::L1ExpressionInner::Deref(l1_expression) => {
                self.infer_expr_ty(l1_expression, stack)?;

                match &l1_expression.ty {
                    L1Type::Ptr(ty) => *ty.clone(),
                    _ => return Err(InferenceError::DerefNoPtr),
                }
            }
            ast::L1ExpressionInner::Ref(l1_expression) => {
                self.infer_expr_ty(l1_expression, stack)?;

                L1Type::Ptr(l1_expression.ty.clone().into())
            }
            ast::L1ExpressionInner::Null => L1Type::Ptr(L1Type::Void.into()),
        };

        expr.ty = ty;

        Ok(())
    }
}
