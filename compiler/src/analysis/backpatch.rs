
use ast::{L1Ast, L1Block, L1Fn, L1FnDeclr, L1Statement, L1Struct, L1Type, SymbolTable};

const RECURSION_LIMIT: u32 = 300;

#[derive(Debug)]
pub enum BackpatchError {
    NoSymbol,
    InappropriateType,
    SelfRecursive,
    CrossRecursive,
}

pub fn backpatch(ast: &mut L1Ast) -> Result<(), BackpatchError> {
    backpatch_local_types(ast)
}

pub fn backpatch_local_types(ast: &mut L1Ast) -> Result<(), BackpatchError> {
    let mut sym_table = ast.symbols.clone();
    for (_, sym) in &mut ast.symbols {
        let mut value = true;
        while value {
            match sym {
                ast::Symbol::Struct(l1_struct) => {
                    backpatch_struct(l1_struct, &mut sym_table, RECURSION_LIMIT)?;
                }
                ast::Symbol::Fn(l1_fn) => {
                    backpatch_fn(l1_fn, &mut sym_table)?;
                }
                ast::Symbol::FnDeclr(l1_fn) => {
                    backpatch_fn_declr(l1_fn, &mut sym_table)?;
                }
                ast::Symbol::Enum(_) => todo!(),
            }
            let ty = L1Type::from(sym.clone());
            value = needs_backpatch(&ty);
        }
    }

    Ok(())
}

fn backpatch_struct(
    strct: &mut L1Struct,
    symbols: &mut SymbolTable,
    recursion_limit: u32,
) -> Result<(), BackpatchError> {
    for field in &mut strct.fields {
        match &field.ty {
            L1Type::Backpatch(s) => {
                if s == &strct.name {
                    return Err(BackpatchError::SelfRecursive);
                }

                let sym = symbols.get(s);
                if let Some(sym) = sym {
                    let sym = sym.clone();
                    match sym {
                        ast::Symbol::Struct(mut l1_struct) => {
                            if recursion_limit == 0 {
                                return Err(BackpatchError::CrossRecursive);
                            }
                            backpatch_struct(&mut l1_struct, symbols, recursion_limit - 1)?;

                            // TODO: Remove this to use From<Symbol>
                            let ty = L1Type::Struct(l1_struct.name.clone());
                            field.ty = ty;

                            symbols
                                .entry(l1_struct.name.clone())
                                .insert_entry(ast::Symbol::Struct(l1_struct));
                        }
                        _ => return Err(BackpatchError::InappropriateType),
                    }
                } else {
                    return Err(BackpatchError::NoSymbol);
                }
            }
            ty => {
                if needs_backpatch(ty) {
                    let ty = backpatch_type(ty, symbols)?;
                    field.ty = ty;
                }
            }
        }
    }
    Ok(())
}

fn backpatch_fn(func: &mut L1Fn, symbols: &mut SymbolTable) -> Result<(), BackpatchError> {
    for field in &mut func.args {
        match &field.ty {
            L1Type::Backpatch(s) => {
                let sym = symbols.get(s);
                if let Some(sym) = sym {
                    let sym = sym.clone();
                    match sym {
                        ast::Symbol::Struct(mut l1_struct) => {
                            backpatch_struct(&mut l1_struct, symbols, RECURSION_LIMIT)?;

                            let ty = L1Type::Struct(l1_struct.name.clone());
                            field.ty = ty;

                            symbols
                                .entry(l1_struct.name.clone())
                                .insert_entry(ast::Symbol::Struct(l1_struct));
                        }
                        _ => return Err(BackpatchError::InappropriateType),
                    }
                } else {
                    return Err(BackpatchError::NoSymbol);
                }
            }
            ty => {
                if needs_backpatch(ty) {
                    let ty = backpatch_type(ty, symbols)?;
                    field.ty = ty;
                }
            }
        }
    }
    if needs_backpatch(&func.ret) {
        let bty = backpatch_type(&func.ret, symbols)?;
        func.ret = bty;
    }

    backpatch_block(&mut func.body, symbols)?;

    Ok(())
}

fn backpatch_block(block: &mut L1Block, symbols: &SymbolTable) -> Result<(), BackpatchError> {
    for (_, ty) in &mut block.scope {
        let bty = backpatch_type(ty, symbols)?;
        *ty = bty;
    }

    for statement in &mut block.statements {
        match statement {
            L1Statement::Declaration { var, value: _ } => {
                let bty = backpatch_type(&var.ty, symbols)?;
                var.ty = bty;
            }
            L1Statement::While(wh) => {
                backpatch_block(&mut wh.body, symbols)?;
            }
            L1Statement::If(i) => {
                backpatch_block(&mut i.if_block, symbols)?;
                if let Some(else_b) = &mut i.else_block {
                    backpatch_block(else_b, symbols)?;
                }
            }
            L1Statement::Block(block) => {
                backpatch_block(block, symbols)?;
            }
            _ => {}
        }
    }

    Ok(())
}

fn backpatch_fn_declr(
    func: &mut L1FnDeclr,
    symbols: &mut SymbolTable,
) -> Result<(), BackpatchError> {
    for field in &mut func.args {
        match &field.ty {
            L1Type::Backpatch(s) => {
                let sym = symbols.get(s);
                if let Some(sym) = sym {
                    let sym = sym.clone();
                    match sym {
                        ast::Symbol::Struct(mut l1_struct) => {
                            backpatch_struct(&mut l1_struct, symbols, RECURSION_LIMIT)?;

                            let ty = L1Type::Struct(l1_struct.name.clone());
                            field.ty = ty;

                            symbols
                                .entry(l1_struct.name.clone())
                                .insert_entry(ast::Symbol::Struct(l1_struct));
                        }
                        _ => return Err(BackpatchError::InappropriateType),
                    }
                } else {
                    return Err(BackpatchError::NoSymbol);
                }
            }
            ty => {
                if needs_backpatch(ty) {
                    let ty = backpatch_type(ty, symbols)?;
                    field.ty = ty;
                }
            }
        }
    }
    if needs_backpatch(&func.ret) {
        let bty = backpatch_type(&func.ret, symbols)?;
        func.ret = bty;
    }
    Ok(())
}

fn backpatch_type(ty: &L1Type, symbols: &SymbolTable) -> Result<L1Type, BackpatchError> {
    match ty {
        L1Type::Ptr(ty) => Ok(L1Type::Ptr(Box::new(backpatch_type(&*ty, symbols)?))),
        L1Type::Arr(ty) => Ok(L1Type::Arr(Box::new(backpatch_type(&*ty, symbols)?))),

        L1Type::Backpatch(name) => {
            let symbol = symbols.get(name);
            if let Some(symbol) = symbol {
                Ok(symbol.into())
            } else {
                Err(BackpatchError::NoSymbol)
            }
        }
        ty => Ok(ty.clone()),
    }
}

fn needs_backpatch(ty: &L1Type) -> bool {
    match ty {
        L1Type::U8
        | L1Type::U16
        | L1Type::U32
        | L1Type::U64
        | L1Type::I8
        | L1Type::I16
        | L1Type::I32
        | L1Type::I64
        | L1Type::F32
        | L1Type::F64
        | L1Type::Bool
        | L1Type::Str
        | L1Type::Char
        | L1Type::Void
        | L1Type::Struct(_)
        | L1Type::Unknown => false,

        L1Type::Arr(ty) => needs_backpatch(ty),
        L1Type::Ptr(ty) => needs_backpatch(ty),
        L1Type::Fn { name: _, args, ret } => {
            for arg in args {
                if needs_backpatch(&arg.ty) {
                    return true;
                }
            }

            needs_backpatch(ret)
        }
        L1Type::Variadic(ty) => needs_backpatch(ty),
        L1Type::Backpatch(_) => true,
        _ => todo!(),
    }
}
