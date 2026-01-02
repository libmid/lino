use crate::lexer::{Lexer, TokenKind};
use crate::parser::L1Parser;
use crate::parser::error::Result;
use ast::{L1Ast, L1Import, L1ImportFragment, Symbol, L1Statement, L1Expression, L1ExpressionInner, L1Type};
use std::collections::HashSet;
use std::path::PathBuf;

pub fn process_imports(ast: &mut L1Ast, base_file_path: PathBuf) -> Result<()> {
    let mut visited = HashSet::new();
    if let Ok(canonical) = std::fs::canonicalize(&base_file_path) {
        visited.insert(canonical);
    }
    
    let base_dir = base_file_path
        .parent()
        .unwrap_or(&std::path::Path::new("."))
        .to_path_buf();
    process_imports_recursive(ast, &base_dir, &mut visited)
}

fn process_imports_recursive(
    ast: &mut L1Ast,
    base_dir: &PathBuf,
    visited: &mut HashSet<PathBuf>,
) -> Result<()> {
    let imports = std::mem::take(&mut ast.imports);

    for import in imports {
        let import_path_str = import_to_string(&import);
        let import_rel_path = import_to_path(&import);
        let file_path = base_dir.join(&import_rel_path).with_extension("li");

        let canonical_path = std::fs::canonicalize(&file_path);

        match canonical_path {
            Ok(path) => {
                if visited.contains(&path) {
                    continue;
                }
                visited.insert(path.clone());

                let input = std::fs::read_to_string(&path).unwrap_or_default();

                let lxr = Lexer::new(&input);
                let tokens = lxr.tokenize();

                let tokens_without_whitespace: Vec<_> = tokens
                    .into_iter()
                    .filter(|token| match token.kind {
                        TokenKind::Comment(_) | TokenKind::Whitespace => false,
                        _ => true,
                    })
                    .collect();

                let mut parser = L1Parser::new(&tokens_without_whitespace);
                if let Err(_) = parser.parse() {
                    eprintln!("Failed to parse imported file: {:?}", file_path);
                    continue;
                }

                let mut imported_ast = std::mem::replace(&mut parser.ast, L1Ast::new());

                let new_base_dir = path
                    .parent()
                    .unwrap_or(&std::path::Path::new("."))
                    .to_path_buf();
                process_imports_recursive(&mut imported_ast, &new_base_dir, visited)?;

                for (name, mut symbol) in imported_ast.symbols {
                    let new_name = format!("{}.{}", import_path_str, name);
                    
                    // Update internal name of the symbol
                    match &mut symbol {
                        Symbol::Fn(f) => f.name = new_name.clone(),
                        Symbol::FnDeclr(f) => f.name = new_name.clone(),
                        Symbol::Struct(s) => s.name = new_name.clone(),
                        Symbol::Enum(e) => e.name = new_name.clone(),
                    }

                    ast.symbols.insert(new_name, symbol);
                }
            }
            Err(_) => {
                eprintln!(
                    "Warning: Could not resolve import: {:?} (looked at {:?})",
                    import, file_path
                );
            }
        }
    }

    Ok(())
}

fn import_to_path(import: &L1Import) -> PathBuf {
    let mut path = PathBuf::new();
    match &import.fragment {
        L1ImportFragment::Path(p) => path.push(p),
        L1ImportFragment::All => {}
    }

    if let Some(nexts) = &import.nexts {
        for next in nexts {
            path.push(import_to_path(next));
        }
    }
    path
}

fn import_to_string(import: &L1Import) -> String {
    let mut s = String::new();
    match &import.fragment {
        L1ImportFragment::Path(p) => s.push_str(p),
        L1ImportFragment::All => s.push('*'),
    }
    
    if let Some(nexts) = &import.nexts {
        for next in nexts {
            s.push('.');
            s.push_str(&import_to_string(next));
        }
    }
    s
}

pub fn resolve_imports(ast: &mut L1Ast) {
    let keys: HashSet<String> = ast.symbols.keys().cloned().collect();
    
    for symbol in ast.symbols.values_mut() {
        if let Symbol::Fn(func) = symbol {
             resolve_block(&mut func.body, &keys);
        }
    }
}

fn resolve_block(block: &mut ast::L1Block, keys: &HashSet<String>) {
    for stmt in &mut block.statements {
        resolve_stmt(stmt, keys);
    }
}

fn resolve_stmt(stmt: &mut L1Statement, keys: &HashSet<String>) {
    match stmt {
        L1Statement::Block(b) => resolve_block(b, keys),
        L1Statement::Declaration { var: _, value } => {
            if let Some(expr) = value {
                resolve_expr(expr, keys);
            }
        }
        L1Statement::Assign { lhs, rhs } => {
            resolve_expr(lhs, keys);
            resolve_expr(rhs, keys);
        }
        L1Statement::While(w) => {
             resolve_expr(&mut w.condition, keys);
             resolve_block(&mut w.body, keys);
        }
        L1Statement::If(i) => {
            resolve_expr(&mut i.if_cond, keys);
            resolve_block(&mut i.if_block, keys);
            if let Some(b) = &mut i.else_block {
                resolve_block(b, keys);
            }
        }
        L1Statement::Return(Some(e)) => resolve_expr(e, keys),
        L1Statement::Expr(e) => resolve_expr(e, keys),
        _ => {}
    }
}

fn resolve_expr(expr: &mut L1Expression, keys: &HashSet<String>) {
    match &mut expr.expr {
        L1ExpressionInner::Array(exprs) => {
            for e in exprs { resolve_expr(e, keys); }
        }
        L1ExpressionInner::FnCall { name: _, args } => {
            for arg in args { resolve_expr(&mut arg.expr, keys); }
        }
        L1ExpressionInner::ArrayAccess { name: _, index } => {
            resolve_expr(index, keys);
        }
        L1ExpressionInner::BinOp { lhs, op: _, rhs } => {
            resolve_expr(lhs, keys);
            resolve_expr(rhs, keys);
        }
        L1ExpressionInner::StructInit { name: _, fields } => {
             for f in fields { resolve_expr(&mut f.expr, keys); }
        }
        L1ExpressionInner::FieldAccess { expr: lhs, field } => {
             resolve_expr(lhs, keys);
             resolve_expr(field, keys);
        }
        L1ExpressionInner::Deref(e) => resolve_expr(e, keys),
        L1ExpressionInner::Ref(e) => resolve_expr(e, keys),
        _ => {}
    }

    let mut transformation = None;
    
    if let L1ExpressionInner::FieldAccess { expr: lhs, field: rhs } = &expr.expr {
        if let L1ExpressionInner::Variable(lhs_name) = &lhs.expr {
            if let L1ExpressionInner::FnCall { name: fname, args } = &rhs.expr {
                 let candidate = format!("{}.{}", lhs_name, fname);
                 if keys.contains(&candidate) {
                     // Found it!
                     transformation = Some((candidate, args.clone()));
                 }
            }
        }
    }
    
    if let Some((new_name, args)) = transformation {
        expr.expr = L1ExpressionInner::FnCall { name: new_name, args };
    }
}
