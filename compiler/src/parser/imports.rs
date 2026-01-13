use crate::lexer::{Lexer, TokenKind};
use crate::parser::L1Parser;
use crate::parser::error::Result;
use ast::{
    L1Ast, L1Expression, L1ExpressionInner, L1Import, L1ImportFragment, L1Statement, Symbol,
};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

pub fn process_imports(
    ast: &mut L1Ast,
    base_file_path: PathBuf,
    stdlib: Option<PathBuf>,
) -> Result<()> {
    let mut visited = HashSet::new();
    if let Ok(canonical) = std::fs::canonicalize(&base_file_path) {
        visited.insert(canonical);
    }

    let base_dir = base_file_path
        .parent()
        .unwrap_or(&std::path::Path::new("."))
        .to_path_buf();
    process_imports_recursive(ast, &base_dir, &mut visited, &stdlib)
}

fn process_imports_recursive(
    ast: &mut L1Ast,
    base_dir: &PathBuf,
    visited: &mut HashSet<PathBuf>,
    stdlib: &Option<PathBuf>,
) -> Result<()> {
    let imports = std::mem::take(&mut ast.imports);
    let imports_copy = imports.clone();

    for import in imports {
        let import_path_str = import_to_string(&import);
        let import_to_path_buf = import_to_path(&import);

        let file_path = if let L1ImportFragment::Path(ref s) = import.fragment {
            if s == "std" && stdlib.is_some() {
                let mut p = stdlib.as_ref().unwrap().clone();
                if let Some(nexts) = &import.nexts {
                    for next in nexts {
                        p.push(import_to_path(next));
                    }
                }
                p
            } else {
                base_dir.join(&import_to_path_buf)
            }
        } else {
            base_dir.join(&import_to_path_buf)
        };

        // Try file.li
        let mut candidate = file_path.with_extension("li");
        let mut canonical_path = std::fs::canonicalize(&candidate);

        // If not found, try file/mod.li
        if canonical_path.is_err() {
            candidate = file_path.join("mod.li");
            canonical_path = std::fs::canonicalize(&candidate);
        }

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
                if let Err(err) = parser.parse() {
                    eprintln!("Failed to parse imported file: {:?}", candidate);
                    eprintln!("ERROR: {err:?}");
                    continue;
                }

                let mut imported_ast = std::mem::replace(&mut parser.ast, L1Ast::new());

                let new_base_dir = path
                    .parent()
                    .unwrap_or(&std::path::Path::new("."))
                    .to_path_buf();
                process_imports_recursive(&mut imported_ast, &new_base_dir, visited, stdlib)?;
                resolve_imports(&mut imported_ast);

                for (name, mut symbol) in imported_ast.symbols {
                    let key_name = if name.starts_with("std.") {
                        name.clone()
                    } else {
                        format!("{}.{}", import_path_str, name)
                    };

                    // Update internal name of the symbol if it's a Definition (Fn/Struct/Enum)
                    // Declarations (FnDeclr) which act as bindings should keep their external name.
                    match &mut symbol {
                        Symbol::Fn(f) => f.name = key_name.clone(),
                        Symbol::Struct(s) => s.name = key_name.clone(),
                        Symbol::Enum(e) => e.name = key_name.clone(),
                        Symbol::FnDeclr(f) => {
                            // Don't prefix name for declarations, keep original C-binding name
                        }
                    }
                    ast.symbols.insert(key_name, symbol);
                }
            }
            Err(_) => {
                eprintln!(
                    "Warning: Could not resolve import: {:?} (looked at {:?} and {:?}/mod.li)",
                    import,
                    file_path.with_extension("li"),
                    file_path
                );
            }
        }
    }

    ast.imports = imports_copy;

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
    let keys: HashSet<String> = ast
        .symbols
        .keys()
        .cloned()
        .filter(|f| f.starts_with("std."))
        .collect();
    // Map from Symbol Key (fully qualified) to the Actual Name (Fn.name)
    // This allows us to resolve a call to "std.libc.puts" but rewrite it to use "puts" (if that's the real name)
    let mut key_to_realname = HashMap::new();
    for (k, v) in &ast.symbols {
        if k.starts_with("std.") {
            let real = match v {
                Symbol::Fn(f) => &f.name,
                Symbol::FnDeclr(f) => &f.name,
                Symbol::Struct(s) => &s.name,
                Symbol::Enum(e) => &e.name,
            };
            key_to_realname.insert(k.clone(), real.clone());
        }
    }

    let mut aliases = HashMap::new();
    for import in &ast.imports {
        let full_path = import_to_string(import);
        if let Some(last) = full_path.split('.').last() {
            if !last.is_empty() {
                aliases.insert(last.to_string(), full_path);
            }
        }
    }

    for symbol in ast.symbols.values_mut() {
        if let Symbol::Fn(func) = symbol {
            resolve_block(&mut func.body, &keys, &aliases, &key_to_realname);
        }
    }
}

fn resolve_block(
    block: &mut ast::L1Block,
    keys: &HashSet<String>,
    aliases: &HashMap<String, String>,
    realnames: &HashMap<String, String>,
) {
    for stmt in &mut block.statements {
        resolve_stmt(stmt, keys, aliases, realnames);
    }
}

fn resolve_stmt(
    stmt: &mut L1Statement,
    keys: &HashSet<String>,
    aliases: &HashMap<String, String>,
    realnames: &HashMap<String, String>,
) {
    match stmt {
        L1Statement::Block(b) => resolve_block(b, keys, aliases, realnames),
        L1Statement::Declaration { var: _, value } => {
            if let Some(expr) = value {
                resolve_expr(expr, keys, aliases, realnames);
            }
        }
        L1Statement::Assign { lhs, rhs } => {
            resolve_expr(lhs, keys, aliases, realnames);
            resolve_expr(rhs, keys, aliases, realnames);
        }
        L1Statement::While(w) => {
            resolve_expr(&mut w.condition, keys, aliases, realnames);
            resolve_block(&mut w.body, keys, aliases, realnames);
        }
        L1Statement::If(i) => {
            resolve_expr(&mut i.if_cond, keys, aliases, realnames);
            resolve_block(&mut i.if_block, keys, aliases, realnames);
            if let Some(b) = &mut i.else_block {
                resolve_block(b, keys, aliases, realnames);
            }
        }
        L1Statement::Return(Some(e)) => resolve_expr(e, keys, aliases, realnames),
        L1Statement::Expr(e) => resolve_expr(e, keys, aliases, realnames),
        _ => {}
    }
}

fn resolve_expr(
    expr: &mut L1Expression,
    keys: &HashSet<String>,
    aliases: &HashMap<String, String>,
    realnames: &HashMap<String, String>,
) {
    match &mut expr.expr {
        L1ExpressionInner::Array(exprs) => {
            for e in exprs {
                resolve_expr(e, keys, aliases, realnames);
            }
        }
        L1ExpressionInner::FnCall { name: _, args } => {
            for arg in args {
                resolve_expr(&mut arg.expr, keys, aliases, realnames);
            }
        }
        L1ExpressionInner::ArrayAccess { name: _, index } => {
            resolve_expr(index, keys, aliases, realnames);
        }
        L1ExpressionInner::BinOp { lhs, op: _, rhs } => {
            resolve_expr(lhs, keys, aliases, realnames);
            resolve_expr(rhs, keys, aliases, realnames);
        }
        L1ExpressionInner::StructInit { name: _, fields } => {
            for f in fields {
                resolve_expr(&mut f.expr, keys, aliases, realnames);
            }
        }
        L1ExpressionInner::FieldAccess { expr: lhs, field } => {
            resolve_expr(lhs, keys, aliases, realnames);
            resolve_expr(field, keys, aliases, realnames);
        }
        L1ExpressionInner::Deref(e) => resolve_expr(e, keys, aliases, realnames),
        L1ExpressionInner::Ref(e) => resolve_expr(e, keys, aliases, realnames),
        _ => {}
    }

    let mut transformation = None;

    if let L1ExpressionInner::FieldAccess {
        expr: lhs,
        field: rhs,
    } = &expr.expr
    {
        if let L1ExpressionInner::Variable(lhs_name) = &lhs.expr {
            if let L1ExpressionInner::FnCall { name: fname, args } = &rhs.expr {
                let direct_candidate = format!("{}.{}", lhs_name, fname);
                // Check if key exists, then get REAL name
                if keys.contains(&direct_candidate) {
                    let real_name = realnames.get(&direct_candidate).unwrap().clone();
                    transformation = Some((real_name, args.clone()));
                } else if let Some(full_prefix) = aliases.get(lhs_name) {
                    let alias_candidate = format!("{}.{}", full_prefix, fname);
                    if keys.contains(&alias_candidate) {
                        let real_name = realnames.get(&alias_candidate).unwrap().clone();
                        transformation = Some((real_name, args.clone()));
                    }
                }
            }
        }
    }

    if let Some((new_name, args)) = transformation {
        expr.expr = L1ExpressionInner::FnCall {
            name: new_name,
            args,
        };
    }
}
