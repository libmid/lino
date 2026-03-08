use crate::lexer::{Lexer, TokenKind};
use crate::parser::L1Parser;
use crate::parser::error::{ParserError, Result};
use crate::{Compiler, CompilerOptions};
use ast::{
    L1Ast, L1Expression, L1ExpressionInner, L1Import, L1ImportFragment, L1Module, L1Statement,
    Symbol,
};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::path::{Path, PathBuf};

pub fn process_imports(
    ast: &mut L1Ast,
    base_file_path: PathBuf,
    stdlib: Option<PathBuf>,
    compiler_options: CompilerOptions,
    parent: String,
) -> Result<()> {
    let mut visited = HashSet::new();
    if let Ok(canonical) = std::fs::canonicalize(&base_file_path) {
        visited.insert(canonical);
    }

    let base_dir = base_file_path
        .parent()
        .unwrap_or(&std::path::Path::new("."))
        .to_path_buf();

    process_imports_recursive(
        ast,
        &base_dir,
        &mut visited,
        &stdlib,
        compiler_options,
        parent,
    )
}

fn process_imports_recursive(
    ast: &mut L1Ast,
    base_dir: &PathBuf,
    visited: &mut HashSet<PathBuf>,
    stdlib: &Option<PathBuf>,
    compiler_options: CompilerOptions,
    parent: String,
) -> Result<()> {
    let imports = std::mem::take(&mut ast.imports);

    let mut import_path_strs = vec![];
    for import in imports {
        import_to_string(&import, &mut import_path_strs);
    }

    for mut import_path in import_path_strs {
        if let Some((file_path, rest)) = resolve_path_recursive(
            if let Some(stdlib) = stdlib
                && import_path.starts_with("std.")
            {
                import_path.replace_first("std.", "");
                stdlib
            } else {
                base_dir
            },
            &import_path,
        ) {
            let fc = file_path.canonicalize().unwrap();
            if visited.contains(&fc) {
                continue;
            }
            visited.insert(fc.clone());

            // FIXME: If the file is mod.li then take directory name
            let module_name = format!(
                "{parent}_{}",
                file_path
                    .file_stem()
                    .unwrap()
                    .to_os_string()
                    .into_string()
                    .unwrap()
            );
            let mut co_clone = compiler_options.clone();
            co_clone.write_to_file = false;
            co_clone.module_name = module_name.clone();
            // Compile file
            let mut compiler = Compiler::new(fc, co_clone);
            // Resolve rest
            // If rest is empty then it is a module
            if rest.is_empty() {
                ast.symbols.insert(
                    file_path
                        .file_stem()
                        .unwrap()
                        .to_os_string()
                        .into_string()
                        .unwrap(),
                    Symbol::Module(L1Module {
                        name: module_name.clone(),
                        symbols: compiler.compile().symbols,
                    }),
                );
            } else {
                todo!();
            }
        } else {
            return Err(ParserError::InvalidImport);
        }
    }

    Ok(())
}

fn import_to_string(import: &L1Import, strings: &mut Vec<String>) {
    let mut s = String::new();

    match &import.fragment {
        L1ImportFragment::Path(p) => s.push_str(p),
        L1ImportFragment::All => s.push('*'),
    }

    if let Some(nexts) = &import.nexts {
        let mut v = vec![];
        for next in nexts {
            import_to_string(&next, &mut v);
        }

        for string in &mut v {
            let mut c = s.clone();
            c.push('.');
            c.push_str(string);
            *string = c;
        }

        strings.extend(v);
    } else {
        strings.push(s);
    }
}

fn resolve_path_recursive(current_path: &Path, remaining_parts: &str) -> Option<(PathBuf, String)> {
    // FIXME: This is just a temporary hack to import mod files
    if remaining_parts.is_empty() && current_path.join("mod.li").is_file() {
        return Some((current_path.join("mod.li"), remaining_parts.to_string()));
    } else if remaining_parts.is_empty() {
        return None;
    }

    let mut parts = remaining_parts.splitn(2, '.');
    let first = parts.next()?;
    let rest = parts.next().unwrap_or("");

    let base_dir = current_path.join(first);
    let li_file = current_path.join(format!("{}.li", first));

    if li_file.is_file() {
        return Some((li_file, rest.to_string()));
    }

    if base_dir.is_dir() {
        // TODO: Module level file "mod.li"

        return resolve_path_recursive(&base_dir, rest);
    }

    None
}
