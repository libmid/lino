use std::{
    path::PathBuf,
};

use crate::parser::error::{ParserError, Result};
use ast::{L1Import, L1ImportFragment};

pub struct ImportResolver {
    stdlib: PathBuf,
    modules: Vec<String>,
}

impl ImportResolver {
    pub fn new(stdlib: Option<PathBuf>) -> std::io::Result<Self> {
        let stdlib = stdlib.unwrap_or("stdlib".into());
        // let modules = read_dir(stdlib)?.map(|e| {
        //     if let Ok(e) = e {
        //         if e.path().is_dir() {}
        //     }
        // });

        Ok(Self {
            stdlib,
            modules: vec![],
        })
    }
}

pub struct ImportResolve {
    files: Option<Vec<String>>,
    symbols: Option<Vec<String>>,
}

pub fn resolve_imports(imports: &Vec<L1Import>) -> Result<ImportResolve> {
    // Make sure first fragment isn't "*"
    // Prevents `import *;`
    for import in imports {
        if import.fragment == L1ImportFragment::All {
            return Err(ParserError::InvalidImport);
        }
    }

    for import in imports {
        // TODO: Remove this unnecessary string conversion
        if import.fragment == L1ImportFragment::Path("std".to_string()) {
            resolve_std_import(import);
        }
    }

    todo!()
}

fn resolve_std_import(import: &L1Import) -> ImportResolve {
    if let Some(nexts) = &import.nexts {
        for next in nexts {
            resolve_sub_std_imports(next);
        }

        todo!()
    } else {
        // Just "import std;"
        ImportResolve {
            files: None,
            symbols: Some(vec!["std".to_string()]),
        }
    }
}

fn resolve_sub_std_imports(import: &L1Import) {}
