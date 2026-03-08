use ast::SymbolTable;

mod block;
mod expression;
mod fndeclr;
mod func;
mod module;
mod statement;
mod struct_gen;
mod types;

pub struct CBackend {
    module: Vec<String>,
    prefix: String,
}

impl crate::Backend for CBackend {
    fn generate(&mut self, ast: &ast::L1Ast) -> String {
        self.module.push("#include <stdint.h>".into());
        self.module.push("#include <stdbool.h>".into());
        self.module.push("#include <stddef.h>".into());
        self.module.push("#include <sys/types.h>".into());

        self.generate_symbols(&ast.symbols);

        self.module.join("\n")
    }
}

impl CBackend {
    pub fn new() -> Self {
        Self {
            prefix: String::new(),
            module: Vec::new(),
        }
    }

    pub(crate) fn generate_symbols(&mut self, symbols: &SymbolTable) {
        let mut main_fn = None;

        for (_, symbol) in symbols {
            match symbol {
                ast::Symbol::Module(l1_mod) => {
                    self.module.push(self.l1mod_to_c(&l1_mod.clone().into()));
                }
                _ => {}
            }
        }

        for (_, symbol) in symbols {
            match symbol {
                ast::Symbol::Struct(l1_struct) => {
                    self.module.push(self.l1struct_to_c(l1_struct));
                }
                ast::Symbol::Enum(_) => todo!(),
                ast::Symbol::FnDeclr(l1_fn_declr) => {
                    self.module.push(self.l1fn_extern_declr_to_c(l1_fn_declr));
                }
                _ => {}
            }
        }

        for (_, symbol) in symbols {
            match symbol {
                ast::Symbol::Fn(l1_fn) => {
                    if l1_fn.name != "main" {
                        self.module
                            .push(self.l1fn_declr_to_c(&l1_fn.clone().into()));
                    }
                }
                _ => {}
            }
        }

        for (_, symbol) in symbols {
            match symbol {
                ast::Symbol::Fn(l1_fn) => {
                    if l1_fn.name == "main" {
                        main_fn = Some(l1_fn);
                    } else {
                        self.module.push(self.l1fn_to_c(l1_fn));
                    }
                }
                _ => {}
            }
        }

        if let Some(main_fn) = main_fn {
            self.module.push(self.l1fn_to_c(main_fn));
        }
    }
}
