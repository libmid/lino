mod block;
mod expression;
mod fndeclr;
mod func;
mod statement;
mod struct_gen;
mod types;

pub struct CBackend {
    module: Vec<String>,
    prefix: &'static str,
}

impl crate::Backend for CBackend {
    fn generate(&mut self, ast: &ast::L1Ast) -> String {
        self.module.push("#include <stdint.h>".into());
        self.module.push("#include <stdbool.h>".into());

        let mut main_fn = None;

        for (_, symbol) in &ast.symbols {
            match symbol {
                ast::Symbol::Struct(l1_struct) => {
                    self.module.push(self.l1struct_to_c(l1_struct));
                }
                ast::Symbol::Enum(_) => todo!(),
                ast::Symbol::FnDeclr(l1_fn_declr) => {
                    self.module.push(self.l1fn_declr_to_c(l1_fn_declr));
                }
                _ => {}
            }
        }

        for (_, symbol) in &ast.symbols {
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

        self.module.join("\n")
    }
}

impl CBackend {
    pub fn new() -> Self {
        Self {
            prefix: "",
            module: Vec::new(),
        }
    }
}
