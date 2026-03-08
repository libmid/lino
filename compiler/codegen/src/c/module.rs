use ast::L1Module;

use crate::c::CBackend;

impl CBackend {
    pub fn l1mod_to_c(&self, module: &L1Module) -> String {
        let mut cg = CBackend::new();
        cg.prefix = format!("{}_", module.name);

        cg.generate_symbols(&module.symbols);

        cg.module.join("\n")
    }
}
