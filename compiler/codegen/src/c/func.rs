use crate::c::CBackend;
use ast::L1Fn;

impl CBackend {
    pub fn l1fn_to_c(&self, l1fn: &L1Fn) -> String {
        format!(
            "{} {}{}({}) {{
                {}
            }}",
            Self::l1type_to_c_type(&l1fn.ret),
            self.prefix,
            if l1fn.name == "main" {
                "__L1_main"
            } else {
                &l1fn.name
            },
            self.l1args_to_c_function_args(&l1fn.args),
            self.l1block_to_c(&l1fn.body),
        )
    }
}
