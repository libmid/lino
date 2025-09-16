
use ast::{L1Arg, L1FnDeclr};

use crate::c::CBackend;

impl CBackend {
    pub fn l1fn_declr_to_c(&self, fn_declr: &L1FnDeclr) -> String {
        format!(
            "{} {}({});",
            Self::l1type_to_c_type(&fn_declr.ret),
            fn_declr.name,
            self.l1args_to_c_function_args(&fn_declr.args),
        )
    }

    pub fn l1args_to_c_function_args(&self, fields: &Vec<L1Arg>) -> String {
        let args = fields
            .iter()
            .map(|field| {
                format!(
                    "{} {}{}",
                    Self::l1type_to_c_type(&field.ty),
                    self.prefix,
                    field.name
                )
            })
            .collect::<Vec<String>>()
            .join(",");

        args
    }
}
