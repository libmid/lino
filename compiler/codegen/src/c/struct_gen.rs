use ast::{L1Arg, L1Struct};

use crate::c::CBackend;

impl CBackend {
    pub fn l1struct_to_c(&self, st: &L1Struct) -> String {
        format!(
            "
struct {}{} {{
{}
}};",
            self.prefix,
            st.name,
            self.l1args_to_c_struct_fields(&st.fields)
        )
    }

    fn l1args_to_c_struct_fields(&self, fields: &Vec<L1Arg>) -> String {
        let mut fields = fields
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
            .join(";");

        fields.push(';');
        fields
    }
}
