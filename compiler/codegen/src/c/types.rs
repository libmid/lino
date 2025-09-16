use ast::L1Type;

use crate::c::CBackend;

impl CBackend {
    pub fn l1type_to_c_type(ty: &L1Type) -> String {
        let suffix = String::new();
        let ty_name = match ty {
            L1Type::U8 => "uint8_t",
            L1Type::U16 => "uint16_t",
            L1Type::U32 => "uint32_t",
            L1Type::U64 => "uint64_t",
            L1Type::I8 => "int8_t",
            L1Type::I16 => "int16_t",
            L1Type::I32 => "int32_t",
            L1Type::I64 => "int64_t",
            L1Type::F32 => "folat",
            L1Type::F64 => "double",
            L1Type::Bool => "bool",
            L1Type::Str => "char*",
            L1Type::Char => "char",
            L1Type::Struct(s) => &format!("struct {s}"),
            L1Type::Enum(e) => &format!("enum {e}"),
            L1Type::Arr(l1_type) => &format!("{}*", Self::l1type_to_c_type(l1_type)),
            L1Type::Variadic(l1_type) => todo!(),
            L1Type::Fn { name, args, ret } => todo!(),
            L1Type::Ptr(l1_type) => &format!("{}*", Self::l1type_to_c_type(l1_type)),
            L1Type::Interface { symbols } => todo!(),
            L1Type::Void => "void",
            L1Type::Backpatch(_) => unreachable!(),
            L1Type::Unknown => unreachable!(),
        };

        suffix + ty_name
    }
}
