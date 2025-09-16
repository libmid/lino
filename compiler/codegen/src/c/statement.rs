use crate::c::CBackend;
use ast::L1Statement;

impl CBackend {
    pub fn l1statement_to_c(&self, statement: &L1Statement) -> String {
        match statement {
            L1Statement::Block(l1_block) => self.l1block_to_c(l1_block),
            L1Statement::Declaration { var, value } => {
                let mut declr = format!(
                    "{} {}{}",
                    CBackend::l1type_to_c_type(&var.ty),
                    self.prefix,
                    var.name
                );

                if let Some(value) = value {
                    let expr = self.l1expr_to_c(value);
                    declr += &format!(" = {};", expr);
                }

                declr
            }
            L1Statement::FnDef(_)
            | L1Statement::ExternFnDeclr(_)
            | L1Statement::StructDef(_)
            | L1Statement::EnumDef(_) => String::new(),
            L1Statement::Assign { lhs, rhs } => {
                format!("{} = {};", self.l1expr_to_c(lhs), self.l1expr_to_c(rhs))
            }
            L1Statement::While(l1_while) => {
                format!(
                    "while ({}) {{
                    {}
                }}",
                    self.l1expr_to_c(&l1_while.condition),
                    self.l1block_to_c(&l1_while.body)
                )
            }
            L1Statement::If(l1_if) => {
                let mut b = format!(
                    "if ({}) {{
                    {}
                }}",
                    self.l1expr_to_c(&l1_if.if_cond),
                    self.l1block_to_c(&l1_if.if_block)
                );

                if let Some(e) = &l1_if.else_block {
                    b += &format!(
                        "else {{
                        {}
                    }}",
                        self.l1block_to_c(e)
                    );
                }

                b
            }
            L1Statement::Return(l1_expression) => {
                if let Some(expr) = l1_expression {
                    format!("return {};", self.l1expr_to_c(expr))
                } else {
                    "return;".into()
                }
            }
            L1Statement::Expr(l1_expression) => format!("{};", self.l1expr_to_c(l1_expression)),
        }
    }
}
