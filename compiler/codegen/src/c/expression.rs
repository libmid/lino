use crate::c::CBackend;
use ast::{L1Expression, L1NamedExpr, L1Type};

impl CBackend {
    pub fn l1expr_to_c(&self, expr: &L1Expression) -> String {
        match &expr.expr {
            ast::L1ExpressionInner::Int(i) => i.to_string(),
            ast::L1ExpressionInner::Float(f) => f.to_string(),
            ast::L1ExpressionInner::Str(s) => format!("\"{s}\""),
            ast::L1ExpressionInner::Bool(b) => b.to_string(),
            ast::L1ExpressionInner::Null => "0".to_owned(),
            ast::L1ExpressionInner::This => "this".to_owned(),
            ast::L1ExpressionInner::Array(l1_expressions) => {
                format!(
                    "{{{}}}",
                    l1_expressions
                        .iter()
                        .map(|expr| self.l1expr_to_c(expr))
                        .collect::<Vec<String>>()
                        .join("\n")
                )
            }
            ast::L1ExpressionInner::FnCall { name, args } => {
                format!(
                    "{}{}({})",
                    self.prefix,
                    name,
                    args.iter()
                        .map(|expr| { self.l1expr_to_c(&expr.expr) })
                        .collect::<Vec<String>>()
                        .join(",")
                )
            }
            ast::L1ExpressionInner::Variable(v) => format!("{}{}", self.prefix, v),
            ast::L1ExpressionInner::Field(f) => format!("{}{}", self.prefix, f),
            ast::L1ExpressionInner::ArrayAccess { name, index } => {
                format!("{}{}[{}]", self.prefix, name, self.l1expr_to_c(index))
            }
            ast::L1ExpressionInner::BinOp { lhs, op, rhs } => {
                let lhs = self.l1expr_to_c(lhs);
                let rhs = self.l1expr_to_c(rhs);

                format!("{} {} {}", lhs, op.to_string(), rhs)
            }
            ast::L1ExpressionInner::StructInit { name: _, fields } => {
                format!(
                    "{{
                    {}
                }}",
                    self.l1named_expr_to_c_struct_fields(fields)
                )
            }
            ast::L1ExpressionInner::FieldAccess { expr, field } => {
                if let L1Type::Ptr(_) = expr.ty {
                    format!("(*{}).{}", self.l1expr_to_c(expr), self.l1expr_to_c(field))
                } else {
                    format!("{}.{}", self.l1expr_to_c(expr), self.l1expr_to_c(field))
                }
            }
            ast::L1ExpressionInner::Deref(l1_expression) => {
                format!("(*({}))", self.l1expr_to_c(l1_expression))
            }
            ast::L1ExpressionInner::Ref(l1_expression) => {
                format!("(&{})", self.l1expr_to_c(l1_expression))
            }
        }
    }

    fn l1named_expr_to_c_struct_fields(&self, fields: &Vec<L1NamedExpr>) -> String {
        fields
            .iter()
            .map(|field| {
                format!(
                    ".{}{} = {}",
                    self.prefix,
                    field
                        .name
                        .as_ref()
                        .expect("Only named expressions are supported in struct inits"),
                    self.l1expr_to_c(&field.expr)
                )
            })
            .collect::<Vec<String>>()
            .join(",\n")
    }
}
