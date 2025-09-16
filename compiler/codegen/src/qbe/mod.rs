use std::collections::{HashMap, HashSet};

use crate::Backend;
use ast::{L1Arg, L1Block, L1Expression, L1ExpressionInner, L1Fn, L1Statement, L1Struct, L1Type};
use qubers::{Cmp, DataDef, DataItem, Function, Instr, Linkage, Module, Type, TypeDef, Value};

pub struct QbeBackend {
    module: Module,
    st_lookup: HashMap<String, L1Struct>,
    // current_block: Option<& mut Block>,
    data_counter: u64,
    expr_counter: u64,
    block_counter: u64,
    func: Function,
    local_defs: HashSet<String>,
}

impl Backend for QbeBackend {
    fn generate(&mut self, ast: &ast::L1Ast) -> String {
        for (_, symbol) in &ast.symbols {
            match symbol {
                ast::Symbol::Struct(l1_struct) => {
                    self.st_lookup
                        .insert(l1_struct.name.clone(), l1_struct.clone());
                }
                ast::Symbol::Enum(_) => todo!(),
                ast::Symbol::FnDeclr(_) => {}
                ast::Symbol::Fn(l1_fn) => {}
            }
        }

        for (_, symbol) in &ast.symbols {
            match symbol {
                ast::Symbol::Struct(l1_struct) => {
                    let def = self.l1struct_to_typedef(&l1_struct);
                    self.module.add_type(def);
                }
                ast::Symbol::Enum(l1_enum) => todo!(),
                ast::Symbol::FnDeclr(l1_fn_declr) => {}
                ast::Symbol::Fn(l1_fn) => {
                    self.local_defs.clear();
                    self.generate_fn(&l1_fn);
                }
            }
        }

        format!("{}", self.module)
    }
}

impl QbeBackend {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            // current_block: None,
            data_counter: 0,
            expr_counter: 0,
            block_counter: 0,
            st_lookup: HashMap::new(),
            func: Function::new(Linkage::public(), "", vec![], None),
            local_defs: HashSet::new(),
        }
    }

    fn generate_fn(&mut self, l1_fn: &L1Fn) {
        let func = Function::new(
            Linkage::public(),
            &l1_fn.name,
            self.args(&l1_fn.args),
            self.tt(&l1_fn.ret),
        );

        self.func = func;

        self.generate_function_body(l1_fn);

        let block = self.func.blocks.last_mut().unwrap();
        if !block.jumps() {
            block.add_instr(Instr::Ret(None));
        }

        self.module.add_function(self.func.clone());
    }

    fn generate_function_body(&mut self, func: &L1Fn) {
        self.func.add_block("arg.init");

        for arg in &func.args {
            let ty = self.tt(&arg.ty).unwrap().into_base();
            self.func.assign_instr(
                Value::Temporary(arg.name.clone()),
                Type::Long,
                Instr::Alloc8(self.type_base_size(&arg.ty)),
            );
            self.func.add_instr(Instr::Store(
                ty,
                Value::Temporary(arg.name.clone()),
                Value::Temporary(format!("{}.arg", arg.name)),
            ));
        }

        self.generate_block(&func.body, "func.body");
    }

    fn generate_block<'a>(&mut self, l1_block: &L1Block, label: impl Into<String>) {
        self.func.add_block(label);
        self.generate_statements(&l1_block.statements);
    }

    fn generate_statements<'a>(&mut self, statements: &Vec<L1Statement>) {
        for statement in statements {
            match statement {
                ast::L1Statement::Block(l1_block) => {
                    let l = self.new_block_name();
                    self.generate_block(l1_block, l);
                }
                ast::L1Statement::Declaration { var, value } => {
                    self.local_defs.insert(var.name.clone());

                    let mut init = Instr::Copy(Value::Const(0));
                    if let Some(expr) = value {
                        init = self.gen_expr(expr);
                    }
                    let init = self.assign_new_temp(init, self.tt(&var.ty).unwrap());

                    let addr = Value::Temporary(var.name.clone());
                    // Allocate stack space for the variable
                    self.func.assign_instr(
                        addr.clone(),
                        Type::Long,
                        Instr::Alloc8(self.type_size(&var.ty)),
                    );

                    if !matches!(var.ty, L1Type::Struct(_)) {
                        self.func
                            .add_instr(Instr::Store(init.0.into_base(), addr, init.1));
                    }
                }
                ast::L1Statement::FnDef(_) => unreachable!(),
                ast::L1Statement::ExternFnDeclr(_) => unreachable!(),
                ast::L1Statement::StructDef(_) => unreachable!(),
                ast::L1Statement::EnumDef(_) => unreachable!(),
                ast::L1Statement::Assign { lhs, rhs } => {
                    // Right now LHS is always a variable
                    // TODO: This might be array, pointer deref or field access as well

                    let rhs_expr = self.gen_expr(rhs);
                    let (rty, rhsi) = self.assign_new_temp(rhs_expr, self.tt(&rhs.ty).unwrap());

                    match &lhs.expr {
                        ast::L1ExpressionInner::Variable(v) => {
                            // if self.local_defs.contains(v) {
                            //     self.func.add_instr(Instr::Store(
                            //         rty.into_base(),
                            //         Value::Temporary(v.clone()),
                            //         rhsi,
                            //     ))
                            // } else {
                            //     self.func.assign_instr(
                            //         Value::Temporary(v.clone()),
                            //         rty,
                            //         Instr::Copy(rhsi),
                            //     );
                            // }

                            self.func.add_instr(Instr::Store(
                                rty.into_base(),
                                Value::Temporary(v.clone()),
                                rhsi,
                            ))
                        }
                        ast::L1ExpressionInner::Deref(expr) => {
                            let lhs_expr = self.gen_expr(expr);
                            let (_, lhsi) =
                                self.assign_new_temp(lhs_expr, self.tt(&lhs.ty).unwrap());

                            self.func
                                .add_instr(Instr::Store(rty.into_base(), lhsi, rhsi));
                        }
                        ast::L1ExpressionInner::FieldAccess { expr: e, field } => {
                            if let L1ExpressionInner::FieldAccess { expr, field } = &e.expr {
                                dbg!(e, field);
                            }

                            let var = e
                                .to_var_name()
                                // .unwrap_or_else(e.to_deref_var_name)
                                .expect("Not a variable or variable deref")
                                .clone();
                            match &e.ty {
                                L1Type::Struct(s) => {
                                    let offset_addr = match &e.expr {
                                        L1ExpressionInner::Variable(v) => {
                                            let st = self.st_lookup.get(s).unwrap();
                                            let offset = self
                                                .calc_offset(st, field.to_field_name().unwrap());

                                            let access = Instr::Add(
                                                Value::Temporary(var),
                                                Value::Const(offset),
                                            );
                                            let offset_addr =
                                                self.assign_new_temp(access, Type::Long);

                                            offset_addr
                                        }
                                        L1ExpressionInner::Deref(drf) => {
                                            let st = self.st_lookup.get(s).unwrap();
                                            let offset = self
                                                .calc_offset(st, field.to_field_name().unwrap());

                                            let actual_addr = self
                                                .assign_new_temp(
                                                    Instr::Load(Type::Long, Value::Temporary(var)),
                                                    Type::Long,
                                                )
                                                .1;

                                            let access =
                                                Instr::Add(actual_addr, Value::Const(offset));
                                            let offset_addr =
                                                self.assign_new_temp(access, Type::Long);

                                            offset_addr
                                        }
                                        _ => todo!(),
                                    };

                                    self.func.add_instr(Instr::Store(
                                        rty.into_base(),
                                        offset_addr.1,
                                        rhsi,
                                    ));
                                }
                                L1Type::Ptr(ptr) => match **ptr {
                                    L1Type::Struct(ref s) => {
                                        let st = self.st_lookup.get(s).unwrap();
                                        let offset =
                                            self.calc_offset(st, field.to_field_name().unwrap());

                                        let actual_addr = self
                                            .assign_new_temp(
                                                Instr::Load(Type::Long, Value::Temporary(var)),
                                                Type::Long,
                                            )
                                            .1;

                                        let access = Instr::Add(actual_addr, Value::Const(offset));
                                        let offset_addr = self.assign_new_temp(access, Type::Long);

                                        self.func.add_instr(Instr::Store(
                                            rty.into_base(),
                                            offset_addr.1,
                                            rhsi,
                                        ));
                                    }
                                    _ => {}
                                },

                                // Only struct fields can be accessed
                                t => unreachable!("{t:?}"),
                            }
                        }
                        t => todo!("{:?}", t),
                    }
                }
                ast::L1Statement::While(l1_while) => {
                    let while_block_start = self.new_block_name();
                    let post_condition_block = self.new_block_name();
                    let post_while_block = self.new_block_name();

                    self.func.add_block(while_block_start.clone());
                    let expr = self.gen_expr(&l1_while.condition);
                    let (_, v) =
                        self.assign_new_temp(expr, self.tt(&l1_while.condition.ty).unwrap());
                    self.func.add_instr(Instr::Jnz(
                        v,
                        post_condition_block.clone(),
                        post_while_block.clone(),
                    ));
                    self.generate_block(&l1_while.body, post_condition_block);
                    self.func.add_instr(Instr::Jmp(while_block_start));
                    self.func.add_block(post_while_block);
                }
                ast::L1Statement::If(l1_if) => {
                    let expr = self.gen_expr(&l1_if.if_cond);
                    let (_, v) = self.assign_new_temp(expr, self.tt(&l1_if.if_cond.ty).unwrap());

                    let if_block_label = self.new_block_name();
                    let post_if_block_label = self.new_block_name();

                    self.func.add_instr(Instr::Jnz(
                        v,
                        if_block_label.clone(),
                        post_if_block_label.clone(),
                    ));

                    self.generate_block(&l1_if.if_block, if_block_label);
                    self.func.add_block(post_if_block_label);
                }
                ast::L1Statement::Return(l1_expression) => {
                    if let Some(expr) = l1_expression {
                        let instr = self.gen_expr(expr);
                        let (_, temp) = self.assign_new_temp(instr, self.tt(&expr.ty).unwrap());
                        self.func.add_instr(Instr::Ret(Some(temp)));
                    } else {
                        self.func.add_instr(Instr::Ret(None));
                    }
                }
                ast::L1Statement::Expr(l1_expression) => {
                    let instr = self.gen_expr(l1_expression);
                    self.func.add_instr(instr);
                }
            }
        }
    }

    fn new_data_name(&mut self) -> String {
        self.data_counter += 1;
        format!("data.{}", self.data_counter)
    }

    fn new_block_name(&mut self) -> String {
        self.block_counter += 1;
        format!("block.{}", self.block_counter)
    }

    fn new_expr_name(&mut self) -> String {
        self.expr_counter += 1;
        format!("expr.{}", self.expr_counter)
    }

    fn add_new_or_existing_data(&mut self, data: &String) -> Value {
        // TODO: Doesn't account for existing data
        let name = self.new_data_name();
        self.module.add_data(DataDef::new(
            Linkage::private(),
            name.clone(),
            None,
            vec![
                (Type::Byte, DataItem::Str(data.clone())),
                (Type::Byte, DataItem::Const(0)),
            ],
        ));

        Value::Global(name)
    }

    fn assign_new_temp(&mut self, instr: Instr, ty: Type) -> (Type, Value) {
        if let Instr::Copy(val) = instr {
            return (ty, val);
        }

        let name = self.new_expr_name();

        self.func
            .assign_instr(Value::Temporary(name.clone()), ty.clone(), instr);

        (ty, Value::Temporary(name))
    }

    fn gen_expr(&mut self, expr: &L1Expression) -> Instr {
        match &expr.expr {
            ast::L1ExpressionInner::Null => Instr::Copy(Value::Const(0)),
            ast::L1ExpressionInner::Int(i) => Instr::Copy(Value::Const(*i)),
            ast::L1ExpressionInner::Float(f) => Instr::Copy(Value::Const(f.to_bits())),
            ast::L1ExpressionInner::Str(s) => {
                let data = self.add_new_or_existing_data(s);
                Instr::Copy(data)
            }
            ast::L1ExpressionInner::Bool(b) => {
                if *b {
                    Instr::Copy(Value::Const(1))
                } else {
                    Instr::Copy(Value::Const(0))
                }
            }
            ast::L1ExpressionInner::This => todo!(),
            ast::L1ExpressionInner::Array(l1_expressions) => todo!(),
            ast::L1ExpressionInner::FnCall { name, args } => {
                let mut instrs = Vec::with_capacity(args.len());
                for expr in args {
                    let instr;
                    if !matches!(expr.expr.ty, L1Type::Struct(_)) {
                        instr = self.gen_expr(&expr.expr);
                    } else {
                        instr = self.gen_expr(&L1Expression {
                            ty: L1Type::Ptr(expr.expr.ty.clone().into()),
                            expr: L1ExpressionInner::Ref(expr.expr.clone().into()),
                        });
                    }
                    let temp = self.assign_new_temp(instr, self.tt(&expr.expr.ty).unwrap());
                    instrs.push(temp);
                }

                Instr::Call(name.clone(), instrs, None)
            }
            ast::L1ExpressionInner::Variable(v) => {
                // if self.local_defs.contains(v) {
                //     Instr::Load(self.tt(&expr.ty).unwrap(), Value::Temporary(v.clone()))
                // } else {
                //     Instr::Copy(Value::Temporary(v.clone()))
                // }
                Instr::Load(
                    self.tt(&expr.ty).unwrap().into_base(),
                    Value::Temporary(v.clone()),
                )
            }
            ast::L1ExpressionInner::ArrayAccess { name, index } => todo!(),
            ast::L1ExpressionInner::BinOp { lhs, op, rhs } => {
                let exp1 = self.gen_expr(lhs);
                let (ty, v1): (Type, Value) = self.assign_new_temp(exp1, self.tt(&lhs.ty).unwrap());

                let exp2 = self.gen_expr(rhs);
                let (_, v2) = self.assign_new_temp(exp2, self.tt(&lhs.ty).unwrap());

                match op {
                    ast::BinOp::Addition => Instr::Add(v1, v2),
                    ast::BinOp::Subtraction => Instr::Sub(v1, v2),
                    ast::BinOp::Multiplication => Instr::Mul(v1, v2),
                    ast::BinOp::Division => Instr::Div(v1, v2),
                    ast::BinOp::Modulus => Instr::Rem(v1, v2),
                    ast::BinOp::LessThan => Instr::Cmp(ty, Cmp::Slt, v1, v2),
                    ast::BinOp::LessThanOrEqual => Instr::Cmp(ty, Cmp::Sle, v1, v2),
                    ast::BinOp::GreaterThan => Instr::Cmp(ty, Cmp::Sgt, v1, v2),
                    ast::BinOp::GreaterThanOrEqual => Instr::Cmp(ty, Cmp::Sge, v1, v2),
                    ast::BinOp::Equal => Instr::Cmp(ty, Cmp::Eq, v1, v2),
                    ast::BinOp::NotEqual => Instr::Cmp(ty, Cmp::Ne, v1, v2),
                    ast::BinOp::And => todo!(),
                    ast::BinOp::Or => todo!(),
                    ast::BinOp::BitwiseAnd => Instr::And(v1, v2),
                    ast::BinOp::BitwiseOr => Instr::Or(v1, v2),
                    ast::BinOp::Xor => Instr::Xor(v1, v2),
                    ast::BinOp::Lsh => Instr::Shl(v1, v2),
                    ast::BinOp::Rsh => Instr::Shr(v1, v2),
                }
            }
            ast::L1ExpressionInner::StructInit { name, fields } => {
                todo!();
            }
            ast::L1ExpressionInner::Field(_) => unreachable!(),
            ast::L1ExpressionInner::FieldAccess { expr, field } => {
                match (&expr.expr, &field.expr) {
                    (L1ExpressionInner::Field(_), L1ExpressionInner::Field(f)) => {
                        if let L1Type::Struct(s) = &expr.ty {
                            let st = self.st_lookup.get(s).unwrap();

                            let offset = self.calc_offset(st, f);
                            return Instr::Copy(Value::Const(offset));
                        }
                        unreachable!()
                    }
                    (
                        L1ExpressionInner::Field(f),
                        L1ExpressionInner::FieldAccess { expr: expr2, field },
                    ) => {
                        let offset_to_add_instr = self.gen_expr(field);
                        let offset_to_add = self.assign_new_temp(offset_to_add_instr, Type::Long);

                        let mut secondery_offset = 0;
                        match &expr2.expr {
                            L1ExpressionInner::Field(f) => {
                                if let L1Type::Struct(s) = &expr.ty {
                                    let st = self.st_lookup.get(s).unwrap();
                                    secondery_offset = self.calc_offset(st, f);
                                }
                            }
                            _ => todo!(),
                        }

                        if let L1Type::Struct(s) = &expr2.ty {
                            let st = self.st_lookup.get(s).unwrap();

                            return Instr::Add(
                                Value::Const(self.calc_offset(st, f) + secondery_offset),
                                offset_to_add.1,
                            );
                        }
                        unreachable!()
                    }
                    _ => {}
                };

                let v = match &expr.expr {
                    L1ExpressionInner::Variable(v) => v,
                    L1ExpressionInner::Deref(v) => v.to_var_name().expect("Not a variable"),
                    _ => unreachable!(),
                };

                let real_addr = self
                    .assign_new_temp(
                        Instr::Load(Type::Long, Value::Temporary(v.clone())),
                        Type::Long,
                    )
                    .1;

                match &expr.ty {
                    L1Type::Struct(s) | L1Type::Ptr(box L1Type::Struct(s)) => {
                        let st = self.st_lookup.get(s).unwrap();

                        match &field.expr {
                            L1ExpressionInner::Field(f) => {
                                let offset = self.calc_offset(st, f);
                                let access = Instr::Add(real_addr, Value::Const(offset));
                                let offset_addr = self.assign_new_temp(access, Type::Long);

                                return Instr::Load(offset_addr.0.into_base(), offset_addr.1);
                            }
                            L1ExpressionInner::FieldAccess { expr: e2, field: _ } => {
                                let f = e2.to_field_name().expect("Not a field name");
                                let offset = self.calc_offset(st, f);

                                let first_access = Instr::Add(real_addr, Value::Const(offset));
                                let first_temp = self.assign_new_temp(first_access, Type::Long);

                                let next_access_offset = self.gen_expr(field);
                                let next_access_offset_temp =
                                    self.assign_new_temp(next_access_offset, Type::Long);

                                let next_access =
                                    Instr::Add(first_temp.1, next_access_offset_temp.1);
                                let offset_addr = self.assign_new_temp(next_access, Type::Long);

                                return Instr::Load(offset_addr.0.into_base(), offset_addr.1);
                            }
                            _ => unreachable!(),
                        }
                    }
                    t => todo!("{t:?}"),
                }
            }
            ast::L1ExpressionInner::Deref(l1_expression) => {
                let expr = self.gen_expr(&l1_expression);

                let val = self.assign_new_temp(expr, self.tt(&l1_expression.ty).unwrap());

                Instr::Load(val.0.into_base(), val.1)
            }
            ast::L1ExpressionInner::Ref(l1_expression) => match &l1_expression.expr {
                ast::L1ExpressionInner::Int(i) => Instr::Copy(Value::Const(*i)),
                ast::L1ExpressionInner::Variable(var) => Instr::Copy(Value::Temporary(var.clone())),
                ast::L1ExpressionInner::Deref(l1_expression) => {
                    let expr = self.gen_expr(&l1_expression);
                    let var = self.assign_new_temp(expr, self.tt(&l1_expression.ty).unwrap());

                    Instr::Copy(var.1)
                }
                t => todo!("Can't generate reference for {t:?}"),
            },
        }
    }

    fn args(&self, args: &Vec<L1Arg>) -> Vec<(Type, Value)> {
        args.iter()
            .map(|arg| {
                (
                    self.tt(&arg.ty).unwrap(),
                    Value::Temporary(format!("{}.arg", arg.name)),
                )
            })
            .collect()
    }

    fn tt(&self, ty: &L1Type) -> Option<Type> {
        Some(match ty {
            L1Type::U8 => Type::UnsignedByte,
            L1Type::I8 => Type::SignedByte,
            L1Type::U16 => Type::UnsignedHalfword,
            L1Type::I16 => Type::SignedHalfword,
            L1Type::U32 | L1Type::I32 => Type::Word,
            L1Type::U64 | L1Type::I64 => Type::Long,
            L1Type::F32 => Type::Single,
            L1Type::F64 => Type::Double,
            L1Type::Bool => Type::Byte,
            L1Type::Str => Type::Long,
            L1Type::Char => Type::Byte,
            L1Type::Struct(s) => {
                Type::Aggregate(self.l1struct_to_typedef(self.st_lookup.get(s).unwrap()))
                // Type::Long
            }
            L1Type::Enum(_) => todo!("Codegen enums"),
            L1Type::Arr(_) => Type::Long,
            L1Type::Variadic(l1_type) => todo!(),
            L1Type::Fn { name, args, ret } => Type::Long,
            L1Type::Ptr(l1_type) => Type::Long,
            L1Type::Interface { symbols } => todo!(),
            L1Type::Void => return None,
            L1Type::Backpatch(_) => unreachable!(),
            L1Type::Unknown => unreachable!(),
        })
    }

    fn l1struct_to_typedef(&self, st: &L1Struct) -> TypeDef {
        // let mut max_size = 0;
        // for field in &st.fields {
        //     let size = self.type_size(&field.ty);
        //     if size > max_size {
        //         max_size = size;
        //     }
        // }

        TypeDef {
            name: st.name.clone(),
            align: None,
            items: st
                .fields
                .iter()
                .map(|x| (self.tt(&x.ty).unwrap(), 1))
                .collect(),
        }
    }

    fn type_size(&self, ty: &L1Type) -> u64 {
        match ty {
            L1Type::U8 | L1Type::I8 => 1,
            L1Type::U16 | L1Type::I16 => 2,
            L1Type::U32 | L1Type::I32 => 4,
            L1Type::U64 | L1Type::I64 => 8,
            L1Type::F32 => 4,
            L1Type::F64 => 8,
            L1Type::Bool => 1,
            L1Type::Char => 1,
            L1Type::Str => 8,
            L1Type::Struct(s) => {
                let st = self.st_lookup.get(s).unwrap();

                let alignment = self.alignment(st);

                alignment * st.fields.len() as u64
            }
            L1Type::Enum(_) => todo!(),
            L1Type::Ptr(l1_type) => 8,
            L1Type::Arr(l1_type) => todo!(),
            L1Type::Variadic(l1_type) => todo!(),
            L1Type::Fn { name, args, ret } => todo!(),
            L1Type::Interface { symbols } => todo!(),
            L1Type::Void => todo!(),
            L1Type::Backpatch(_) => unreachable!(),
            L1Type::Unknown => unreachable!(),
        }
    }

    fn type_base_size(&self, ty: &L1Type) -> u64 {
        match ty {
            L1Type::U8 | L1Type::I8 => 1,
            L1Type::U16 | L1Type::I16 => 2,
            L1Type::U32 | L1Type::I32 => 4,
            L1Type::U64 | L1Type::I64 => 8,
            L1Type::F32 => 4,
            L1Type::F64 => 8,
            L1Type::Bool => 1,
            L1Type::Char => 1,
            L1Type::Str => 8,
            L1Type::Struct(_) => 8,
            L1Type::Enum(_) => 8,
            L1Type::Ptr(l1_type) => 8,
            L1Type::Arr(l1_type) => todo!(),
            L1Type::Variadic(l1_type) => todo!(),
            L1Type::Fn { name, args, ret } => todo!(),
            L1Type::Interface { symbols } => todo!(),
            L1Type::Void => todo!(),
            L1Type::Backpatch(_) => unreachable!(),
            L1Type::Unknown => unreachable!(),
        }
    }

    fn calc_offset(&self, st: &L1Struct, f: &String) -> u64 {
        let mut offset = 0;
        let alignment = self.alignment(st);
        for field in &st.fields {
            if f == &field.name {
                return offset;
            }
            offset += alignment;
        }
        offset
    }

    fn alignment(&self, st: &L1Struct) -> u64 {
        let mut alignment = 0;
        for field in &st.fields {
            let size = self.type_size(&field.ty);
            if size > alignment {
                alignment = size;
            }
        }
        alignment
    }
}
