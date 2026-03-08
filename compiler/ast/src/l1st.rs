use std::collections::HashMap;

pub type SymbolTable = HashMap<String, Symbol>;

use L1Type::*;

#[derive(Debug, Clone)]
pub struct L1Ast {
    // The reason of using a map instead of set here is to
    // allow overloading.
    pub imports: Vec<L1Import>,
    pub symbols: SymbolTable,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Struct(L1Struct),
    Enum(L1Enum),
    FnDeclr(L1FnDeclr),
    Fn(L1Fn),
    Module(L1Module),
}

impl From<&Symbol> for L1Type {
    fn from(value: &Symbol) -> Self {
        match value {
            Symbol::Struct(l1_struct) => L1Type::Struct(l1_struct.name.clone()),
            Symbol::Enum(_l1_enum) => todo!("L1Enum to L1type"),
            Symbol::FnDeclr(l1_fn_declr) => L1Type::Fn {
                name: l1_fn_declr.name.clone(),
                args: l1_fn_declr.args.iter().map(|f| f.ty.clone()).collect(),
                ret: Box::new(l1_fn_declr.ret.clone()),
                extrn: true,
            },
            Symbol::Fn(l1_fn) => L1Type::Fn {
                name: l1_fn.name.clone(),
                args: l1_fn.args.iter().map(|f| f.ty.clone()).collect(),
                ret: Box::new(l1_fn.ret.clone()),
                extrn: false,
            },
            Symbol::Module(l1_module) => L1Type::Module(l1_module.name.clone()),
        }
    }
}

impl From<Symbol> for L1Type {
    fn from(value: Symbol) -> Self {
        match value {
            Symbol::Struct(l1_struct) => L1Type::Struct(l1_struct.name.clone()),
            Symbol::Enum(_l1_enum) => todo!("L1Enum to L1type"),
            Symbol::FnDeclr(l1_fn_declr) => L1Type::Fn {
                name: l1_fn_declr.name,
                args: l1_fn_declr.args.iter().map(|f| f.ty.clone()).collect(),
                ret: l1_fn_declr.ret.into(),
                extrn: true,
            },
            Symbol::Fn(l1_fn) => L1Type::Fn {
                name: l1_fn.name.clone(),
                args: l1_fn.args.iter().map(|f| f.ty.clone()).collect(),
                ret: l1_fn.ret.clone().into(),
                extrn: false,
            },
            Symbol::Module(l1_module) => L1Type::Module(l1_module.name),
        }
    }
}

impl L1Ast {
    pub fn new() -> Self {
        Self {
            imports: Vec::new(),
            symbols: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct L1Module {
    pub name: String,
    pub symbols: SymbolTable,
}

#[derive(Debug, Clone)]
pub struct L1Import {
    pub fragment: L1ImportFragment,
    pub nexts: Option<Vec<L1Import>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum L1ImportFragment {
    Path(String),
    All,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum L1Type {
    U8,
    U16,
    U32,
    U64,
    USize,
    I8,
    I16,
    I32,
    I64,
    ISize,
    F32,
    F64,
    Bool,
    Str,
    Char,
    Module(String),
    Struct(String),
    Enum(String),
    Arr(Box<L1Type>),
    // Not a real type
    // Parser reduces this to Arr(Type) in later stages
    Variadic(Box<L1Type>),
    Ty(Box<L1Type>),
    Fn {
        name: String,
        args: Vec<L1Type>,
        ret: Box<L1Type>,
        extrn: bool,
    },
    Ptr(Box<L1Type>),
    Interface {
        symbols: HashMap<String, L1Type>,
    },
    Void,
    SSelf,
    // Not a real type
    // The parser backpathes this from symbols table
    Backpatch(String),
    // Not a real type. It is inferred later
    Unknown,
}

impl L1Type {
    pub fn allows_binop(lhs: &Self, rhs: &Self) -> Option<Self> {
        match lhs {
            Ptr(_) | USize | U64 | U32 | U16 | U8 | ISize | I64 | I32 | I16 | I8 => match rhs {
                Ptr(_) | USize | U64 | U32 | U16 | U8 | ISize | I64 | I32 | I16 | I8 => {
                    Some(lhs.clone())
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn can_assign(&self, value: &Self) -> bool {
        match (self, value) {
            (Ptr(_), Ptr(_)) => true,
            (t1, t2) if t1 == t2 => true,
            (Ptr(_), U64 | U32 | U16 | U8) => true,
            (U64, Ptr(_)) => true,
            _ => false,
        }
    }
}

impl<'a> From<&'a str> for L1Type {
    fn from(value: &'a str) -> Self {
        match value {
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            "usize" => Self::USize,
            "i8" => Self::I8,
            "i16" => Self::I16,
            "i32" => Self::I32,
            "i64" => Self::I64,
            "isize" => Self::ISize,
            "f32" => Self::F32,
            "f64" => Self::F64,
            "bool" => Self::Bool,
            "char" => Self::Char,
            "str" => Self::Str,
            "void" => Self::Void,
            _ => Self::Backpatch(value.to_string()),
        }
    }
}

impl ToString for L1Type {
    fn to_string(&self) -> String {
        match self {
            U8 => "u8".into(),
            U16 => "u16".into(),
            U32 => "u32".into(),
            U64 => "u64".into(),
            USize => "usize".into(),
            I8 => "i8".into(),
            I16 => "i16".into(),
            I32 => "i32".into(),
            I64 => "i64".into(),
            ISize => "isize".into(),
            F32 => "f32".into(),
            F64 => "f64".into(),
            Bool => "bool".into(),
            Str => "str".into(),
            Char => "char".into(),
            Module(m) => format!("L1_MODULE_{m}"),
            Struct(s) => s.into(),
            Enum(e) => e.into(),
            Arr(l1_type) => format!("L1ARRAY_{}", l1_type.to_string()),
            Variadic(l1_type) => format!("L1VARIADIC_{}", l1_type.to_string()),
            Fn {
                name,
                args,
                ret: _,
                extrn,
            } => {
                format!(
                    "L1FN_{name}_{}",
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join("_")
                )
            }
            Ptr(l1_type) => format!("L1PTR_{}", l1_type.to_string()),
            Interface { symbols } => todo!(),
            Void => "void".into(),
            SSelf => "self".into(),
            Backpatch(b) => b.into(),
            Unknown => unreachable!(),
            Ty(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct L1StructField {
    pub name: String,
    pub ty: L1Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct L1ArgField {
    pub name: String,
    pub ty: L1Type,
}

#[derive(Debug, Clone)]
pub struct L1Fn {
    pub name: String,
    pub generics: Vec<L1Generic>,
    pub body: L1Block,
    pub args: Vec<L1Arg>,
    pub ret: L1Type,
}

#[derive(Debug, Clone)]
pub struct L1FnDeclr {
    pub name: String,
    pub generics: Vec<L1Generic>,
    pub args: Vec<L1Arg>,
    pub ret: L1Type,
}

impl From<L1Fn> for L1FnDeclr {
    fn from(value: L1Fn) -> Self {
        Self {
            name: value.name,
            generics: value.generics,
            args: value.args,
            ret: value.ret,
        }
    }
}

#[derive(Debug, Clone)]
pub struct L1Interface {
    pub symbols: HashMap<String, L1Type>,
}

#[derive(Debug, Clone)]
pub struct L1Generic {
    pub name: String,
    pub interfaces: Vec<L1Type>,
}

#[derive(Debug, Clone)]
pub struct L1Arg {
    pub name: String,
    pub ty: L1Type,
    pub default: Option<L1Value>,
}

#[derive(Debug, Clone)]
pub enum L1Value {
    U64(u64),
    I64(i64),
    Bool(bool),
    Str(String),
    Arr(Vec<L1Value>),
    Fn(L1Fn),
    Ptr(Box<L1Value>),
    // struct here
}

#[derive(Debug, Clone)]
pub struct L1Block {
    pub statements: Vec<L1Statement>,
    pub scope: HashMap<String, L1Type>,
}

impl L1Block {
    pub fn new(current_scope: HashMap<String, L1Type>) -> Self {
        Self {
            statements: Vec::new(),
            scope: current_scope,
        }
    }

    pub fn push(&mut self, statement: L1Statement) {
        self.statements.push(statement);
    }

    pub fn scope(&mut self, name: String, ty: L1Type) {
        self.scope.insert(name, ty);
    }
}

#[derive(Debug, Clone)]
pub enum L1Statement {
    Block(L1Block),
    Declaration {
        var: L1Variable,
        value: Option<L1Expression>,
    },
    FnDef(L1Fn),
    ExternFnDeclr(L1FnDeclr),
    StructDef(L1Struct),
    MethodDef {
        on: L1Type,
        defs: Vec<L1Statement>,
    },
    EnumDef(L1Enum),
    Assign {
        lhs: L1Expression,
        rhs: L1Expression,
    },
    While(L1While),
    If(L1If),
    Return(Option<L1Expression>),
    Expr(L1Expression),
}

#[derive(Debug, Clone)]
pub struct L1Variable {
    pub name: String,
    pub ty: L1Type,
}

#[derive(Debug, Clone)]
pub struct L1Expression {
    pub ty: L1Type,
    pub expr: L1ExpressionInner,
}

impl L1Expression {
    pub fn to_field_name(&self) -> Result<&String, ()> {
        match &self.expr {
            L1ExpressionInner::Field(f) => Ok(f),
            _ => Err(()),
        }
    }

    pub fn to_var_name(&self) -> Result<&String, ()> {
        match &self.expr {
            L1ExpressionInner::Variable(f) => Ok(f),
            _ => Err(()),
        }
    }

    pub fn to_deref_var_name(&self) -> Result<&String, ()> {
        match &self.expr {
            L1ExpressionInner::Deref(e) => match &e.expr {
                L1ExpressionInner::Variable(v) => Ok(v),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum L1ExpressionInner {
    // TODO: Add negative number types
    Int(u64),
    Float(f64),
    Str(String),
    Bool(bool),
    Null,
    This, // Represented as Self in the language
    Array(Vec<L1Expression>),
    FnCall {
        name: String,
        args: Vec<L1NamedExpr>,
        extrn: bool,
    },
    Variable(String),
    Field(String),
    ArrayAccess {
        name: String,
        index: Box<L1Expression>,
    },
    BinOp {
        lhs: Box<L1Expression>,
        op: BinOp,
        rhs: Box<L1Expression>,
    },
    StructInit {
        name: String,
        fields: Vec<L1NamedExpr>,
    },
    FieldAccess {
        expr: Box<L1Expression>,
        field: Box<L1Expression>,
    },
    Deref(Box<L1Expression>),
    Ref(Box<L1Expression>),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    /// "+"
    Addition,
    /// "-"
    Subtraction,
    /// "*"
    Multiplication,
    /// "/"
    Division,
    /// "%"
    Modulus,
    /// "<"
    LessThan,
    /// '<=
    LessThanOrEqual,
    /// ">"
    GreaterThan,
    /// ">="
    GreaterThanOrEqual,
    /// "=="
    Equal,
    /// "!="
    NotEqual,
    /// "&&"
    And,
    /// "&"
    BitwiseAnd,
    /// "||"
    Or,
    /// "||"
    BitwiseOr,
    Xor,
    Lsh,
    Rsh,
}

impl ToString for BinOp {
    fn to_string(&self) -> String {
        match self {
            BinOp::Addition => "+",
            BinOp::Subtraction => "-",
            BinOp::Multiplication => "*",
            BinOp::Division => "/",
            BinOp::Modulus => "%",
            BinOp::LessThan => "<",
            BinOp::LessThanOrEqual => "<=",
            BinOp::GreaterThan => ">",
            BinOp::GreaterThanOrEqual => ">=",
            BinOp::Equal => "==",
            BinOp::NotEqual => "!=",
            BinOp::And => "&&",
            BinOp::BitwiseAnd => "&",
            BinOp::Or => "||",
            BinOp::BitwiseOr => "|",
            BinOp::Xor => "^",
            BinOp::Lsh => "<<",
            BinOp::Rsh => ">>",
        }
        .into()
    }
}

#[derive(Debug, Clone)]
pub struct L1While {
    pub condition: L1Expression,
    pub body: L1Block,
}

#[derive(Debug, Clone)]
pub struct L1If {
    pub if_cond: L1Expression,
    pub else_block: Option<L1Block>,
    pub if_block: L1Block,
}

#[derive(Debug, Clone)]
pub struct L1NamedExpr {
    pub name: Option<String>,
    pub expr: L1Expression,
}

#[derive(Debug, Clone)]
pub struct L1Struct {
    pub name: String,
    pub generics: Vec<L1Generic>,
    pub fields: Vec<L1Arg>,
}

#[derive(Debug, Clone)]
pub struct L1Enum {
    pub name: String,
    pub generics: Vec<L1Generic>,
    pub variants: Vec<L1EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct L1EnumVariant {
    pub name: String,
    pub ty: Option<L1Type>,
}
