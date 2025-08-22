use std::collections::HashMap;

pub type SymbolTable = HashMap<String, Symbol>;

#[derive(Debug)]
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
}

impl From<&Symbol> for L1Type {
    fn from(value: &Symbol) -> Self {
        match value {
            Symbol::Struct(l1_struct) => L1Type::Struct(l1_struct.name.clone()),
            Symbol::Enum(l1_enum) => todo!("L1Enum to L1type"),
            Symbol::FnDeclr(l1_fn_declr) => todo!(),
            Symbol::Fn(l1_fn) => todo!(),
        }
    }
}

impl From<Symbol> for L1Type {
    fn from(value: Symbol) -> Self {
        match value {
            Symbol::Struct(l1_struct) => L1Type::Struct(l1_struct.name.clone()),
            Symbol::Enum(l1_enum) => todo!("L1Enum to L1type"),
            Symbol::FnDeclr(l1_fn_declr) => L1Type::Fn {
                name: l1_fn_declr.name,
                args: l1_fn_declr
                    .args
                    .iter()
                    .map(|f| L1ArgField {
                        name: f.name.clone(),
                        ty: f.ty.clone(),
                    })
                    .collect(),
                ret: l1_fn_declr.ret.into(),
            },
            Symbol::Fn(l1_fn) => L1Type::Fn {
                name: l1_fn.name.clone(),
                args: l1_fn
                    .args
                    .iter()
                    .map(|f| L1ArgField {
                        name: f.name.clone(),
                        ty: f.ty.clone(),
                    })
                    .collect(),
                ret: l1_fn.ret.clone().into(),
            },
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
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Str,
    Char,
    Struct(String),
    Enum(String),
    Arr(Box<L1Type>),
    // Not a real type
    // Parser reduces this to Arr(Type) in later stages
    Variadic(Box<L1Type>),
    Fn {
        name: String,
        args: Vec<L1ArgField>,
        ret: Box<L1Type>,
    },
    Ptr(Box<L1Type>),
    Interface {
        symbols: HashMap<String, L1Type>,
    },
    Void,
    // Not a real type
    // The parser backpathes this from symbols table
    Backpatch(String),
    // Not a real type. It is inferred later
    Unknown,
}

impl<'a> From<&'a str> for L1Type {
    fn from(value: &'a str) -> Self {
        match value {
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            "i8" => Self::I8,
            "i16" => Self::I16,
            "i32" => Self::I32,
            "i64" => Self::I64,
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
}

impl L1Block {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn push(&mut self, statement: L1Statement) {
        self.statements.push(statement);
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

#[derive(Debug, Clone)]
pub enum L1ExpressionInner {
    // TODO: Add negative number types
    Int(u64),
    Float(f64),
    Str(String),
    Bool(bool),
    This, // Represented as Self in the language
    Array(Vec<L1Expression>),
    FnCall {
        name: String,
        args: Vec<L1NamedExpr>,
    },
    Variable(String),
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
