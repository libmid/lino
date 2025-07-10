use std::collections::HashMap;

#[derive(Debug)]
pub struct L1Ast {
    // The reason of using a map instead of set here is to
    // allow overloading.
    pub funcs: HashMap<String, Symbol>,
}

#[derive(Debug)]
pub enum Symbol {
    Fn(L1Fn),
}

impl L1Ast {
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct L1Import {

}

#[derive(Debug)]
pub enum L1Type {
    HalfWord,
    Word,
    Long,
    Byte,
    F32,
    F64,
    Bool,
    Str,
    Char,
    Arr(Box<L1Type>),
    // Not a real type
    // Parser reduces this to Arr(Type) in later stages
    Variadic(Box<L1Type>),
    Fn { args: Vec<L1Type>, ret: Box<L1Type> },
    Ptr(Box<L1Type>),
    Struct { fileds: Vec<L1Type> },
    Interface { symbols: HashMap<String, L1Type> },
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
            "u8" => Self::Byte,
            "u16" => Self::HalfWord,
            "u32" => Self::Word,
            "u64" => Self::Long,
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

#[derive(Debug)]
pub struct L1Fn {
    pub name: String,
    pub generics: Vec<L1Generic>,
    pub body: L1Block,
    pub args: Vec<L1Arg>,
    pub ret: L1Type,
}

#[derive(Debug)]
pub struct L1Interface {
    pub symbols: HashMap<String, L1Type>,
}

#[derive(Debug)]
pub struct L1Generic {
    pub name: String,
    pub interfaces: Vec<L1Type>,
}

#[derive(Debug)]
pub struct L1Arg {
    pub name: String,
    pub ty: L1Type,
    pub default: Option<L1Value>,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum L1Statement {
    Block(L1Block),
    Declaration {
        var: L1Variable,
        value: Option<L1Expression>,
    },
    FnDeclr(L1Fn),
    Assign {
        lhs: L1Expression,
        rhs: L1Expression,
    },
    Return(L1Expression),
    Exr(L1Expression),
}

#[derive(Debug)]
pub struct L1Variable {
    pub name: String,
    pub ty: L1Type,
}

#[derive(Debug)]
pub struct L1Expression {
    pub ty: L1Type,
    pub expr: L1ExpressionInner,
}

#[derive(Debug)]
pub enum L1ExpressionInner {
    // TODO: Add number types
    Int(u64),
    Float(f64),
    Str(String),
    Bool(bool),
    This, // Represented as Self in the language
    Array(Vec<L1Expression>),
    FnCall {
        name: String,
        args: Vec<L1Expression>,
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
        fields: HashMap<String, L1Expression>,
    },
    FieldAccess {
        expr: Box<L1Expression>,
        field: Box<L1Expression>,
    },
}

#[derive(Debug)]
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
}
