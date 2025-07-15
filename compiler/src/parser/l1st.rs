use std::collections::HashMap;

use crate::{
    TokenKind::{self, *},
    parser::error::ParserError,
};

#[derive(Debug)]
pub struct L1Ast {
    // The reason of using a map instead of set here is to
    // allow overloading.
    pub imports: Vec<L1Import>,
    pub struct_defs: HashMap<String, Symbol>,
    pub fn_declerations: HashMap<String, Symbol>,
    pub funcs: HashMap<String, Symbol>,
}

#[derive(Debug)]
pub enum Symbol {
    Struct(L1Struct),
    FnDeclr(L1FnDeclr),
    Fn(L1Fn),
}

impl L1Ast {
    pub fn new() -> Self {
        Self {
            imports: Vec::new(),
            struct_defs: HashMap::new(),
            fn_declerations: HashMap::new(),
            funcs: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct L1Import {
    pub fragment: L1ImportFragment,
    pub nexts: Option<Vec<L1Import>>,
}

#[derive(Debug)]
pub enum L1ImportFragment {
    Path(String),
    All,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct L1Fn {
    pub name: String,
    pub generics: Vec<L1Generic>,
    pub body: L1Block,
    pub args: Vec<L1Arg>,
    pub ret: L1Type,
}

#[derive(Debug)]
pub struct L1FnDeclr {
    pub name: String,
    pub generics: Vec<L1Generic>,
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
    FnDef(L1Fn),
    ExternFnDeclr(L1FnDeclr),
    StructDef(L1Struct),
    Assign {
        lhs: L1Expression,
        rhs: L1Expression,
    },
    While(L1While),
    If(L1If),
    Return(L1Expression),
    Expr(L1Expression),
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
    Xor,
    Lsh,
    Rsh,
}

impl TryFrom<TokenKind> for BinOp {
    type Error = ParserError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        let op = match value {
            Plus => BinOp::Addition,
            Minus => BinOp::Subtraction,
            Star => BinOp::Multiplication,
            Slash => BinOp::Division,
            Percent => BinOp::Modulus,
            Lt => BinOp::LessThan,
            Lte => BinOp::LessThanOrEqual,
            Gt => BinOp::GreaterThan,
            Gte => BinOp::GreaterThanOrEqual,
            DoubleEq => BinOp::Equal,
            NotEq => BinOp::NotEqual,
            AndAnd => BinOp::And,
            And => BinOp::BitwiseAnd,
            OrOr => BinOp::Or,
            Or => BinOp::BitwiseOr,
            Xor => BinOp::Xor,
            Lsh => BinOp::Lsh,
            Rsh => BinOp::Rsh,

            _ => return Err(ParserError::InvalidBinOp),
        };

        Ok(op)
    }
}

#[derive(Debug)]
pub struct L1While {
    pub condition: L1Expression,
    pub body: L1Block,
}

#[derive(Debug)]
pub struct L1If {
    pub if_cond: L1Expression,
    pub else_block: Option<L1Block>,
    pub if_block: L1Block,
}

#[derive(Debug)]
pub struct L1NamedExpr {
    pub name: Option<String>,
    pub expr: L1Expression,
}

#[derive(Debug)]
pub struct L1Struct {
    pub name: String,
    pub fields: Vec<L1Arg>,
}
