use crate::TokenKind;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken,
    NoMatch {
        expected: TokenKind,
        found: TokenKind,
        pos: usize,
    },
    ExpectedIden {
        pos: usize,
    },
    InvalidDef {
        msg: &'static str,
        pos: usize,
    },
    InvalidType,
    InvalidLiteral,
    InvalidStatement,
    IntParseError,
    InvalidImport,
    InvalidCall,
    InvalidExpr,
    InvalidBinOp,

    /// No  more tokens
    EOF,
}

#[derive(Debug)]
pub struct ErrorState {}

#[derive(Debug)]
pub struct ParserErrorT {
    parser_state: ErrorState,
    ty: ParserError,
}

pub type Result<T> = std::result::Result<T, ParserError>;
