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

    /// No  more tokens
    EOF,
}

pub type Result<T> = std::result::Result<T, ParserError>;
