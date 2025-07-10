use crate::{
    Token, TokenKind,
    lexer::Keyword,
    parser::{
        error::ParserError,
        l1st::{L1Ast, L1Statement},
    },
};

pub mod error;
mod l1rules;
pub mod l1st;

use error::Result;

#[derive(Debug)]
pub struct L1Parser<'a> {
    tokens: &'a Vec<Token<'a>>,
    ast: L1Ast,
    current_tok: usize,
}

impl<'a> L1Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            tokens: tokens,
            ast: L1Ast::new(),
            current_tok: 0,
        }
    }

    pub fn parse(&mut self) -> Result<()> {
        loop {
            if !self.has_more() {
                break;
            }
            self.parse_next()?;
        }
        Ok(())
    }

    fn parse_next(&mut self) -> Result<()> {
        let tok = self.peek()?;
        match tok.kind {
            TokenKind::Keyword(Keyword::Def) => {
                let statement = self.parse_def()?;

                match statement {
                    L1Statement::FnDeclr(func) => self
                        .ast
                        .funcs
                        .insert(func.name.clone(), l1st::Symbol::Fn(func)),
                    _ => unreachable!(),
                };
            }
            TokenKind::Keyword(Keyword::Import) => {
                let import = self.parse_import()?;
            }
            _ => {
                return Err(ParserError::UnexpectedToken);
            }
        }
        Ok(())
    }

    pub fn _recover(&mut self, _er: ParserError) -> bool {
        // TODO: Try to recover from the error
        todo!()
    }

    pub fn get_ast(&self) -> &L1Ast {
        &self.ast
    }

    fn next(&mut self) -> Option<&Token> {
        let tok: Option<&Token<'a>> = self.tokens.get(self.current_tok);
        self.current_tok += 1;
        tok
    }

    fn has_more(&'a self) -> bool {
        self.peek().is_ok()
    }

    fn peek(&self) -> Result<&Token<'a>> {
        self.tokens.get(self.current_tok).ok_or(ParserError::EOF)
    }

    fn match_token(&mut self, kind: TokenKind) -> Result<&Token> {
        match self.next() {
            Some(tok) if tok.kind == kind => {
                return Ok(tok);
            }
            Some(tok) => Err(ParserError::NoMatch {
                expected: kind,
                found: tok.kind.clone(),
                pos: tok.pos,
            }),
            None => Err(ParserError::EOF),
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> Result<()> {
        self.match_token(TokenKind::Keyword(keyword))?;
        Ok(())
    }

    fn match_iden(&mut self) -> Result<String> {
        match self.next() {
            Some(tok) => match &tok.kind {
                TokenKind::Identifier(iden) => Ok(iden.clone()),
                _ => Err(ParserError::ExpectedIden { pos: tok.pos }),
            },
            None => Err(ParserError::EOF),
        }
    }
}
