use crate::{Token, TokenKind, lexer::Keyword, parser::error::ParserError};

pub mod error;
mod imports;
mod l1rules;

use ast::{L1Ast, L1Statement};

use error::Result;

#[derive(Debug)]
pub struct L1Parser<'a> {
    tokens: &'a Vec<Token<'a>>,
    ast: L1Ast,
    current_tok: usize,
    saved_state: usize,
}

impl<'a> L1Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            tokens: tokens,
            ast: L1Ast::new(),
            current_tok: 0,
            saved_state: 0,
        }
    }

    pub fn snapshot(&mut self) {
        self.saved_state = self.current_tok;
    }

    pub fn rollback(&mut self) {
        self.current_tok = self.saved_state;
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
                    L1Statement::FnDef(func) => self
                        .ast
                        .symbols
                        .insert(func.name.clone(), ast::Symbol::Fn(func)),
                    L1Statement::ExternFnDeclr(func) => self
                        .ast
                        .symbols
                        .insert(func.name.clone(), ast::Symbol::FnDeclr(func)),
                    L1Statement::StructDef(strct) => self
                        .ast
                        .symbols
                        .insert(strct.name.clone(), ast::Symbol::Struct(strct)),
                    L1Statement::EnumDef(e) => self
                        .ast
                        .symbols
                        .insert(e.name.clone(), ast::Symbol::Enum(e)),
                    _ => unreachable!(),
                };
            }
            TokenKind::Keyword(Keyword::Import) => {
                let import = self.parse_import()?;

                self.ast.imports.push(import);
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

    pub fn get_ast(&mut self) -> &mut L1Ast {
        &mut self.ast
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

    fn peek_2(&self) -> Result<&Token<'a>> {
        self.tokens
            .get(self.current_tok + 1)
            .ok_or(ParserError::EOF)
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
