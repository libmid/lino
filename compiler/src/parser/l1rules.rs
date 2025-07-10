use crate::{
    lexer::{Keyword, TokenKind::*, Value},
    parser::{
        L1Parser,
        error::{ParserError, Result},
        l1st::{
            L1Arg, L1Block, L1Expression, L1ExpressionInner, L1Fn, L1Generic, L1Import,
            L1Statement, L1Type, L1Value, L1Variable,
        },
    },
};

impl<'a> L1Parser<'a> {
    pub fn parse_import(&mut self) -> Result<L1Import> {
        self.match_keyword(Keyword::Import)?;
        let import = self.parse_import_path()?;
        self.match_token(SemiColon)?;

        Ok(import)
    }

    fn parse_import_path(&mut self) -> Result<L1Import> {
        let fragment = self.match_iden()?;

        match self.peek()?.kind {
            SemiColon => return Ok(L1Import {}),
            Dot => {
                self.match_token(SemiColon)?;
                todo!()
            }
            _ => Err(ParserError::InvalidImport),
        }
    }

    /// Parses the "def" keyword
    /// It can parse functions, structs and variables.
    pub fn parse_def(&mut self) -> Result<L1Statement> {
        self.match_keyword(Keyword::Def)?;

        let tok = self.peek()?;
        let mut generics = Vec::new();
        match tok.kind {
            BraceOpen => {
                self.match_token(BraceOpen)?;
                let generic = self.parse_generic()?;
                generics.push(generic);

                while self.peek()?.kind == Comma {
                    self.match_token(Comma)?;
                    let generic = self.parse_generic()?;
                    generics.push(generic);
                }

                self.match_token(BraceClose)?;
            }
            Identifier(_) => {}
            // TODO: Provide generics hint
            _ => {
                return Err(ParserError::InvalidDef {
                    msg: "Are you trying to define generics? The syntax is: def(T: MyInterface)",
                    pos: tok.pos,
                });
            }
        }

        let name = self.match_iden()?.to_owned();

        let tok = self.peek()?;
        match tok.kind {
            BraceOpen => {
                self.match_token(BraceOpen)?;
                let args = self.parse_args()?;
                self.match_token(BraceClose)?;

                // TODO: parse the return type
                let mut ret = L1Type::Void;
                match self.peek()?.kind {
                    CurlyBracesOpen => {}
                    _ => {
                        ret = self.parse_type()?;
                    }
                }

                let block = self.parse_block()?;

                let func = L1Fn {
                    name,
                    generics,
                    body: block,
                    args,
                    ret,
                };

                Ok(L1Statement::FnDeclr(func))
            }
            Colon => {
                self.match_token(Colon)?;

                let ty = self.parse_type()?;

                let tok = self.peek()?;
                match tok.kind {
                    Equals => {
                        self.match_token(Equals)?;

                        let expr = self.parse_expr()?;
                        self.match_token(SemiColon)?;

                        return Ok(L1Statement::Declaration {
                            var: L1Variable { name, ty },
                            value: Some(expr),
                        });
                    }
                    SemiColon => {
                        self.match_token(SemiColon)?;

                        return Ok(L1Statement::Declaration {
                            var: L1Variable { name, ty },
                            value: None,
                        });
                    }
                    _ => {
                        return Err(ParserError::InvalidDef {
                            msg: "Invalid variable declaration",
                            pos: tok.pos,
                        });
                    }
                }
            }
            Equals => {
                self.match_token(Equals)?;

                let expr = self.parse_expr()?;
                self.match_token(SemiColon)?;

                return Ok(L1Statement::Declaration {
                    var: L1Variable {
                        name,
                        ty: L1Type::Unknown,
                    },
                    value: Some(expr),
                });
            }
            SemiColon => {
                self.match_token(SemiColon)?;

                return Ok(L1Statement::Declaration {
                    var: L1Variable {
                        name,
                        ty: L1Type::Unknown,
                    },
                    value: None,
                });
            }
            _ => {
                return Err(ParserError::InvalidDef {
                    msg: "Expected '(' found something else",
                    pos: tok.pos,
                });
            }
        }
    }

    fn parse_generic(&mut self) -> Result<L1Generic> {
        let iden = self.match_iden()?;

        let interfaces = match self.peek()?.kind {
            Colon => {
                self.match_token(Colon)?;

                let mut interfaces = Vec::new();
                while self.peek()?.kind != BraceClose || self.peek()?.kind != Comma {
                    let interface = self.match_iden()?;
                    interfaces.push(L1Type::Backpatch(interface));
                    match self.peek()?.kind {
                        Plus => {
                            self.match_token(Plus)?;
                        }
                        _ => {
                            break;
                        }
                    };
                }
                interfaces
            }
            _ => Vec::new(),
        };

        let generic = L1Generic {
            name: iden,
            interfaces,
        };

        Ok(generic)
    }

    fn parse_type(&mut self) -> Result<L1Type> {
        let ty = match self.peek()?.kind {
            Identifier(_) => {
                let iden = self.match_iden()?;
                L1Type::from(iden.as_str())
            }
            SquareBracesOpen => {
                self.match_token(SquareBracesOpen)?;
                let ty = self.parse_type()?;
                self.match_token(SquareBracesClose)?;
                L1Type::Arr(Box::new(ty))
            }
            Star => {
                self.match_token(Star)?;
                L1Type::Ptr(Box::new(self.parse_type()?))
            }
            _ => {
                return Err(ParserError::InvalidType);
            }
        };

        Ok(L1Type::from(ty))
    }

    fn parse_args(&mut self) -> Result<Vec<L1Arg>> {
        match self.peek()?.kind {
            BraceClose => Ok(Vec::new()),
            _ => {
                let mut args = Vec::new();
                let arg = self.parse_arg()?;
                args.push(arg);
                while self.peek()?.kind == Comma {
                    self.match_token(Comma)?;
                    let arg = self.parse_arg()?;
                    args.push(arg);
                }
                Ok(args)
            }
        }
    }

    fn parse_arg(&mut self) -> Result<L1Arg> {
        let mut ty = self.parse_type()?;

        match self.peek()?.kind {
            Variadic => {
                self.match_token(Variadic)?;
                ty = L1Type::Variadic(Box::new(ty));
            }
            _ => {}
        }

        let name = self.match_iden()?;
        let mut default = None;

        match self.peek()?.kind {
            Equals => {
                self.match_token(Equals)?;
                default = Some(self.parse_value()?);
            }
            _ => {}
        }

        let arg = L1Arg { name, ty, default };

        Ok(arg)
    }

    // TODO: Do the Int and Float Parsing correctlt
    fn parse_value(&mut self) -> Result<L1Value> {
        let tok = self.next().ok_or(ParserError::EOF)?;
        let val = match tok.kind {
            Keyword(Keyword::Bool(b)) => L1Value::Bool(b),
            Literal(Value::Int) => {
                let num = tok
                    .raw
                    .parse::<u64>()
                    .map_err(|_| ParserError::InvalidLiteral)?;
                L1Value::U64(num)
            }
            Literal(Value::Str(ref s)) => L1Value::Str(s.clone()),
            _ => {
                // TODO: negative values, f64, and constants
                unimplemented!()
            }
        };

        Ok(val)
    }

    fn parse_block(&mut self) -> Result<L1Block> {
        self.match_token(CurlyBracesOpen)?;

        let mut statements = vec![];

        while self.peek()?.kind != CurlyBracesClose {
            statements.push(self.parse_statement()?);
        }

        self.match_token(CurlyBracesClose)?;

        Ok(L1Block { statements })
    }

    fn parse_statement(&mut self) -> Result<L1Statement> {
        let token = self.peek()?;

        let statement = match &token.kind {
            CurlyBracesOpen => L1Statement::Block(self.parse_block()?),
            Keyword(key) => match key {
                Keyword::Def => {
                    let statement = self.parse_def()?;

                    statement
                }
                _ => return Err(ParserError::InvalidStatement),
            },
            BraceOpen => L1Statement::Exr(self.parse_expr()?),
            _ => {
                return Err(ParserError::InvalidStatement);
            }
        };

        Ok(statement)
    }

    fn parse_expr(&mut self) -> Result<L1Expression> {
        let tok = self.peek()?;

        let expr = match &tok.kind {
            BraceOpen => {
                self.match_token(BraceOpen)?;
                let expr = self.parse_expr()?;
                self.match_token(BraceClose)?;
                expr
            }
            Keyword(Keyword::Bool(b)) => {
                let expr = L1Expression {
                    ty: L1Type::Bool,
                    expr: L1ExpressionInner::Bool(*b),
                };
                self.next().unwrap();
                expr
            }
            Literal(Value::Int) => {
                // TODO: Handel negative numbers here
                let clean_str = tok.raw.replace('_', "");

                let num = clean_str
                    .parse::<u64>()
                    .map_err(|_| ParserError::IntParseError)?;

                self.next().unwrap();

                L1Expression {
                    ty: L1Type::Unknown,
                    expr: L1ExpressionInner::Int(num),
                }
            }
            Literal(Value::Float) => {
                // TODO: Handel negative numbers here
                let clean_str = tok.raw.replace('_', "");

                let num = clean_str
                    .parse::<f64>()
                    .map_err(|_| ParserError::IntParseError)?;

                self.next().unwrap();

                L1Expression {
                    ty: L1Type::Unknown,
                    expr: L1ExpressionInner::Float(num),
                }
            }
            Literal(Value::Str(s)) => {
                let expr = L1Expression {
                    ty: L1Type::Str,
                    expr: L1ExpressionInner::Str(s.clone()),
                };

                self.next().unwrap();

                expr
            }
            _ => {
                todo!();
            }
        };

        Ok(expr)
    }
}
