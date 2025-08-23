use crate::{
    TokenKind,
    lexer::{Keyword, TokenKind::*, Value},
    parser::{
        L1Parser,
        error::{ParserError, Result},
    },
};

use ast::{
    BinOp, L1Arg, L1Block, L1Enum, L1EnumVariant, L1Expression, L1ExpressionInner, L1Fn, L1FnDeclr,
    L1Generic, L1If, L1Import, L1ImportFragment, L1NamedExpr, L1Statement, L1Struct, L1Type,
    L1Value, L1Variable, L1While,
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
            SemiColon | CurlyBracesClose => {
                // Do not consume the semicolon, its done by caller
                return Ok(L1Import {
                    fragment: L1ImportFragment::Path(fragment),
                    nexts: None,
                });
            }
            Comma => {
                self.match_token(Comma)?;

                Ok(L1Import {
                    fragment: L1ImportFragment::Path(fragment),
                    nexts: None,
                })
            }
            Dot => {
                self.match_token(Dot)?;

                match self.peek()?.kind {
                    Identifier(_) => {
                        let imp = self.parse_import_path()?;

                        Ok(L1Import {
                            fragment: L1ImportFragment::Path(fragment),
                            nexts: Some(vec![imp]),
                        })
                    }
                    Star => {
                        self.match_token(Star)?;

                        Ok(L1Import {
                            fragment: L1ImportFragment::Path(fragment),
                            nexts: Some(vec![L1Import {
                                fragment: L1ImportFragment::All,
                                nexts: None,
                            }]),
                        })
                    }
                    CurlyBracesOpen => {
                        self.match_token(CurlyBracesOpen)?;

                        let mut imports = vec![];
                        while self.peek()?.kind != CurlyBracesClose {
                            let imp = self.parse_import_path()?;
                            imports.push(imp);

                            if self.peek()?.kind == Comma {
                                self.match_token(Comma)?;
                            }
                        }

                        self.match_token(CurlyBracesClose)?;

                        Ok(L1Import {
                            fragment: L1ImportFragment::Path(fragment),
                            nexts: Some(imports),
                        })
                    }
                    _ => Err(ParserError::InvalidImport),
                }
            }
            ref k => {
                dbg!(k);
                Err(ParserError::InvalidImport)
            }
        }
    }

    /// Parses the "def" keyword
    /// It can parse (extern) functions, structs and (static) variables.
    pub fn parse_def(&mut self) -> Result<L1Statement> {
        self.match_keyword(Keyword::Def)?;

        let tok = self.peek()?;
        let mut generics = Vec::new();
        match tok.kind {
            // def(T) is the syntax for generics
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
            // No generics, type name
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
            // Is a function
            BraceOpen => {
                self.match_token(BraceOpen)?;
                let args = self.parse_args(BraceClose)?;
                self.match_token(BraceClose)?;

                // TODO: parse the return type
                let mut ret = L1Type::Void;
                match self.peek()?.kind {
                    CurlyBracesOpen | SemiColon => {}
                    _ => {
                        ret = self.parse_type()?;
                    }
                }

                match self.peek()?.kind {
                    // Function doesn't have a body (extern declaration)
                    SemiColon => {
                        self.match_token(SemiColon)?;

                        return Ok(L1Statement::ExternFnDeclr(L1FnDeclr {
                            name,
                            generics,
                            args,
                            ret,
                        }));
                    }
                    _ => {}
                }

                let block = self.parse_block()?;

                let func = L1Fn {
                    name,
                    generics,
                    body: block,
                    args,
                    ret,
                };

                Ok(L1Statement::FnDef(func))
            }
            // Is a struct or enum
            CurlyBracesOpen => {
                self.match_token(CurlyBracesOpen)?;
                // Try to parse it as a struct
                self.snapshot();
                if let Ok(fields) = self.parse_args(CurlyBracesClose) {
                    self.match_token(CurlyBracesClose)?;
                    Ok(L1Statement::StructDef(L1Struct {
                        name,
                        generics,
                        fields,
                    }))
                } else {
                    self.rollback();
                    // Try to parse it as an enum
                    let variants = self.parse_enum_variants()?;
                    self.match_token(CurlyBracesClose)?;
                    Ok(L1Statement::EnumDef(L1Enum {
                        name,
                        generics,
                        variants,
                    }))
                }
            }
            // Variable with type
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
            // Variable without type
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
            // Variable without type or default value
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
                match self.peek()?.kind {
                    ColonColon => {
                        self.match_token(ColonColon)?;
                        self.match_token(BraceOpen)?;
                        // TODO: Use the generics list
                        let generics_list = self.parse_type_list(BraceClose)?;
                        self.match_token(BraceClose)?;
                    }
                    _ => {}
                }
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

        Ok(ty)
    }

    fn parse_type_list(&mut self, close_token: TokenKind) -> Result<Vec<L1Type>> {
        match &self.peek()?.kind {
            token if token == &close_token => Ok(Vec::new()),
            _ => {
                let mut types = Vec::new();
                let ty = self.parse_type()?;
                types.push(ty);
                loop {
                    match &self.peek()?.kind {
                        token if token == &close_token => {
                            break;
                        }
                        Comma => {
                            self.match_token(Comma)?;
                        }
                        _ => {
                            let ty = self.parse_type()?;
                            types.push(ty);
                        }
                    };
                }
                Ok(types)
            }
        }
    }

    fn parse_enum_variants(&mut self) -> Result<Vec<L1EnumVariant>> {
        match &self.peek()?.kind {
            CurlyBracesClose => Ok(Vec::new()),
            _ => {
                let mut variants = Vec::new();
                let variant = self.parse_enum_variant()?;
                variants.push(variant);
                loop {
                    match &self.peek()?.kind {
                        CurlyBracesClose => {
                            break;
                        }
                        Comma => {
                            self.match_token(Comma)?;
                        }
                        _ => {
                            let variant = self.parse_enum_variant()?;
                            variants.push(variant);
                        }
                    };
                }
                Ok(variants)
            }
        }
    }

    fn parse_enum_variant(&mut self) -> Result<L1EnumVariant> {
        let name = self.match_iden()?;
        let mut ty = None;
        match self.peek()?.kind {
            BraceOpen => {
                self.match_token(BraceOpen)?;
                ty = Some(self.parse_type()?);
                self.match_token(BraceClose)?;
            }
            _ => {}
        }
        Ok(L1EnumVariant { name, ty })
    }

    fn parse_args(&mut self, close_token: TokenKind) -> Result<Vec<L1Arg>> {
        match &self.peek()?.kind {
            token if token == &close_token => Ok(Vec::new()),
            _ => {
                let mut args = Vec::new();
                let arg = self.parse_arg()?;
                args.push(arg);
                loop {
                    match &self.peek()?.kind {
                        token if token == &close_token => {
                            break;
                        }
                        Comma => {
                            self.match_token(Comma)?;
                        }
                        _ => {
                            let arg = self.parse_arg()?;
                            args.push(arg);
                        }
                    };
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

    // TODO: Do the Int and Float Parsing correctly
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
        let kind = &self.peek()?.kind.clone();

        let statement = match kind {
            CurlyBracesOpen => L1Statement::Block(self.parse_block()?),
            Keyword(key) => match key {
                Keyword::Def => {
                    let statement = self.parse_def()?;

                    statement
                }
                Keyword::Return => {
                    self.match_keyword(Keyword::Return)?;

                    if self.peek()?.kind == SemiColon {
                        self.match_token(SemiColon)?;

                        L1Statement::Return(None)
                    } else {
                        let expr = self.parse_expr()?;

                        self.match_token(SemiColon)?;

                        L1Statement::Return(Some(expr))
                    }
                }
                Keyword::While => {
                    let statement = self.parse_while()?;

                    L1Statement::While(statement)
                }
                Keyword::If => {
                    let statement = self.parse_if()?;

                    L1Statement::If(statement)
                }
                _ => return Err(ParserError::InvalidStatement),
            },
            _ => {
                let exp1 = self.parse_expr()?;

                let stat = if self.peek()?.kind == Equals {
                    self.match_token(Equals)?;

                    let exp2 = self.parse_expr()?;

                    L1Statement::Assign {
                        lhs: exp1,
                        rhs: exp2,
                    }
                } else {
                    L1Statement::Expr(exp1)
                };

                self.match_token(SemiColon)?;

                stat
            }
        };

        Ok(statement)
    }

    fn parse_while(&mut self) -> Result<L1While> {
        self.match_keyword(Keyword::While)?;

        let expr = self.parse_expr()?;

        let block = self.parse_block()?;

        Ok(L1While {
            condition: expr,
            body: block,
        })
    }

    fn parse_if(&mut self) -> Result<L1If> {
        self.match_keyword(Keyword::If)?;

        let expr = self.parse_expr()?;

        let if_block = self.parse_block()?;

        let mut else_block = None;

        match self.peek()?.kind {
            Keyword(Keyword::Else) => match self.peek_2()?.kind {
                Keyword(Keyword::If) => {
                    // TODO: Parse "else if"
                    todo!("Parse else if blocks")
                }
                _ => {
                    self.match_keyword(Keyword::Else)?;
                    else_block = Some(self.parse_block()?);
                }
            },
            _ => {}
        }

        // TODO: Else if and else blocks
        Ok(L1If {
            if_cond: expr,
            if_block: if_block,
            else_block,
        })
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
                    // Unknown because Int can be word, dword, hword or byte
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
                    // Unknown cause Float can be f32 or f64
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
            Identifier(_) => {
                match self.peek_2()?.kind {
                    // func()
                    BraceOpen => {
                        let call = self.parse_function_call()?;

                        call
                    }
                    CurlyBracesOpen => {
                        let s = self.parse_struct_init()?;

                        s
                    }
                    _ => {
                        let iden = self.match_iden()?;
                        L1Expression {
                            ty: L1Type::Unknown,
                            expr: L1ExpressionInner::Variable(iden),
                        }
                    }
                }
            }
            Star => {
                self.match_token(Star)?;

                L1Expression {
                    ty: L1Type::Unknown,
                    expr: L1ExpressionInner::Deref(self.parse_expr()?.into()),
                }
            }
            tok => {
                return Err(ParserError::InvalidExpr);
            }
        };

        match self.peek()?.kind {
            ref kind if BinOp::try_from(kind.clone()).is_ok() => {
                let op = BinOp::try_from(kind.clone()).unwrap();

                self.next();

                let rhs = self.parse_expr()?;

                Ok(L1Expression {
                    ty: L1Type::Unknown,
                    expr: L1ExpressionInner::BinOp {
                        lhs: Box::new(expr),
                        op,
                        rhs: Box::new(rhs),
                    },
                })
            }
            Dot => self.parse_field_access(expr),

            _ => Ok(expr),
        }
    }

    fn parse_field_access(&mut self, lhs: L1Expression) -> Result<L1Expression> {
        self.match_token(Dot)?;

        let field = match self.peek_2()?.kind {
            BraceOpen => self.parse_function_call()?,
            _ => L1Expression {
                ty: L1Type::Unknown,
                expr: L1ExpressionInner::Variable(self.match_iden()?),
            },
        };

        let expr = L1Expression {
            ty: L1Type::Unknown,
            expr: L1ExpressionInner::FieldAccess {
                expr: Box::new(lhs),
                field: Box::new(field),
            },
        };

        match self.peek()?.kind {
            Dot => self.parse_field_access(expr), // TODO: DO domething about array access
            _ => Ok(expr),
        }
    }

    fn parse_function_call(&mut self) -> Result<L1Expression> {
        let func = self.match_iden()?;
        self.match_token(BraceOpen)?;

        match self.peek()?.kind {
            BraceClose => {
                self.match_token(BraceClose)?;

                Ok(L1Expression {
                    ty: L1Type::Unknown,
                    expr: L1ExpressionInner::FnCall {
                        name: func,
                        args: vec![],
                    },
                })
            }
            _ => {
                let mut args = vec![self.parse_named_expr()?];

                loop {
                    match self.peek()?.kind {
                        Comma => {
                            self.match_token(Comma)?;
                            args.push(self.parse_named_expr()?);
                        }
                        BraceClose => {
                            self.match_token(BraceClose)?;
                            break;
                        }
                        _ => return Err(ParserError::InvalidCall),
                    }
                }

                Ok(L1Expression {
                    ty: L1Type::Unknown,
                    expr: L1ExpressionInner::FnCall { name: func, args },
                })
            }
        }
    }

    fn parse_struct_init(&mut self) -> Result<L1Expression> {
        let name = self.match_iden()?;
        self.match_token(CurlyBracesOpen)?;

        match self.peek()?.kind {
            CurlyBracesClose => {
                self.match_token(CurlyBracesClose)?;

                Ok(L1Expression {
                    ty: L1Type::Unknown,
                    expr: L1ExpressionInner::StructInit {
                        name,
                        fields: vec![],
                    },
                })
            }
            _ => {
                let mut fields = vec![self.parse_named_expr()?];

                loop {
                    match self.peek()?.kind {
                        Comma => {
                            self.match_token(Comma)?;
                            fields.push(self.parse_named_expr()?);
                        }
                        CurlyBracesClose => {
                            self.match_token(CurlyBracesClose)?;
                            break;
                        }
                        _ => return Err(ParserError::InvalidStructInit),
                    }
                }

                Ok(L1Expression {
                    ty: L1Type::Unknown,
                    expr: L1ExpressionInner::StructInit { name, fields },
                })
            }
        }
    }

    fn parse_named_expr(&mut self) -> Result<L1NamedExpr> {
        match self.peek()?.kind {
            Identifier(_) if self.peek_2()?.kind == Equals => {
                let name = self.match_iden()?;
                self.match_token(Equals)?;
                let expr = self.parse_expr()?;
                Ok(L1NamedExpr {
                    name: Some(name),
                    expr,
                })
            }
            _ => Ok(L1NamedExpr {
                name: None,
                expr: self.parse_expr()?,
            }),
        }
    }
}

impl TryFrom<TokenKind> for BinOp {
    type Error = ParserError;

    fn try_from(value: TokenKind) -> std::result::Result<Self, Self::Error> {
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
