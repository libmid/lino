use TokenKind::*;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Identifier(String),
    Keyword(Keyword),
    Literal(Value),

    /// "("
    BraceOpen,
    /// ")"
    BraceClose,
    /// "{"
    CurlyBracesOpen,
    /// "}"
    CurlyBracesClose,
    /// ":"
    Colon,
    /// "::"
    ColonColon,
    /// "*"
    Star,
    /// "["
    SquareBracesOpen,
    /// "]"
    SquareBracesClose,
    /// ","
    Comma,
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "/"
    Slash,
    /// "%"
    Percent,
    /// "."
    Dot,
    /// ".."
    Range,
    /// "..="
    RangeEq,
    /// "..."
    Variadic,
    /// "="
    Equals,
    /// "=="
    DoubleEq,
    /// ";"
    SemiColon,
    /// "!"
    Not,
    /// "!="
    NotEq,

    /// "&"
    And,
    /// "&&"
    AndAnd,
    /// "|"
    Or,
    /// "||"
    OrOr,
    /// "^"
    Xor,
    /// "<<"
    Lsh,
    /// ">>"
    Rsh,
    Gt,
    Gte,
    Lt,
    Lte,

    /// "#"
    Comment(String),
    /// This only exists for error reporting, in practice the parser doesn't encounter this
    Unknown,
    /// Similarly parser doesn't see this too
    Whitespace,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int,
    Float,
    Str(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Def,
    /// true and false
    Bool(bool),
    Import,
    While,
    If,
    Else,
    Return,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub len: usize,
    pub raw: &'a str,
    pub pos: usize,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: Vec<char>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    pub fn tokenize(mut self) -> Vec<Token<'a>> {
        let mut prev_pos = 0;
        let mut tokens = vec![];
        while let Some(token) = self.consume_token() {
            tokens.push(Token {
                kind: token,
                len: self.pos - prev_pos,
                raw: &self.input[prev_pos..self.pos],
                pos: prev_pos,
            });
            prev_pos = self.pos;
        }

        tokens
    }

    fn consume_token(&mut self) -> Option<TokenKind> {
        let first_char = self.chars.get(self.pos);
        let second_char = self.chars.get(self.pos + 1);
        let third_char = self.chars.get(self.pos + 2);

        match first_char {
            Some(c) => match c {
                ch if is_whitespace(*ch) => {
                    self.pos += 1;
                    Some(Whitespace)
                }
                '"' => Some(self.consume_string()),
                '{' => {
                    self.pos += 1;
                    Some(CurlyBracesOpen)
                }
                '}' => {
                    self.pos += 1;
                    Some(CurlyBracesClose)
                }
                '(' => {
                    self.pos += 1;
                    Some(BraceOpen)
                }
                ')' => {
                    self.pos += 1;
                    Some(BraceClose)
                }
                ':' => match second_char {
                    Some(':') => {
                        self.pos += 2;
                        Some(ColonColon)
                    }
                    _ => {
                        self.pos += 1;
                        Some(Colon)
                    }
                },
                '*' => {
                    self.pos += 1;
                    Some(Star)
                }
                '[' => {
                    self.pos += 1;
                    Some(SquareBracesOpen)
                }
                ']' => {
                    self.pos += 1;
                    Some(SquareBracesClose)
                }
                ',' => {
                    self.pos += 1;
                    Some(Comma)
                }
                '+' => {
                    self.pos += 1;
                    Some(Plus)
                }
                '-' => {
                    self.pos += 1;
                    Some(Minus)
                }
                '=' => match second_char {
                    Some('=') => {
                        self.pos += 2;
                        Some(DoubleEq)
                    }
                    _ => {
                        self.pos += 1;
                        Some(Equals)
                    }
                },
                '!' => match second_char {
                    Some('=') => {
                        self.pos += 2;
                        Some(NotEq)
                    }
                    _ => {
                        self.pos += 1;
                        Some(Not)
                    }
                },
                ';' => {
                    self.pos += 1;
                    Some(SemiColon)
                }
                '%' => {
                    self.pos += 1;
                    Some(Percent)
                }
                '/' => {
                    self.pos += 1;
                    Some(Slash)
                }
                '&' => match second_char {
                    Some('&') => {
                        self.pos += 2;
                        Some(AndAnd)
                    }
                    _ => {
                        self.pos += 1;
                        Some(And)
                    }
                },
                '|' => match second_char {
                    Some('|') => {
                        self.pos += 2;
                        Some(OrOr)
                    }
                    _ => {
                        self.pos += 1;
                        Some(Or)
                    }
                },
                '^' => {
                    self.pos += 1;
                    Some(Xor)
                }
                '#' => {
                    let comment = self.consume_comment();
                    Some(comment)
                }

                '<' => match second_char {
                    Some('=') => {
                        self.pos += 2;
                        Some(Lte)
                    }
                    Some('<') => {
                        self.pos += 2;
                        Some(Lsh)
                    }
                    _ => {
                        self.pos += 1;
                        Some(Lt)
                    }
                },

                '>' => match second_char {
                    Some('=') => {
                        self.pos += 2;
                        Some(Gte)
                    }
                    Some('>') => {
                        self.pos += 2;
                        Some(Rsh)
                    }
                    _ => {
                        self.pos += 1;
                        Some(Gt)
                    }
                },

                '.' => match second_char {
                    Some('.') => match third_char {
                        Some('.') => {
                            self.pos += 3;
                            Some(Variadic)
                        }
                        Some('=') => {
                            self.pos += 3;
                            Some(RangeEq)
                        }
                        _ => {
                            self.pos += 1;
                            Some(Range)
                        }
                    },
                    _ => {
                        self.pos += 1;
                        Some(Dot)
                    }
                },
                '0'..='9' => {
                    let number = self.consume_number();
                    Some(number)
                }
                ch if is_id_start(*ch) => {
                    let token_kind = self.consume_iden_or_keyword();
                    Some(token_kind)
                }
                _ => {
                    self.pos += 1;
                    Some(Unknown)
                }
            },
            None => None,
        }
    }

    fn consume_iden_or_keyword(&mut self) -> TokenKind {
        let thing = consume_iden(&self.input[self.pos..]);

        self.pos += thing.len();
        match thing {
            "def" => Keyword(Keyword::Def),
            "true" => Keyword(Keyword::Bool(true)),
            "false" => Keyword(Keyword::Bool(false)),
            "import" => Keyword(Keyword::Import),
            "while" => Keyword(Keyword::While),
            "if" => Keyword(Keyword::If),
            "else" => Keyword(Keyword::Else),
            "return" => Keyword(Keyword::Return),
            thing => Identifier(thing.to_string()),
        }
    }

    fn consume_comment(&mut self) -> TokenKind {
        let mut buf = String::new();
        // Skip the #
        self.pos += 1;

        loop {
            match self.chars.get(self.pos) {
                None => break,
                Some('\n') => {
                    self.pos += 1;
                    break;
                }
                Some(ch) => {
                    buf.push(*ch);
                    self.pos += 1;
                }
            };
        }

        Comment(buf)
    }

    fn consume_number(&mut self) -> TokenKind {
        // TODO: Support binary, octal and hex
        let mut is_float = false;
        loop {
            match *self.chars.get(self.pos).unwrap() {
                '_' => {
                    self.pos += 1;
                }
                '0'..='9' => {
                    self.pos += 1;
                }
                '.' if !is_float => match *self.chars.get(self.pos + 1).unwrap() {
                    '0'..'9' => {
                        is_float = true;
                        self.pos += 2;
                    }
                    _ => break,
                },
                _ => {
                    break;
                }
            }
        }
        if is_float {
            Literal(Value::Float)
        } else {
            Literal(Value::Int)
        }
    }

    fn consume_string(&mut self) -> TokenKind {
        let mut buf = String::new();
        self.pos += 1;
        loop {
            match self.chars.get(self.pos).unwrap() {
                // TODO: Analyse this
                // '\\' => {
                //     self.pos += 1;
                //     buf.push(self.consume_escape())
                // }
                ch if *ch == '"' => break,
                ch => {
                    buf.push(*ch);
                    self.pos += 1;
                }
            };
        }

        // Eat last quote
        self.pos += 1;

        Literal(Value::Str(buf))
    }

    fn consume_escape(&mut self) -> char {
        let ch = self.chars.get(self.pos).unwrap();
        let ch = match ch {
            'n' => '\n',       // Newline
            'r' => '\r',       // Carriage Return
            'b' => '\u{0008}', // Backspace
            'f' => '\u{000C}', // Form feed
            't' => '\t',       // Horizontal tab
            '"' | '\\' => *ch,
            ch => *ch,
        };
        self.pos += 1;

        ch
    }
}

fn is_whitespace(c: char) -> bool {
    // https://doc.rust-lang.org/reference/whitespace.html
    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

fn consume_iden(input: &str) -> &str {
    return consume_while(input, is_id_continue);
}

fn consume_while<F>(input: &str, predicate: F) -> &str
where
    F: Fn(char) -> bool,
{
    let mut counter = 0;
    for ch in input.chars() {
        if !predicate(ch) {
            return &input[..counter];
        }
        counter += 1;
    }
    input
}

pub fn is_id_start(c: char) -> bool {
    // Valid identifier start is either an underscore or any Unicode letter
    c == '_' || c.is_alphabetic()
}

pub fn is_id_continue(c: char) -> bool {
    // Valid identifier continuation is underscore, letter, or number
    c == '_' || c.is_alphabetic() || c.is_numeric()
}
