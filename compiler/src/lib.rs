use std::path::PathBuf;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

pub use crate::lexer::{Token, TokenKind};

mod lexer;
mod parser;

pub enum Target {
    // Lattice Text IR
    LtIR,
}

pub struct Compiler {
    target: Target,
    input_file: PathBuf,
    output_file: PathBuf,
    input: String,
}

impl Compiler {
    pub fn new(input_file: PathBuf, target: Target) -> Self {
        let input = std::fs::read_to_string(&input_file).unwrap_or_else(|err| {
            eprintln!("ERROR: {}", err);
            std::process::exit(1);
        });

        Self {
            target: target,
            input_file: input_file,
            input,
            output_file: "a.lb".into(),
        }
    }

    pub fn output_file(&mut self, path: PathBuf) -> &mut Self {
        self.output_file = path;
        self
    }

    pub fn compile(&mut self) {
        // Step 1: Tokenize
        // Step 2: Parse tokens into initial parsing tree
        // Step 3: Parse imports
        // Step 3.5: Verify the order of default args
        // Step 4: Backpatch types
        // Step 5: Infer expression type
        // Step 6: Type Checking

        let lxr = lexer::Lexer::new(&self.input);
        let tokens = lxr.tokenize();

        // There might be unknown tokens, handel them gracefully
        self.error_tokens(&tokens);

        let tokens_without_whitespace: Vec<Token> = tokens
            .into_iter()
            .filter(|token| token.kind != TokenKind::Whitespace)
            .collect();

        dbg!(&tokens_without_whitespace);

        let mut l1p = parser::L1Parser::new(&tokens_without_whitespace);
        l1p.parse().unwrap();

        dbg!(l1p.get_ast());
    }

    fn error_tokens(&self, tokens: &Vec<Token>) {
        let mut had_unknown = false;
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        let mut files = SimpleFiles::new();
        let file_id = files.add(self.input_file.as_os_str().to_str().unwrap(), &self.input);
        let mut errors = vec![];
        for token in tokens {
            match token.kind {
                TokenKind::Unknown => {
                    errors.push(
                        Label::primary(file_id, token.pos..token.pos + token.len)
                            .with_message("Unknown Token"),
                    );

                    had_unknown = true;
                }
                _ => {}
            }
        }
        if had_unknown {
            let diagnostic = Diagnostic::error().with_labels(errors);

            term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();

            std::process::exit(1);
        }
    }
}
