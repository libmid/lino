#![feature(box_patterns)]

use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use codegen::c::CBackend;
use codegen::qbe::QbeBackend;
use codegen::{Backend, Codegen};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::analysis::backpatch::backpatch;
use crate::analysis::inference::Inference;
pub use crate::lexer::{Token, TokenKind};

mod analysis;
mod lexer;
mod parser;

#[derive(Debug)]
pub enum Target {
    Qbe,
    C,
}

#[derive(Debug)]
pub struct CompilerOptions {
    print_output_flag: bool,
    stdlib: Option<PathBuf>,
    output_file: PathBuf,
    target: Target,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            print_output_flag: Default::default(),
            stdlib: Default::default(),
            output_file: "build/out.ssa".into(),
            target: Target::C,
        }
    }
}

impl CompilerOptions {
    pub fn print_output(&mut self) -> &mut Self {
        self.print_output_flag = true;
        self
    }

    pub fn stdlib(&mut self, stdlib: PathBuf) -> &mut Self {
        self.stdlib = Some(stdlib);
        self
    }

    pub fn output_file(&mut self, output_file: impl Into<PathBuf>) -> &mut Self {
        self.output_file = output_file.into();
        self
    }

    pub fn backend(&mut self, target: Target) -> &mut Self {
        self.target = target;
        self
    }
}

pub struct Compiler {
    input_file: PathBuf,
    input: String,
    options: CompilerOptions,
}

impl Compiler {
    pub fn new(input_file: PathBuf, target: Target, options: CompilerOptions) -> Self {
        let input = std::fs::read_to_string(&input_file).unwrap_or_else(|err| {
            eprintln!("ERROR: {}", err);
            std::process::exit(1);
        });

        Self {
            input_file,
            input,
            options,
        }
    }

    pub fn compile(&mut self) {
        // Step 3: Parse imports
        // Step 3.5: Verify the order of default args
        // Step 5: Reorder bin ops
        // Step 8: Type Checking
        // Step ??: Dead code elimination

        // Step 1: Tokenize
        let lxr = lexer::Lexer::new(&self.input);
        let tokens = lxr.tokenize();

        // There might be unknown tokens, handel them gracefully
        self.error_tokens(&tokens);

        let tokens_without_whitespace: Vec<Token> = tokens
            .into_iter()
            .filter(|token| match token.kind {
                TokenKind::Comment(_) | TokenKind::Whitespace => false,
                _ => true,
            })
            .collect();

        // dbg!(&tokens_without_whitespace);

        // Step 2: Parse tokens into initial parsing tree
        let mut l1p = parser::L1Parser::new(&tokens_without_whitespace);
        // TODO: Better error reporting
        l1p.parse().unwrap();

        // Step 4: Backpatch types
        backpatch(l1p.get_ast()).unwrap();

        // Step 7: Infer expression type
        let inference = Inference::new(l1p.get_ast());
        inference.infer_types(l1p.get_ast()).unwrap();

        // dbg!(l1p.get_ast());

        // Step 9: Codegen
        let mut cg: Codegen<Box<dyn Backend>> = match self.options.target {
            Target::Qbe => Codegen::new(Box::new(QbeBackend::new())),
            Target::C => Codegen::new(Box::new(CBackend::new())),
        };

        let output = cg.generate(&l1p.get_ast());

        if self.options.print_output_flag {
            println!("{}", &output);
        }

        let mut out = File::create(&self.options.output_file).unwrap();
        out.write_all(output.as_bytes()).unwrap();
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
