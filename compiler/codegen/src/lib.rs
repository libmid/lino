#![feature(box_patterns)]

pub mod c;
pub mod qbe;

pub trait Backend {
    fn generate(&mut self, ast: &ast::L1Ast) -> String;
}

pub struct Codegen<T: Backend> {
    backend: T,
}

impl<T: Backend> Codegen<T> {
    pub fn new(backend: T) -> Self {
        Self { backend }
    }

    pub fn generate(&mut self, ast: &ast::L1Ast) -> String {
        self.backend.generate(ast)
    }
}

impl Backend for Box<dyn Backend> {
    fn generate(&mut self, ast: &ast::L1Ast) -> String {
        (**self).generate(ast)
    }
}
