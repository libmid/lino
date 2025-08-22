pub mod qbe;

pub trait Backend {
    fn generate(self, ast: &ast::L1Ast) -> String;
}

pub struct Codegen<T: Backend> {
    backend: T,
}

impl<T: Backend> Codegen<T> {
    pub fn new(backend: T) -> Self {
        Self { backend }
    }

    pub fn generate(self, ast: &ast::L1Ast) -> String {
        self.backend.generate(ast)
    }
}
