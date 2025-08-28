use ast::*;

mod cx;

trait CxFns {
    fn dbg(&self, expr: L1Expression) {
        dbg!(expr);
    }
}

pub struct VMOptions<const N: usize> {
    stack_size: usize,
}

impl<const N: usize> Default for VMOptions<N> {
    fn default() -> Self {
        Self { stack_size: N }
    }
}

pub struct VM<const N: usize> {
    stack_size: usize,
    stack: [u8; N],
}

impl<const N: usize> VM<N> {
    pub const fn init(options: VMOptions<N>) -> Self {
        let vm = VM {
            stack_size: options.stack_size,
            stack: [0; N],
        };

        vm
    }

    pub fn patch_cx(ast: &mut L1Ast) {}
}
