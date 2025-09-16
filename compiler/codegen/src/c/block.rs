use crate::c::CBackend;
use ast::L1Block;

impl CBackend {
    pub fn l1block_to_c(&self, block: &L1Block) -> String {
        block
            .statements
            .iter()
            .map(|statement| self.l1statement_to_c(statement))
            .map(|mut statement| {
                statement.push_str("\n");
                statement
            })
            .collect()
    }
}
