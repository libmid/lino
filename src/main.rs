use clap::{Parser, Subcommand};
use std::path::PathBuf;

use compiler::Target;

mod error;

#[derive(Parser)]
#[command(version)]
struct LatticeCli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a program to .lb file
    Compile {
        file: PathBuf,
        #[arg(short, long)]
        stdlib: Option<PathBuf>,
    },
}

fn main() {
    let cli = LatticeCli::parse();

    match cli.command {
        Commands::Compile { file, stdlib } => {
            compiler::Compiler::new(file, stdlib, Target::LtIR).compile()
        }
    }
}
