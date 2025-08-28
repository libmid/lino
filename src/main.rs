use clap::{Parser, Subcommand};
use std::process::{Command, exit};
use std::{fs::create_dir, path::PathBuf};

use compiler::{CompilerOptions, Target};

mod error;

#[derive(Parser)]
#[command(version)]
struct LatticeCli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a program to QBE IR
    #[clap(alias = "c")]
    Compile {
        file: PathBuf,
        #[arg(short, long)]
        output_file: Option<PathBuf>,
        /// Print the generated ir to stdout
        #[arg(short, long)]
        ir: bool,
    },
    /// Build executible
    #[clap(alias = "b")]
    Build {
        file: PathBuf,
        #[arg(short, long)]
        output_file: Option<PathBuf>,
        /// Print the generated ir to stdout
        #[arg(short, long)]
        ir: bool,
    },
    /// Build and run the executible
    #[clap(alias = "r")]
    Run {
        file: PathBuf,
        /// Print the generated ir to stdout
        #[arg(short, long)]
        ir: bool,
    },
}

fn main() {
    let cli = LatticeCli::parse();

    match cli.command {
        Commands::Compile {
            file,
            output_file,
            ir,
        } => {
            let mut options = CompilerOptions::default();
            if ir {
                options.print_output();
            }
            if let Some(output_file) = output_file {
                options.output_file(output_file);
            }

            compiler::Compiler::new(file, Target::Qbe, options).compile();
        }
        Commands::Build {
            file,
            output_file,
            ir,
        } => {
            let output = build(file, output_file, ir);
            println!("Saved at {output:?}");
        }
        Commands::Run { file, ir } => {
            let output = build(file, None, ir);
            if let Some(output) = output {
                let status = Command::new(output).status().unwrap();
                exit(status.code().unwrap_or(0));
            }
        }
    }
}

fn build(file: PathBuf, output_file: Option<PathBuf>, ir: bool) -> Option<PathBuf> {
    let _ = create_dir("build/");
    let ir_file = "build/out.ssa";
    let asm_file = "build/out.s";
    let output_file = output_file.unwrap_or("build/a.out".into());

    let mut options = CompilerOptions::default();
    if ir {
        options.print_output();
    }
    options.output_file(ir_file);

    compiler::Compiler::new(file, Target::Qbe, options).compile();

    let mut status;
    status = Command::new("qbe")
        .arg(ir_file)
        .arg("-o")
        .arg(asm_file)
        .status()
        .unwrap();
    if status.success() {
        status = Command::new("gcc")
            .arg(asm_file)
            .arg("-lc")
            .arg("-ggdb")
            .arg("-o")
            .arg(&output_file)
            .status()
            .unwrap();
    }

    if status.success() {
        Some(output_file)
    } else {
        None
    }
}
