use clap::Parser;
use std::{io::Write, path::PathBuf};

#[derive(Parser, Debug)]
struct Cli {
    /// Path to seeds.txt
    #[clap(short = 'n', long = "num", default_value = "100")]
    num: String,
    /// Path to input directory
    #[clap(short = 'f', long = "file", default_value = "./tools/seeds.txt")]
    file_path: PathBuf,
}

fn main() {
    let cli = Cli::parse();
    std::fs::remove_file(&cli.file_path).unwrap();
    let mut file = std::fs::File::create(&cli.file_path).unwrap();
    let num = cli.num.parse::<usize>().unwrap();
    for i in 0..num {
        file.write_all(i.to_string().as_bytes()).unwrap();
        file.write_all('\n'.to_string().as_bytes()).unwrap();
    }
}
