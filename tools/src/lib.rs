use proconio::{input, marker::Chars};
use std::process::Stdio;

#[derive(Clone, Debug)]
pub struct Input {
    pub n: usize,
    pub k: usize,
    pub h: usize,
    pub w: usize,
    pub t: usize,
    pub maps: Vec<Vec<Vec<char>>>,
}

impl std::fmt::Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {} {} {} {}", self.n, self.k, self.h, self.w, self.t)?;
        for map in self.maps.iter() {
            for row in map.iter() {
                for (w, ch) in row.iter().enumerate() {
                    if w != self.w - 1 {
                        write!(f, "{ch}")?;
                    } else {
                        writeln!(f, "{ch}")?;
                    }
                }
            }
        }
        Ok(())
    }
}

pub fn parse_input(f: &str) -> Input {
    let f = proconio::source::once::OnceSource::from(f);
    input! {
        from f,
        n: usize,
        k: usize,
        h: usize,
        w: usize,
        t: usize,
        maps: [[Chars; h];n],
    }
    Input {
        n,
        k,
        h,
        w,
        t,
        maps,
    }
}

pub fn gen(seed: u64) -> Input {
    const TESTER_DIR_PATH: &str = "./qual_A/tester";
    let p = std::process::Command::new("java")
        .current_dir(TESTER_DIR_PATH)
        .args(["Generator", "-seed", &seed.to_string()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| {
            eprintln!("failed to execute the command");
            eprintln!("{e}");
            std::process::exit(1)
        });
    let output = p.wait_with_output().unwrap();
    let f = output.stdout.iter().map(|&b| b as char).collect::<String>();
    parse_input(&f)
}
