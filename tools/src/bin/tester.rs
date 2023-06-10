use indicatif::{ProgressBar, ProgressStyle};
use std::{
    fs,
    io::{Read, Write},
    path::PathBuf,
    process::Stdio,
    sync::{Arc, Mutex},
};
use threadpool::ThreadPool;

fn main() {
    fs::remove_dir_all("./tools/out").unwrap();
    fs::create_dir("./tools/out").unwrap();
    // pbを作るときにファイル数が知りたくてlenにしているが、もっといい方法ある？
    let files = fs::read_dir("./tools/in/").unwrap().collect::<Vec<_>>();

    // プログレスバーの初期化
    let sty = ProgressStyle::with_template(
        "[{elapsed_precise}] {bar:40.cyan/blue} {pos:>7}/{len:7} {msg}",
    )
    .unwrap()
    .progress_chars("##-");
    let pb = ProgressBar::new(files.len() as u64);
    pb.set_style(sty);
    let pb = Arc::new(Mutex::new(pb));

    let pool = ThreadPool::new(12);
    let total_score = Arc::new(Mutex::new(0));
    let mut case_num = 0_i64;
    for file in files {
        case_num += 1;
        let file = file.unwrap();
        let file_path = file.path();
        let total_score = total_score.clone();
        let pb = pb.clone();
        pool.execute(move || {
            let score = exec(file_path);
            *total_score.lock().unwrap() += score;
            pb.lock().unwrap().inc(1);
        })
    }
    pool.join();
    const PRETESTNUM: i64 = 30;
    let total_score = *total_score.lock().unwrap();
    let local_score = total_score * PRETESTNUM / case_num;
    println!("local score:{}", add_separator(local_score.to_string()));
}

fn exec(file_path: PathBuf) -> i64 {
    let file_name = file_path.file_name().unwrap().to_string_lossy();
    let in_file = format!("./tools/in/{file_name}");
    let out_file = format!("./tools/out/{file_name}");
    {
        // 実行部分
        let p = std::process::Command::new("cargo")
            .args(["run", "--release"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap_or_else(|e| {
                eprintln!("failed to execute the command");
                eprintln!("{e}");
                std::process::exit(1)
            });
        let mut file = fs::File::open(in_file).unwrap();
        let mut buf = vec![];
        file.read_to_end(&mut buf).unwrap_or_else(|e| {
            eprintln!("failed to read {file:?}");
            eprintln!("{e}");
            std::process::exit(1)
        });
        let mut stdin = p.stdin.as_ref().unwrap();
        stdin.write_all(&buf).unwrap();
        let output = p.wait_with_output().unwrap();
        let mut file = fs::File::create(out_file).unwrap();
        file.write_all(&output.stdout).unwrap();
    };

    // ジャッジ部分
    const TESTER_DIR_PATH: &str = "./qual_A/tester";
    let in_file = format!("../../tools/in/{file_name}");
    let out_file = format!("../../tools/out/{file_name}");
    let p = std::process::Command::new("java")
        .current_dir(TESTER_DIR_PATH)
        .args(["Judge", &in_file, &out_file])
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
    let b = String::from_utf8(output.stdout).unwrap();
    let s = b.split('\n').collect::<Vec<_>>();
    let score = s[0].split(':').collect::<Vec<_>>()[1]
        .trim()
        .parse::<i64>()
        .unwrap();
    let error = output
        .stderr
        .into_iter()
        .map(|b| b as char)
        .collect::<String>();
    if error.is_empty() {
        score
    } else {
        eprintln!("failed {file_name} because of {error}");
        0
    }
}

fn add_separator(score: String) -> String {
    score
        .chars()
        .rev()
        .collect::<Vec<char>>()
        .chunks(3)
        .map(|cs| cs.iter().collect::<String>())
        .collect::<Vec<String>>()
        .join(",")
        .chars()
        .rev()
        .collect()
}
