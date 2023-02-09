use itertools::Itertools;
// use rand::prelude::*;

// const DIJ: [(usize, usize); 4] = [(0, !0), (!0, 0), (0, 1), (1, 0)];
// const DIR: [char; 4] = ['L', 'U', 'R', 'D'];

fn main() {
    let input = read_input();
    let out: Output = Output::new(&input);
    write_output(&out);
}

#[allow(dead_code)]
struct Input {
    n: usize,
    k: usize,
    h: usize,
    w: usize,
    t: usize,
    maps: Vec<Vec<Vec<char>>>,
}

fn read_input() -> Input {
    use proconio::{input, marker::Chars};
    input! {
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

struct Output {
    maps: Vec<usize>,
    commands: Vec<char>,
}

impl Output {
    fn new(input: &Input) -> Self {
        let maps = (0..input.k).collect();
        let commands = (0..input.t).map(|_| 'L').collect();
        Output { maps, commands }
    }
}

fn write_output(out: &Output) {
    println!("{}", out.maps.iter().join(" "));
    println!("{}", out.commands.iter().join(""));
}

// fn get_time() -> f64 {
//     static mut STIME: f64 = -1.0;
//     let t = std::time::SystemTime::now()
//         .duration_since(std::time::UNIX_EPOCH)
//         .unwrap();
//     let ms = t.as_secs() as f64 + t.subsec_nanos() as f64 * 1e-9;
//     unsafe {
//         if STIME < 0.0 {
//             STIME = ms;
//         }
//         // ローカル環境とジャッジ環境の実行速度差はget_timeで吸収しておくと便利
//         #[cfg(feature = "local")]
//         {
//             (ms - STIME) * 1.0
//         }
//         #[cfg(not(feature = "local"))]
//         {
//             ms - STIME
//         }
//     }
// }
