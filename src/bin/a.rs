use itertools::Itertools;
// use rand::prelude::*;

const DIJ: [(usize, usize); 4] = [(0, !0), (!0, 0), (0, 1), (1, 0)];
const DIR: [char; 4] = ['L', 'U', 'R', 'D'];

fn main() {
    let input = read_input();
    let out: Output = Output::new(&input);
    write_output(&out);
    eprintln!("score:{}", compute_score(&mut input.maps.clone(), &out));
}

#[derive(PartialEq, PartialOrd, Clone, Copy)]
enum Square {
    Player,
    Coin,
    Trap,
    Wall,
    Empty,
}

#[allow(dead_code)]
struct Input {
    n: usize,
    k: usize,
    h: usize,
    w: usize,
    t: usize,
    maps: Vec<Vec<Vec<Square>>>,
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
    let maps = maps
        .into_iter()
        .map(|map| {
            map.into_iter()
                .map(|row| {
                    row.into_iter()
                        .map(|ch| match ch {
                            '@' => Square::Player,
                            'o' => Square::Coin,
                            'x' => Square::Trap,
                            '#' => Square::Wall,
                            _ => unreachable!("マップの入力が不正です"),
                        })
                        .collect()
                })
                .collect()
        })
        .collect();
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

fn compute_score(maps: &mut [Vec<Vec<Square>>], out: &Output) -> i64 {
    let mut score = 0;
    for &k in out.maps.iter() {
        let map = &mut maps[k];
        let (mut player_r, mut player_c) = find_player_position(map);
        for &ch in out.commands.iter() {
            if let Some(d) = DIR.iter().position(|dir_c| *dir_c == ch) {
                let next_r = player_r + DIJ[d].0;
                let next_c = player_c + DIJ[d].1;
                match map[next_r][next_c] {
                    Square::Wall => continue,
                    Square::Trap => break,
                    Square::Coin => score += 1,
                    Square::Empty => {}
                    Square::Player => unreachable!("プレイヤーが複数います"),
                }
                map[next_r][next_c] = Square::Empty;
                player_r = next_r;
                player_c = next_c;
            }
        }
    }
    score
}

fn find_player_position(map: &[Vec<Square>]) -> (usize, usize) {
    for (r, row) in map.iter().enumerate() {
        for (c, square) in row.iter().enumerate() {
            if *square == Square::Player {
                return (r, c);
            }
        }
    }
    unreachable!("map上にplayerが見つかりませんでした")
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
