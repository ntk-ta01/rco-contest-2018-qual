#![allow(clippy::uninlined_format_args)]

use itertools::Itertools;
use rand::prelude::*;
use std::collections::VecDeque;

const TIMELIMIT: f64 = 3.9;

const DIJ: [(usize, usize); 4] = [(0, !0), (!0, 0), (0, 1), (1, 0)];
const DIR: [char; 4] = ['L', 'U', 'R', 'D'];
const INF: i64 = 1_000_000_000_000;

fn main() {
    let mut rng = rand_chacha::ChaCha20Rng::seed_from_u64(0);
    let input = read_input();
    let mut maps = input.maps.clone();
    let mut out = Output::new(&input);
    greedy(&input, &maps, &mut out, &mut rng);
    annealing(&input, &mut maps, &mut out, &mut rng);
    write_output(&out);
    // eprintln!("score:{}", compute_score(&mut maps.clone(), &out).0);
}

fn annealing<T: Rng>(_input: &Input, maps: &mut [Vec<Vec<Square>>], out: &mut Output, rng: &mut T) {
    const T0: f64 = 100.0;
    const T1: f64 = 0.01;
    let mut temp;
    let mut prob;
    let mut now_score = compute_score(&mut maps.to_vec(), out);

    let mut best_score = now_score;
    let mut best_out = out.clone();

    loop {
        let passed = get_time() / TIMELIMIT;
        if 1.0 <= passed {
            break;
        }
        temp = T0.powf(1.0 - passed) * T1.powf(passed);

        let mut new_out = out.clone();
        // 近傍解作成
        // コマンドをランダムに一文字変える
        let change_com_i = rng.gen_range(0, out.commands.len());
        let new_com = DIR[rng.gen_range(0, DIR.len())];
        new_out.commands[change_com_i] = new_com;
        // 近傍解作成ここまで
        let new_score = compute_score(&mut maps.to_vec(), &new_out);
        prob = f64::exp((new_score - now_score) as f64 / temp);
        if now_score < new_score || rng.gen_bool(prob) {
            now_score = new_score;
            *out = new_out;
        }

        if best_score < now_score {
            best_score = now_score;
            best_out = out.clone();
        }
    }
    *out = best_out;
    eprintln!("score:{}", best_score);
}

fn greedy<T: Rng>(input: &Input, maps: &[Vec<Vec<Square>>], out: &mut Output, _rng: &mut T) {
    // k個のマップそれぞれでBFSして、発券したうちで最大の距離のパスをそのマップのコマンドとする
    let mut commands = vec![];
    for map in maps.iter() {
        let mut prev_dir = vec![vec![!0; input.w]; input.h];
        let mut dist = vec![vec![INF; input.w]; input.h];
        let mut max_dist = 0;
        let mut max_pos = find_player_position(map);
        let mut que = VecDeque::new();
        dist[max_pos.0][max_pos.1] = 0;
        que.push_back(max_pos);
        while !que.is_empty() {
            let (ur, uc) = que.pop_front().unwrap();
            for (dir, &(dr, dc)) in DIJ.iter().enumerate() {
                let nr = ur + dr;
                let nc = uc + dc;
                match map[nr][nc] {
                    Square::Wall => continue,
                    Square::Trap => continue,
                    Square::Coin => {
                        if dist[nr][nc] != INF {
                            continue;
                        }
                        dist[nr][nc] = dist[ur][uc] + 1;
                        if max_dist < dist[nr][nc] {
                            max_dist = dist[nr][nc];
                            max_pos = (nr, nc);
                        }
                        prev_dir[nr][nc] = dir;
                        que.push_back((nr, nc));
                    }
                    Square::Empty => unreachable!("存在しない空のマスに到達しています"),
                    Square::Player => continue,
                }
            }
        }
        let mut pos = max_pos;
        let mut command = vec![];
        while prev_dir[pos.0][pos.1] != !0 {
            command.push(DIR[prev_dir[pos.0][pos.1]]);
            let dir = (prev_dir[pos.0][pos.1] + 2) % 4; // 一つ前のマスに戻る進行方向に変換する
            pos.0 += DIJ[dir].0;
            pos.1 += DIJ[dir].1;
        }
        command.reverse(); // 最後のマスから最初のマスの順になっているので反転
        commands.push(command);
    }
    // n個のマップからコマンドの長い順にk個のマップを選ぶ
    let mut maps = (0..input.n).collect_vec();
    maps.sort_by_key(|&k| std::cmp::Reverse(commands[k].len()));
    for (i, &k) in maps.iter().enumerate().take(input.k) {
        out.maps[i] = k;
    }
    // n個のコマンドから最長のコマンドを選ぶ
    let commands_i = maps[0];
    out.commands = std::mem::take(&mut commands[commands_i]);
    while out.commands.len() < input.t {
        out.commands.push('L');
    }
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

#[derive(Clone)]
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

fn compute_map_score(map: &mut [Vec<Square>], out: &Output) -> i64 {
    let (mut player_r, mut player_c) = find_player_position(map);
    let mut score = 0;
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
            map[player_r][player_c] = Square::Empty;
            player_r = next_r;
            player_c = next_c;
        }
    }
    score
}

fn compute_score(maps: &mut [Vec<Vec<Square>>], out: &Output) -> i64 {
    let mut score = 0;
    for &k in out.maps.iter().take(1) {
        let map = &mut maps[k];
        score += compute_map_score(map, out);
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

fn get_time() -> f64 {
    static mut STIME: f64 = -1.0;
    let t = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap();
    let ms = t.as_secs() as f64 + t.subsec_nanos() as f64 * 1e-9;
    unsafe {
        if STIME < 0.0 {
            STIME = ms;
        }
        // ローカル環境とジャッジ環境の実行速度差はget_timeで吸収しておくと便利
        #[cfg(feature = "local")]
        {
            (ms - STIME) * 1.0
        }
        #[cfg(not(feature = "local"))]
        {
            ms - STIME
        }
    }
}
