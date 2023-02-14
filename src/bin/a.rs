#![allow(clippy::uninlined_format_args)]

use itertools::Itertools;
// use rand::prelude::*;

// const TIMELIMIT: f64 = 3.9;

const DIJ: [(usize, usize); 4] = [(0, !0), (!0, 0), (0, 1), (1, 0)];
const DIR: [char; 4] = ['L', 'U', 'R', 'D'];
// const INF: i64 = 1_000_000_000_000;

fn main() {
    // let mut rng = rand_chacha::ChaCha20Rng::seed_from_u64(0);
    let input = read_input();
    let mut maps = input.maps.clone();
    let out = beam_search(&input, &maps);
    write_output(&out);
    eprintln!("score:{}", compute_score(&mut maps, &out));
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct State {
    score: i64,
    maps: Vec<Vec<Vec<Square>>>,
    poses: Vec<(usize, usize)>,
    commands: Vec<char>,
    captured: Vec<bool>,
}

impl State {
    fn new(
        score: i64,
        maps: Vec<Vec<Vec<Square>>>,
        poses: Vec<(usize, usize)>,
        commands: Vec<char>,
        captured: Vec<bool>,
    ) -> Self {
        State {
            score,
            maps,
            poses,
            commands,
            captured,
        }
    }

    fn convert_next_state(item: NextState, mut prev_state: State) -> Self {
        for (k, map) in prev_state.maps.iter_mut().enumerate() {
            if prev_state.captured[k] {
                continue;
            }
            let (player_r, player_c) = &mut prev_state.poses[k];
            let next_r = *player_r + DIJ[item.dir].0;
            let next_c = *player_c + DIJ[item.dir].1;
            match map[next_r][next_c] {
                Square::Wall => continue,
                Square::Trap => prev_state.captured[k] = true,
                Square::Coin => prev_state.score += 1,
                Square::Empty => {}
                Square::Player => unreachable!("プレイヤーが複数います"),
            }
            map[*player_r][*player_c] = Square::Empty;
            map[next_r][next_c] = Square::Player;
            *player_r = next_r;
            *player_c = next_c;
        }
        prev_state.commands.push(DIR[item.dir]);
        prev_state
    }
}

struct NextState {
    score: i64,
    prev_index: usize,
    dir: usize,
}

impl NextState {
    fn new(score: i64, prev_index: usize, dir: usize) -> Self {
        NextState {
            score,
            prev_index,
            dir,
        }
    }
}

fn beam_search(input: &Input, maps: &[Vec<Vec<Square>>]) -> Output {
    const BEAM_WIDTH: usize = 100;
    let mut states = vec![State::new(
        0,
        maps.iter().take(input.k).cloned().collect(),
        maps.iter().map(|map| find_player_position(map)).collect(),
        vec![],
        vec![false; input.k],
    )];
    for _ in 0..input.t {
        let mut next_states = vec![];
        for (i, state) in states.iter().enumerate().rev() {
            for (dir, &(dr, dc)) in DIJ.iter().enumerate() {
                let mut score = state.score;
                for (k, map) in state.maps.iter().enumerate() {
                    if state.captured[k] {
                        continue;
                    }
                    let (player_r, player_c) = &state.poses[k];
                    let next_r = *player_r + dr;
                    let next_c = *player_c + dc;
                    if map[next_r][next_c] == Square::Coin {
                        score += 1;
                    }
                }
                next_states.push(NextState::new(score, i, dir));
            }
        }

        if BEAM_WIDTH < next_states.len() {
            next_states.sort_by_key(|next_state| std::cmp::Reverse(next_state.score));
            next_states.truncate(BEAM_WIDTH);
        }
        let next_states = next_states
            .into_iter()
            .map(|item| {
                let prev_state = states[item.prev_index].clone();
                State::convert_next_state(item, prev_state)
            })
            .collect();
        states = next_states;
    }
    let best_state = states.into_iter().max().unwrap();
    Output::new((0..input.k).collect(), best_state.commands)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
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
    fn new(maps: Vec<usize>, commands: Vec<char>) -> Self {
        Output { maps, commands }
    }
}

#[allow(dead_code)]
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

#[allow(dead_code)]
fn compute_score(maps: &mut [Vec<Vec<Square>>], out: &Output) -> i64 {
    let mut score = 0;
    for &k in out.maps.iter() {
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

#[allow(dead_code)]
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
