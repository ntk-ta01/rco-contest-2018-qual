#![allow(clippy::uninlined_format_args)]

use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use itertools::Itertools;

const DIJ: [(usize, usize); 4] = [(0, !0), (!0, 0), (0, 1), (1, 0)];
const DIR: [char; 4] = ['L', 'U', 'R', 'D'];

fn main() {
    let input = read_input();
    let maps = input.maps.clone();
    let out = beam_search(&input, &maps);
    write_output(&out);
    // eprintln!("score:{}", compute_score(&mut maps, &out));
}

#[derive(Clone)]
struct State {
    score: i64,
    maps: Vec<Vec<Vec<Square>>>,
    poses: Vec<(usize, usize)>,
    captured: Vec<usize>,
    turn: usize,
}

impl State {
    fn new(
        score: i64,
        maps: Vec<Vec<Vec<Square>>>,
        poses: Vec<(usize, usize)>,
        captured: Vec<usize>,
    ) -> Self {
        State {
            score,
            maps,
            poses,
            captured,
            turn: 0,
        }
    }

    fn apply(&mut self, act: &Action) {
        self.turn += 1;
        for (k, map) in self.maps.iter_mut().enumerate() {
            if self.captured[k] > 0 {
                self.captured[k] += 1;
                continue;
            }
            let (player_r, player_c) = &mut self.poses[k];
            let next_r = *player_r + DIJ[act.dir].0;
            let next_c = *player_c + DIJ[act.dir].1;
            match map[next_r][next_c] {
                Square::Wall => continue,
                Square::Trap => self.captured[k] = 1,
                Square::Coin => {
                    self.score += 1;
                }
                Square::Empty => {}
                Square::Player => unreachable!("プレイヤーが複数います"),
            }
            if act.is_move_successed[k] {
                map[*player_r][*player_c] = Square::Empty;
                map[next_r][next_c] = Square::Player;
                *player_r = next_r;
                *player_c = next_c;
            }
        }
    }

    fn revert(&mut self, act: &Action) {
        self.turn -= 1;
        let rev_dir = (act.dir + 2) % 4;
        for (k, map) in self.maps.iter_mut().enumerate() {
            if !act.is_move_successed[k] {
                if self.captured[k] > 0 {
                    self.captured[k] -= 1;
                }
                continue;
            }
            let (player_r, player_c) = &mut self.poses[k];
            let prev_r = *player_r + DIJ[rev_dir].0;
            let prev_c = *player_c + DIJ[rev_dir].1;
            match map[prev_r][prev_c] {
                Square::Wall => unreachable!("壁にぶつかるように戻っています"),
                Square::Trap => unreachable!("罠にぶつかるように戻っています"),
                Square::Coin => unreachable!("コインを取っていないマスに戻っています"),
                Square::Empty => {}
                Square::Player => unreachable!("プレイヤーが複数います"),
            }
            if act.is_gained[k] {
                self.score -= 1;
                map[*player_r][*player_c] = Square::Coin;
            } else {
                map[*player_r][*player_c] = Square::Empty;
            }
            map[prev_r][prev_c] = Square::Player;
            *player_r = prev_r;
            *player_c = prev_c;
        }
    }
}

struct NextState {
    score: i64,
    parent: Rc<Node>,
    act: Action,
}

impl NextState {
    fn new(score: i64, parent: Rc<Node>, act: Action) -> Self {
        NextState { score, parent, act }
    }
}

struct Action {
    dir: usize,
    is_move_successed: Vec<bool>,
    is_gained: Vec<bool>,
}

impl Action {
    fn new(dir: usize, is_move_successed: Vec<bool>, is_gained: Vec<bool>) -> Self {
        Action {
            dir,
            is_move_successed,
            is_gained,
        }
    }
}

struct Node {
    score: i64,
    parent: Option<Rc<Node>>,
    children: RefCell<Vec<(Action, Weak<Node>)>>,
}

impl Node {
    fn new(score: i64, parent: Option<Rc<Node>>) -> Self {
        let children = RefCell::new(vec![]);
        Self {
            score,
            parent,
            children,
        }
    }
}

struct BeamSearchTree {
    state: State,
    head: Rc<Node>, // 木上の探索中のnodeを保持する
}

impl BeamSearchTree {
    fn dfs(&mut self, next_states: &mut Vec<NextState>, target_turn: usize, is_single_path: bool) {
        if self.state.turn == target_turn {
            for (dir, &(dr, dc)) in DIJ.iter().enumerate() {
                let mut score = self.head.score;
                let mut is_move_successed = vec![false; self.state.maps.len()];
                let mut is_gained = vec![false; self.state.maps.len()];
                for (k, map) in self.state.maps.iter().enumerate() {
                    if self.state.captured[k] > 0 {
                        continue;
                    }
                    let (player_r, player_c) = &self.state.poses[k];
                    let next_r = *player_r + dr;
                    let next_c = *player_c + dc;
                    if map[next_r][next_c] == Square::Coin {
                        score += 1;
                        is_gained[k] = true;
                        is_move_successed[k] = true;
                    }
                    if map[next_r][next_c] == Square::Empty {
                        is_move_successed[k] = true;
                    }
                }
                next_states.push(NextState::new(
                    score,
                    self.head.clone(),
                    Action::new(dir, is_move_successed, is_gained),
                ));
            }
        } else {
            let node = self.head.clone();

            let next_is_single_path = {
                let children = &mut node.children.borrow_mut();
                children.retain(|(_, child)| child.upgrade().is_some());
                if children.is_empty() {
                    false
                } else {
                    let next_is_single_path = is_single_path && (children.len() == 1);
                    for (act, child) in children.iter_mut() {
                        let next_turn = self.state.turn + 1;
                        if next_turn > target_turn {
                            continue;
                        }

                        self.head = child.upgrade().unwrap();
                        self.state.apply(act);

                        self.dfs(next_states, target_turn, next_is_single_path);

                        if !next_is_single_path {
                            self.state.revert(act);
                        }
                    }
                    next_is_single_path
                }
            };

            // nodeの子が1つでなければ、headを元のnodeに戻す
            if !next_is_single_path {
                self.head = node;
            }
        }
    }
}

fn beam_search(input: &Input, maps: &[Vec<Vec<Square>>]) -> Output {
    const BEAM_WIDTH: usize = 1000;
    let mut tree = {
        let state = State::new(
            0,
            maps.iter().take(input.k).cloned().collect(),
            maps.iter().map(|map| find_player_position(map)).collect(),
            vec![0; input.k],
        );
        let head = Rc::new(Node::new(0, None));
        BeamSearchTree { state, head }
    };
    let mut current_states = vec![tree.head.clone()];
    let mut next_states = vec![];
    for turn in 0..input.t {
        tree.dfs(&mut next_states, turn, true);
        current_states.clear();
        if turn + 1 == input.t {
            // 最終ターンなのでnext_statesを作らない
            break;
        }
        if next_states.len() > BEAM_WIDTH {
            next_states.sort_by_key(|next_state| std::cmp::Reverse(next_state.score));
            next_states.truncate(BEAM_WIDTH);
        }
        for next_state in std::mem::take(&mut next_states) {
            let child = Node::new(next_state.score, Some(next_state.parent.clone()));
            let child_ptr = Rc::new(child);
            let children = &mut next_state.parent.children.borrow_mut();
            children.push((next_state.act, Rc::downgrade(&child_ptr)));
            current_states.push(child_ptr);
        }
    }
    let best_state = next_states
        .into_iter()
        .max_by_key(|state| state.score)
        .unwrap();

    // 復元
    let mut commands = vec![DIR[best_state.act.dir]];
    commands.reserve(input.t);
    let mut parent = best_state.parent;
    while let Some(p) = parent.parent.clone() {
        {
            let children = &mut p.children.borrow_mut();
            let (act, _) = children
                .iter()
                .find(|(_, child)| child.upgrade().is_some())
                .unwrap();
            commands.push(DIR[act.dir]);
        }
        parent = p;
    }
    commands.reverse();
    Output::new((0..input.k).collect(), commands)
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
