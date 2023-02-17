#![allow(clippy::uninlined_format_args)]

use fixedbitset::FixedBitSet;
use itertools::Itertools;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

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
    score: i32,
    maps: Vec<Vec<Vec<Square>>>,
    poses: Vec<(usize, usize)>,
    captured: Vec<usize>,
    turn: usize,
}

impl State {
    fn new(
        score: i32,
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
            if act.is_moved[k] {
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
            if !act.is_moved[k] {
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

struct Candidate {
    score: i32,
    parent: Rc<Node>,
    act: Action,
}

impl Candidate {
    fn new(score: i32, parent: Rc<Node>, act: Action) -> Self {
        Candidate { score, parent, act }
    }
}

struct Action {
    dir: usize,
    is_moved: FixedBitSet,
    is_gained: FixedBitSet,
}

impl Action {
    fn new(dir: usize, is_moved: FixedBitSet, is_gained: FixedBitSet) -> Self {
        Action {
            dir,
            is_moved,
            is_gained,
        }
    }
}

struct Node {
    score: i32,
    parent: Option<Rc<Node>>,
    children: RefCell<Vec<(Action, Weak<Node>)>>,
}

impl Node {
    fn new(score: i32, parent: Option<Rc<Node>>) -> Self {
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
    fn dfs(&mut self, candidates: &mut Vec<Candidate>, target_turn: usize, is_single_path: bool) {
        if self.state.turn == target_turn {
            for (dir, &(dr, dc)) in DIJ.iter().enumerate() {
                let mut score = self.head.score;
                let mut is_moved = FixedBitSet::with_capacity(self.state.maps.len());
                let mut is_gained = FixedBitSet::with_capacity(self.state.maps.len());
                for (k, map) in self.state.maps.iter().enumerate() {
                    if self.state.captured[k] > 0 {
                        continue;
                    }
                    let (player_r, player_c) = &self.state.poses[k];
                    let next_r = *player_r + dr;
                    let next_c = *player_c + dc;
                    if map[next_r][next_c] == Square::Coin {
                        score += 1;
                        is_gained.set(k, true);
                        is_moved.set(k, true);
                    }
                    if map[next_r][next_c] == Square::Empty {
                        is_moved.set(k, true);
                    }
                }
                candidates.push(Candidate::new(
                    score,
                    self.head.clone(),
                    Action::new(dir, is_moved, is_gained),
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

                        self.dfs(candidates, target_turn, next_is_single_path);

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
    let mut current_queue = vec![tree.head.clone()];
    let mut candidates = vec![];
    for turn in 0..input.t {
        tree.dfs(&mut candidates, turn, true);
        current_queue.clear();
        if turn + 1 == input.t {
            // 最終ターンなのでcandidatesを作らない
            break;
        }
        if candidates.len() > BEAM_WIDTH {
            selection::select_nth_unstable_by_key(&mut candidates, BEAM_WIDTH - 1, |c| {
                std::cmp::Reverse(c.score)
            });
            candidates.truncate(BEAM_WIDTH);
        }
        for candidate in std::mem::take(&mut candidates) {
            let child = Node::new(candidate.score, Some(candidate.parent.clone()));
            let child_ptr = Rc::new(child);
            let children = &mut candidate.parent.children.borrow_mut();
            children.push((candidate.act, Rc::downgrade(&child_ptr)));
            current_queue.push(child_ptr);
        }
    }
    let best_candidate = candidates
        .into_iter()
        .max_by_key(|state| state.score)
        .unwrap();

    // 復元
    let mut commands = vec![DIR[best_candidate.act.dir]];
    commands.reserve(input.t);
    let mut parent = best_candidate.parent;
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
fn compute_map_score(map: &mut [Vec<Square>], out: &Output) -> i32 {
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
fn compute_score(maps: &mut [Vec<Vec<Square>>], out: &Output) -> i32 {
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

/// select_nth_unstable()をRust 1.49以前でも使えるようにするモジュール
/// 言語アップデートが行われれば不要になるはず
#[allow(dead_code)]
mod selection {
    use std::cmp;
    use std::mem::{self, MaybeUninit};
    use std::ptr;

    struct CopyOnDrop<T> {
        src: *const T,
        dest: *mut T,
    }

    impl<T> Drop for CopyOnDrop<T> {
        fn drop(&mut self) {
            unsafe {
                ptr::copy_nonoverlapping(self.src, self.dest, 1);
            }
        }
    }

    fn shift_tail<T, F>(v: &mut [T], is_less: &mut F)
    where
        F: FnMut(&T, &T) -> bool,
    {
        let len = v.len();
        unsafe {
            if len >= 2 && is_less(v.get_unchecked(len - 1), v.get_unchecked(len - 2)) {
                let tmp = mem::ManuallyDrop::new(ptr::read(v.get_unchecked(len - 1)));
                let v = v.as_mut_ptr();
                let mut hole = CopyOnDrop {
                    src: &*tmp,
                    dest: v.add(len - 2),
                };
                ptr::copy_nonoverlapping(v.add(len - 2), v.add(len - 1), 1);

                for i in (0..len - 2).rev() {
                    if !is_less(&*tmp, &*v.add(i)) {
                        break;
                    }

                    ptr::copy_nonoverlapping(v.add(i), v.add(i + 1), 1);
                    hole.dest = v.add(i);
                }
            }
        }
    }

    fn insertion_sort<T, F>(v: &mut [T], is_less: &mut F)
    where
        F: FnMut(&T, &T) -> bool,
    {
        for i in 1..v.len() {
            shift_tail(&mut v[..i + 1], is_less);
        }
    }

    fn partition_in_blocks<T, F>(v: &mut [T], pivot: &T, is_less: &mut F) -> usize
    where
        F: FnMut(&T, &T) -> bool,
    {
        const BLOCK: usize = 128;

        let mut l = v.as_mut_ptr();
        let mut block_l = BLOCK;
        let mut start_l = ptr::null_mut();
        let mut end_l = ptr::null_mut();
        let mut offsets_l = [MaybeUninit::<u8>::uninit(); BLOCK];

        let mut r = unsafe { l.add(v.len()) };
        let mut block_r = BLOCK;
        let mut start_r = ptr::null_mut();
        let mut end_r = ptr::null_mut();
        let mut offsets_r = [MaybeUninit::<u8>::uninit(); BLOCK];

        fn width<T>(l: *mut T, r: *mut T) -> usize {
            assert!(mem::size_of::<T>() > 0);
            (r as usize - l as usize) / mem::size_of::<T>()
        }

        loop {
            let is_done = width(l, r) <= 2 * BLOCK;

            if is_done {
                let mut rem = width(l, r);
                if start_l < end_l || start_r < end_r {
                    rem -= BLOCK;
                }

                if start_l < end_l {
                    block_r = rem;
                } else if start_r < end_r {
                    block_l = rem;
                } else {
                    block_l = rem / 2;
                    block_r = rem - block_l;
                }
                debug_assert!(block_l <= BLOCK && block_r <= BLOCK);
                debug_assert!(width(l, r) == block_l + block_r);
            }

            if start_l == end_l {
                start_l = offsets_l.as_mut_ptr() as *mut _;
                end_l = start_l;
                let mut elem = l;

                for i in 0..block_l {
                    unsafe {
                        *end_l = i as u8;
                        end_l = end_l.offset(!is_less(&*elem, pivot) as isize);
                        elem = elem.offset(1);
                    }
                }
            }

            if start_r == end_r {
                start_r = offsets_r.as_mut_ptr() as *mut _;
                end_r = start_r;
                let mut elem = r;

                for i in 0..block_r {
                    unsafe {
                        elem = elem.offset(-1);
                        *end_r = i as u8;
                        end_r = end_r.offset(is_less(&*elem, pivot) as isize);
                    }
                }
            }

            let count = cmp::min(width(start_l, end_l), width(start_r, end_r));

            if count > 0 {
                macro_rules! left {
                    () => {
                        l.offset(*start_l as isize)
                    };
                }
                macro_rules! right {
                    () => {
                        r.offset(-(*start_r as isize) - 1)
                    };
                }

                unsafe {
                    let tmp = ptr::read(left!());
                    ptr::copy_nonoverlapping(right!(), left!(), 1);

                    for _ in 1..count {
                        start_l = start_l.offset(1);
                        ptr::copy_nonoverlapping(left!(), right!(), 1);
                        start_r = start_r.offset(1);
                        ptr::copy_nonoverlapping(right!(), left!(), 1);
                    }

                    ptr::copy_nonoverlapping(&tmp, right!(), 1);
                    mem::forget(tmp);
                    start_l = start_l.offset(1);
                    start_r = start_r.offset(1);
                }
            }

            if start_l == end_l {
                l = unsafe { l.offset(block_l as isize) };
            }

            if start_r == end_r {
                r = unsafe { r.offset(-(block_r as isize)) };
            }

            if is_done {
                break;
            }
        }

        if start_l < end_l {
            debug_assert_eq!(width(l, r), block_l);
            while start_l < end_l {
                unsafe {
                    end_l = end_l.offset(-1);
                    ptr::swap(l.offset(*end_l as isize), r.offset(-1));
                    r = r.offset(-1);
                }
            }
            width(v.as_mut_ptr(), r)
        } else if start_r < end_r {
            debug_assert_eq!(width(l, r), block_r);
            while start_r < end_r {
                unsafe {
                    end_r = end_r.offset(-1);
                    ptr::swap(l, r.offset(-(*end_r as isize) - 1));
                    l = l.offset(1);
                }
            }
            width(v.as_mut_ptr(), l)
        } else {
            width(v.as_mut_ptr(), l)
        }
    }

    fn partition<T, F>(v: &mut [T], pivot: usize, is_less: &mut F) -> (usize, bool)
    where
        F: FnMut(&T, &T) -> bool,
    {
        let (mid, was_partitioned) = {
            v.swap(0, pivot);
            let (pivot, v) = v.split_at_mut(1);
            let pivot = &mut pivot[0];

            let tmp = mem::ManuallyDrop::new(unsafe { ptr::read(pivot) });
            let _pivot_guard = CopyOnDrop {
                src: &*tmp,
                dest: pivot,
            };
            let pivot = &*tmp;

            let mut l = 0;
            let mut r = v.len();

            unsafe {
                while l < r && is_less(v.get_unchecked(l), pivot) {
                    l += 1;
                }

                while l < r && !is_less(v.get_unchecked(r - 1), pivot) {
                    r -= 1;
                }
            }

            (
                l + partition_in_blocks(&mut v[l..r], pivot, is_less),
                l >= r,
            )
        };

        v.swap(0, mid);

        (mid, was_partitioned)
    }

    fn partition_equal<T, F>(v: &mut [T], pivot: usize, is_less: &mut F) -> usize
    where
        F: FnMut(&T, &T) -> bool,
    {
        v.swap(0, pivot);
        let (pivot, v) = v.split_at_mut(1);
        let pivot = &mut pivot[0];

        let tmp = mem::ManuallyDrop::new(unsafe { ptr::read(pivot) });
        let _pivot_guard = CopyOnDrop {
            src: &*tmp,
            dest: pivot,
        };
        let pivot = &*tmp;

        let mut l = 0;
        let mut r = v.len();
        loop {
            unsafe {
                while l < r && !is_less(pivot, v.get_unchecked(l)) {
                    l += 1;
                }

                while l < r && is_less(pivot, v.get_unchecked(r - 1)) {
                    r -= 1;
                }

                if l >= r {
                    break;
                }

                r -= 1;
                let ptr = v.as_mut_ptr();
                ptr::swap(ptr.add(l), ptr.add(r));
                l += 1;
            }
        }

        l + 1
    }

    fn choose_pivot<T, F>(v: &mut [T], is_less: &mut F) -> (usize, bool)
    where
        F: FnMut(&T, &T) -> bool,
    {
        const SHORTEST_MEDIAN_OF_MEDIANS: usize = 50;
        const MAX_SWAPS: usize = 4 * 3;

        let len = v.len();

        let mut a = len / 4 * 1;
        let mut b = len / 4 * 2;
        let mut c = len / 4 * 3;

        let mut swaps = 0;

        if len >= 8 {
            let mut sort2 = |a: &mut usize, b: &mut usize| unsafe {
                if is_less(v.get_unchecked(*b), v.get_unchecked(*a)) {
                    ptr::swap(a, b);
                    swaps += 1;
                }
            };

            let mut sort3 = |a: &mut usize, b: &mut usize, c: &mut usize| {
                sort2(a, b);
                sort2(b, c);
                sort2(a, b);
            };

            if len >= SHORTEST_MEDIAN_OF_MEDIANS {
                let mut sort_adjacent = |a: &mut usize| {
                    let tmp = *a;
                    sort3(&mut (tmp - 1), a, &mut (tmp + 1));
                };

                sort_adjacent(&mut a);
                sort_adjacent(&mut b);
                sort_adjacent(&mut c);
            }

            sort3(&mut a, &mut b, &mut c);
        }

        if swaps < MAX_SWAPS {
            (b, swaps == 0)
        } else {
            v.reverse();
            (len - 1 - b, true)
        }
    }

    fn partition_at_index_loop<'a, T, F>(
        mut v: &'a mut [T],
        mut index: usize,
        is_less: &mut F,
        mut pred: Option<&'a T>,
    ) where
        F: FnMut(&T, &T) -> bool,
    {
        loop {
            const MAX_INSERTION: usize = 10;
            if v.len() <= MAX_INSERTION {
                insertion_sort(v, is_less);
                return;
            }

            let (pivot, _) = choose_pivot(v, is_less);

            if let Some(p) = pred {
                if !is_less(p, &v[pivot]) {
                    let mid = partition_equal(v, pivot, is_less);

                    if mid > index {
                        return;
                    }

                    v = &mut v[mid..];
                    index = index - mid;
                    pred = None;
                    continue;
                }
            }

            let (mid, _) = partition(v, pivot, is_less);

            let (left, right) = v.split_at_mut(mid);
            let (pivot, right) = right.split_at_mut(1);
            let pivot = &pivot[0];

            if mid < index {
                v = right;
                index = index - mid - 1;
                pred = Some(pivot);
            } else if mid > index {
                v = left;
            } else {
                return;
            }
        }
    }

    fn partition_at_index<T, F>(
        v: &mut [T],
        index: usize,
        mut is_less: F,
    ) -> (&mut [T], &mut T, &mut [T])
    where
        F: FnMut(&T, &T) -> bool,
    {
        use cmp::Ordering::Greater;
        use cmp::Ordering::Less;

        if index >= v.len() {
            panic!(
                "partition_at_index index {} greater than length of slice {}",
                index,
                v.len()
            );
        }

        if mem::size_of::<T>() == 0 {
        } else if index == v.len() - 1 {
            let (max_index, _) = v
                .iter()
                .enumerate()
                .max_by(|&(_, x), &(_, y)| if is_less(x, y) { Less } else { Greater })
                .unwrap();
            v.swap(max_index, index);
        } else if index == 0 {
            let (min_index, _) = v
                .iter()
                .enumerate()
                .min_by(|&(_, x), &(_, y)| if is_less(x, y) { Less } else { Greater })
                .unwrap();
            v.swap(min_index, index);
        } else {
            partition_at_index_loop(v, index, &mut is_less, None);
        }

        let (left, right) = v.split_at_mut(index);
        let (pivot, right) = right.split_at_mut(1);
        let pivot = &mut pivot[0];
        (left, pivot, right)
    }

    #[inline]
    pub fn select_nth_unstable_by_key<T, K, F>(
        slice: &mut [T],
        index: usize,
        mut f: F,
    ) -> (&mut [T], &mut T, &mut [T])
    where
        F: FnMut(&T) -> K,
        K: Ord,
    {
        let mut g = |a: &T, b: &T| f(a).lt(&f(b));
        partition_at_index(slice, index, &mut g)
    }
}
