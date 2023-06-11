use grid::*;
use itertools::Itertools;
use rand::prelude::*;

fn main() {
    let input = read_input();
    let map_ids = select_maps(&input);
    let state = State::new(&input, map_ids.clone());
    let width_manager = RampWidthManager::new(1250, 3500);
    let generator = ActionGenerator::new(&input);
    let actions = beam_search::BeamSearch::new(input.t, width_manager, generator).run(state);
    write_output(actions, map_ids);
}

struct RampWidthManager {
    width1: usize,
    width2: usize,
}

impl RampWidthManager {
    fn new(width1: usize, width2: usize) -> Self {
        RampWidthManager { width1, width2 }
    }
}

impl beam_search::WidthManager for RampWidthManager {
    fn beam_width(&self, turn: usize, _elapsed: std::time::Duration) -> usize {
        let t = turn as f64 / 2500.0; // max_turn = 2500
        (self.width1 as f64 * (1.0 - t) + self.width2 as f64 * t) as usize // だんだんwidth1 -> width2になるようにする
    }
}

// k個のマップを選ぶ
fn select_maps(input: &Input) -> Vec<usize> {
    // (コインの個数 - 罠の個数)が大きい方からk個選ぶ
    // 個数はplayerの位置からdfsで数える
    let mut fs = vec![];
    for (i, map) in input.maps.iter().enumerate() {
        let pos = find_player_position(input, map);
        let f_value = dfs(input, pos, map);
        fs.push((f_value, i));
    }
    fs.sort_by_key(|&(f, _)| std::cmp::Reverse(f));
    fs.truncate(input.k);
    fs.into_iter().map(|(_, i)| i).collect()
}

fn dfs(input: &Input, s: Coordinate, map: &Map2d<Square>) -> i32 {
    let mut coins = 0;
    let mut stack = vec![];
    let mut seen = Map2d::new(vec![false; input.w * input.h], input.w, input.h);
    seen[s] = true;
    stack.push(s);
    while let Some(coord) = stack.pop() {
        for &adj in ADJACENTS.iter() {
            let next_coord = coord + adj;
            if seen[next_coord]
                || map[next_coord] == Square::Wall
                || map[next_coord] == Square::Trap
            {
                continue;
            }
            if map[next_coord] == Square::Coin {
                coins += 1;
            }
            seen[next_coord] = true;
            stack.push(next_coord);
        }
    }
    coins
}

#[derive(Clone)]
struct State {
    score: i32,
    map_ids: Vec<usize>,
    poses: Vec<Coordinate>,
    visited: Vec<Map2d<bool>>,
    turn: usize,
    hash: u64,
}

impl State {
    fn new(input: &Input, map_ids: Vec<usize>) -> Self {
        let poses: Vec<Coordinate> = map_ids
            .iter()
            .map(|&map_id| find_player_position(input, &input.maps[map_id]))
            .collect();
        let mut hash = 0;
        for (i, &c) in poses.iter().enumerate() {
            hash ^= input.player_hashes[i][c];
        }
        let mut visited =
            vec![Map2d::new(vec![false; input.w * input.h], input.w, input.h); input.k];
        for (k, &c) in poses.iter().enumerate() {
            visited[k][c] = true;
        }
        State {
            score: 0,
            map_ids,
            poses,
            visited,
            turn: 0,
            hash,
        }
    }
}

impl beam_search::State<Action> for State {
    fn score(&self) -> i32 {
        self.score
    }

    fn turn(&self) -> usize {
        self.turn
    }

    fn hash(&self) -> u64 {
        self.hash
    }

    fn apply(&mut self, act: &Action) {
        self.turn += 1;
        self.score += act.score_diff;
        self.hash ^= act.hash_diff;
        for (k, player_coord) in self.poses.iter_mut().enumerate() {
            let next_coord = *player_coord + ADJACENTS[act.dir];
            if (act.is_moved >> k) & 1 == 1 {
                *player_coord = next_coord;
            }
            if (act.is_new_visited >> k) & 1 == 1 {
                self.visited[k][*player_coord] = true;
            }
        }
    }

    fn revert(&mut self, act: &Action) {
        self.turn -= 1;
        self.score -= act.score_diff;
        self.hash ^= act.hash_diff;
        let rev_dir = (act.dir + 2) % 4;
        for (k, player_coord) in self.poses.iter_mut().enumerate() {
            let prev_coord = *player_coord + ADJACENTS[rev_dir];
            if (act.is_new_visited >> k) & 1 == 1 {
                self.visited[k][*player_coord] = false;
            }
            if (act.is_moved >> k) & 1 == 1 {
                *player_coord = prev_coord;
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Action {
    dir: usize,
    is_moved: u8,
    is_new_visited: u8,
    score_diff: i32,
    hash_diff: u64,
}

impl beam_search::Action for Action {
    fn get_consumed_turn(&self) -> usize {
        1
    }
}

impl Action {
    fn new(dir: usize, is_moved: u8, is_new_visited: u8, score_diff: i32, hash_diff: u64) -> Self {
        Action {
            dir,
            is_moved,
            is_new_visited,
            score_diff,
            hash_diff,
        }
    }
}
struct ActionGenerator<'a> {
    input: &'a Input,
}

impl<'a> ActionGenerator<'a> {
    fn new(input: &'a Input) -> Self {
        Self { input }
    }
}

impl<'a> beam_search::ActionGenerator<State, Action> for ActionGenerator<'a> {
    fn enumerate(&self, state: &mut State, stack: &mut Vec<(Action, i32, u64)>) {
        for (dir, &adj) in ADJACENTS.iter().enumerate() {
            let mut score_diff = 0;
            let mut is_moved = 0;
            let mut is_new_visited = 0;
            let mut hash_diff = 0;
            for (k, &map_id) in state.map_ids.iter().enumerate() {
                let map = &self.input.maps[map_id];
                let player_coord = &state.poses[k];
                let next_coord = *player_coord + adj;
                if map[*player_coord] == Square::Trap || map[next_coord] == Square::Wall {
                    continue;
                }
                is_moved |= 1 << k;
                if !state.visited[k][next_coord] {
                    is_new_visited |= 1 << k;
                    if map[next_coord] == Square::Coin {
                        score_diff += 1;
                    } else if map[next_coord] == Square::Trap {
                        score_diff -= (self.input.t - state.turn) as i32 / 2;
                    }
                }
                hash_diff ^= self.input.player_hashes[k][*player_coord];
                hash_diff ^= self.input.player_hashes[k][next_coord];
            }

            let action = Action::new(dir, is_moved, is_new_visited, score_diff, hash_diff);
            stack.push((action, state.score + score_diff, state.hash ^ hash_diff));
        }
    }
}

// rhooさんとterry_u16さんとterry_x86さんに感謝しています
mod beam_search {
    use itertools::Itertools;
    use rustc_hash::FxHashSet;
    use std::{
        cell::RefCell,
        cmp::Reverse,
        marker::PhantomData,
        rc::{Rc, Weak},
        time::{Duration, Instant},
    };

    pub trait Action {
        /// 変形により消費されるターン数（デフォルト値:1）
        fn get_consumed_turn(&self) -> usize {
            1
        }
    }
    pub trait State<A: Action> {
        // 変形操作を適用する
        fn apply(&mut self, action: &A);

        // 変形操作の逆操作を行う
        fn revert(&mut self, action: &A);

        /// 状態のスコア
        fn score(&self) -> i32;

        /// ターン数
        fn turn(&self) -> usize;

        /// ハッシュ
        fn hash(&self) -> u64;
    }

    #[allow(clippy::type_complexity)]
    struct Node<S, A>
    where
        S: State<A>,
        A: Action,
    {
        /// 親ノードへの参照
        parent: Option<Rc<Node<S, A>>>,
        /// (操作, 子ノード)のVec
        children: RefCell<Vec<(A, Weak<Node<S, A>>)>>,
    }

    impl<S, A> Node<S, A>
    where
        S: State<A>,
        A: Action,
    {
        fn new(parent: Option<Rc<Node<S, A>>>) -> Self {
            let children = RefCell::new(vec![]);
            Self { parent, children }
        }
    }

    /// 次の遷移先候補
    struct Candidate<S, A>
    where
        S: State<A>,
        A: Action,
    {
        /// 変形操作
        action: A,
        /// 親ノード
        parent: Rc<Node<S, A>>,
        /// スコア
        score: i32,
        /// ハッシュ
        hash: u64,
    }

    impl<S, A> Candidate<S, A>
    where
        S: State<A>,
        A: Action,
    {
        fn new(action: A, parent: Rc<Node<S, A>>, score: i32, hash: u64) -> Self {
            Self {
                action,
                parent,
                score,
                hash,
            }
        }
    }

    /// 変形操作の生成器
    pub trait ActionGenerator<S, A> {
        /// ある状態に対して有効な変形操作を列挙し、stackに詰める
        fn enumerate(&self, state: &mut S, stack: &mut Vec<(A, i32, u64)>);
    }

    /// ビーム幅の管理
    pub trait WidthManager {
        /// 現在のターン数と経過時間からビーム幅を算出する
        fn beam_width(&self, turn: usize, elapsed: Duration) -> usize;
    }

    /// ビーム幅固定戦略
    pub struct FixedWidthManager {
        width: usize,
    }

    impl FixedWidthManager {
        pub fn new(width: usize) -> Self {
            Self { width }
        }
    }

    impl WidthManager for FixedWidthManager {
        fn beam_width(&self, _turn: usize, _elapsed: Duration) -> usize {
            self.width
        }
    }

    /// ビームサーチ
    pub struct BeamSearch<S, A, G, W>
    where
        S: State<A>,
        A: Action,
        G: ActionGenerator<S, A>,
        W: WidthManager,
    {
        /// 最大ターン数
        max_turn: usize,
        /// ビーム幅
        width_manager: W,
        /// 変形操作の生成を行うジェネレータ
        generator: G,
        _phantom_state: PhantomData<S>,
        _phantom_action: PhantomData<A>,
    }

    impl<S, A, G, W> BeamSearch<S, A, G, W>
    where
        S: State<A>,
        A: Action + Clone,
        G: ActionGenerator<S, A>,
        W: WidthManager,
    {
        pub fn new(max_turn: usize, width_manager: W, generator: G) -> Self {
            Self {
                max_turn,
                width_manager,
                generator,
                _phantom_state: PhantomData,
                _phantom_action: PhantomData,
            }
        }

        /// ビームサーチを実行し、最終的な操作列を得る
        pub fn run(&self, state: S) -> Vec<A> {
            let since = Instant::now();
            let mut tree = Tree::new(state, &self.generator);
            let mut current_queue = vec![tree.head.clone()];
            let mut beam_queues = (0..(self.max_turn + 1)).map(|_| vec![]).collect_vec();
            let mut hash_set = FxHashSet::default();

            for turn in 0..self.max_turn {
                let elapsed = Instant::now() - since;

                // ビームを1段階進めて遷移先候補を得る
                tree.dfs(&mut beam_queues, turn, true);
                current_queue.clear();

                if turn + 1 == self.max_turn {
                    // 最終ターンなのでcandidatesを作らない
                    break;
                }

                // Vecの中身を消費するために空のVecとswapする
                let mut candidates = vec![];
                std::mem::swap(&mut candidates, &mut beam_queues[turn + 1]);

                // 上位beam_width件だけ残す
                let beam_width = self.width_manager.beam_width(turn, elapsed);
                if candidates.len() > beam_width {
                    selection::select_nth_unstable_by_key(&mut candidates, beam_width - 1, |c| {
                        Reverse(c.score)
                    });
                    candidates.truncate(beam_width);
                }

                // 残った候補をビームサーチ木に追加する
                hash_set.clear();
                for candidate in candidates {
                    if !hash_set.insert(candidate.hash) {
                        continue;
                    }

                    let child = Node::new(Some(candidate.parent.clone()));
                    let child_ptr = Rc::new(child);
                    let children = &mut candidate.parent.children.borrow_mut();
                    children.push((candidate.action, Rc::downgrade(&child_ptr)));
                    current_queue.push(child_ptr);
                }
            }

            let last_queue = beam_queues.pop().unwrap();
            let best_candidate = last_queue
                .into_iter()
                .max_by_key(|state| state.score)
                .unwrap();

            // 復元
            let mut actions = vec![best_candidate.action.clone()];
            actions.reserve(self.max_turn);
            let mut parent = best_candidate.parent;
            while let Some(p) = parent.parent.clone() {
                {
                    let children = &mut p.children.borrow_mut();
                    let (action, _) = children
                        .iter()
                        .find(|(_, child)| child.upgrade().is_some())
                        .unwrap();
                    actions.push(action.clone());
                }
                parent = p;
            }

            actions.reverse();
            actions
        }
    }

    /// ビームサーチ木
    struct Tree<'a, S, A, G>
    where
        S: State<A>,
        A: Action,
        G: ActionGenerator<S, A>,
    {
        state: S,
        head: Rc<Node<S, A>>,
        generator: &'a G,
        stack: Vec<(A, i32, u64)>,
    }

    impl<'a, S, A, G> Tree<'a, S, A, G>
    where
        S: State<A>,
        A: Action,
        G: ActionGenerator<S, A>,
    {
        fn new(state: S, generator: &'a G) -> Self {
            let head = Rc::new(Node::new(None));
            let stack = vec![];
            Self {
                state,
                head,
                generator,
                stack,
            }
        }

        /// ビームサーチ木に対してDFSを行い、
        /// 深さがtarget_turnのノードから次の遷移先候補を列挙する
        fn dfs(
            &mut self,
            beam_queues: &mut Vec<Vec<Candidate<S, A>>>,
            target_turn: usize,
            single_path: bool,
        ) {
            if self.state.turn() == target_turn {
                self.generator.enumerate(&mut self.state, &mut self.stack);

                while let Some((action, next_score, next_hash)) = self.stack.pop() {
                    let next_turn = self.state.turn() + action.get_consumed_turn();
                    if next_turn < beam_queues.len() {
                        let candidate =
                            Candidate::new(action, self.head.clone(), next_score, next_hash);
                        beam_queues[next_turn].push(candidate);
                    }
                }
            } else {
                let node = self.head.clone();

                let next_single_path = {
                    let children = &mut node.children.borrow_mut();
                    children.retain(|(_, child)| child.upgrade().is_some());
                    if children.is_empty() {
                        false
                    } else {
                        let next_is_single_path = single_path && (children.len() == 1);
                        for (action, child) in children.iter_mut() {
                            let next_turn = self.state.turn() + 1;
                            if next_turn > target_turn {
                                continue;
                            }

                            self.head = child.upgrade().unwrap();
                            self.state.apply(action);

                            self.dfs(beam_queues, target_turn, next_is_single_path);

                            if !next_is_single_path {
                                self.state.revert(action);
                            }
                        }
                        next_is_single_path
                    }
                };

                if !next_single_path {
                    self.head = node;
                }
            }
        }
    }

    /// select_nth_unstable()をRust 1.49以前でも使えるようにするモジュール
    /// 言語アップデートが行われれば不要になるはず
    #[allow(
        dead_code,
        clippy::ptr_offset_with_cast,
        clippy::identity_op,
        clippy::assign_op_pattern,
        clippy::comparison_chain
    )]
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
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum Square {
    Player,
    Coin,
    Trap,
    Wall,
    Empty,
}

// terry_u16さんに感謝します
mod grid {
    #[derive(Debug, Clone, Copy)]
    pub struct Coordinate {
        pub index: usize,
    }

    impl Coordinate {
        pub fn new(row: usize, col: usize) -> Self {
            Self {
                index: row * 50 + col,
            }
        }

        pub const fn row(&self) -> usize {
            self.index / 50
        }

        pub const fn col(&self) -> usize {
            self.index % 50
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct CoordinateDiff {
        pub diff: usize,
    }

    impl std::ops::Add<CoordinateDiff> for Coordinate {
        type Output = Coordinate;

        fn add(self, rhs: CoordinateDiff) -> Self::Output {
            Coordinate {
                index: self.index + rhs.diff,
            }
        }
    }

    pub const ADJACENTS: [CoordinateDiff; 4] = [
        CoordinateDiff { diff: !0 },
        CoordinateDiff { diff: !49 },
        CoordinateDiff { diff: 1 },
        CoordinateDiff { diff: 50 },
    ];

    pub const DIRECTIONS: [char; 4] = ['L', 'U', 'R', 'D'];

    #[derive(Debug, Clone)]
    pub struct Map2d<T> {
        pub width: usize,
        pub height: usize,
        map: Vec<T>,
    }

    impl<T> Map2d<T> {
        pub fn new(map: Vec<T>, width: usize, height: usize) -> Self {
            Self { width, height, map }
        }
    }

    impl<T> std::ops::Index<Coordinate> for Map2d<T> {
        type Output = T;

        #[inline]
        fn index(&self, coordinate: Coordinate) -> &Self::Output {
            &self.map[coordinate.index]
        }
    }

    impl<T> std::ops::IndexMut<Coordinate> for Map2d<T> {
        #[inline]
        fn index_mut(&mut self, coordinate: Coordinate) -> &mut Self::Output {
            &mut self.map[coordinate.index]
        }
    }

    impl<T> std::ops::Index<&Coordinate> for Map2d<T> {
        type Output = T;

        #[inline]
        fn index(&self, coordinate: &Coordinate) -> &Self::Output {
            &self.map[coordinate.row() * self.width + coordinate.col()]
        }
    }

    impl<T> std::ops::IndexMut<&Coordinate> for Map2d<T> {
        #[inline]
        fn index_mut(&mut self, coordinate: &Coordinate) -> &mut Self::Output {
            &mut self.map[coordinate.row() * self.width + coordinate.col()]
        }
    }

    impl<T> std::ops::Index<usize> for Map2d<T> {
        type Output = [T];

        #[inline]
        fn index(&self, row: usize) -> &Self::Output {
            let begin = row * self.width;
            let end = begin + self.width;
            &self.map[begin..end]
        }
    }

    impl<T> std::ops::IndexMut<usize> for Map2d<T> {
        #[inline]
        fn index_mut(&mut self, row: usize) -> &mut Self::Output {
            let begin = row * self.width;
            let end = begin + self.width;
            &mut self.map[begin..end]
        }
    }
}

#[allow(dead_code)]
struct Input {
    n: usize,
    k: usize,
    h: usize,
    w: usize,
    t: usize,
    maps: Vec<Map2d<Square>>,
    player_hashes: Vec<Map2d<u64>>,
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
    let maps = {
        let mut ms = vec![];
        for map in maps {
            let mut m = vec![];
            for row in map {
                for ch in row {
                    let square = match ch {
                        '@' => Square::Player,
                        'o' => Square::Coin,
                        'x' => Square::Trap,
                        '#' => Square::Wall,
                        _ => unreachable!("マップの入力が不正です"),
                    };
                    m.push(square);
                }
            }
            ms.push(Map2d::new(m, w, h));
        }
        ms
    };
    let mut player_hashes = vec![Map2d::new(vec![0_u64; w * h], w, h); k];
    let mut rng = rand_pcg::Pcg64Mcg::new(0);
    for player_hash in player_hashes.iter_mut() {
        for row in 0..h {
            for col in 0..w {
                player_hash[Coordinate::new(row, col)] = rng.gen();
            }
        }
    }
    Input {
        n,
        k,
        h,
        w,
        t,
        player_hashes,
        maps,
    }
}

fn find_player_position(input: &Input, map: &Map2d<Square>) -> Coordinate {
    for row in 0..input.h {
        for col in 0..input.w {
            let c = Coordinate::new(row, col);
            if map[c] == Square::Player {
                return c;
            }
        }
    }
    unreachable!("map上にplayerが見つかりませんでした")
}

fn write_output(actions: Vec<Action>, map_ids: Vec<usize>) {
    let commands = actions
        .iter()
        .map(|a| DIRECTIONS[a.dir])
        .collect::<String>();
    println!("{}", map_ids.iter().join(" "));
    println!("{}", commands);
}
