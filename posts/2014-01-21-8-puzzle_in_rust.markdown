---
layout: post
title: "8-puzzle in rust"
category: Rust
tags: Rust
---

I think [rust][] is one of the most interesting upcoming programming languages out there. So I wrote a solution to the 8-puzzle (see the [15-puzzle][15p]) using A\*. It also has a breadth first search for solutions on a specific distance away from the goal.

The solution is not by any means a great one and there are parts I'm a bit annoyed with, but that's mostly because of my inexperience with rust.

Rust was surprisingly easy to use and I like the strong typing and the pattern matching. One feature I found very useful was to do `println!("{:?}", x);` and be able to print any variable, composite or not.

I'm not friends with the different storage containers and especially the moving of values. Often I called a function with a value, but then I couldn't reuse the value in another, so I ended up using `clone()` a bunch. I'm probably abusing it and there's bound to be a better way to do things.

I got a little stumped on the standard library and spent some time perusing the documentation. It was very light on examples, which I find is the by far most useful thing to have in documentation, and I couldn't even find some of the things. But it all worked out with a bit of determination.

I should mention that I'm using rust master, not a stable release, and I'm well aware of rust being in development at the moment. Still things worked out fine and I quite enjoyed dabbling with rust. I'll write some more in it I think.

This is the code, working on `rustc 0.10-pre (5512fb4 2014-01-19 05:56:35 -0800)`:

[rust]: http://www.rust-lang.org/
[15p]: http://en.wikipedia.org/wiki/15_puzzle

``` {.rust}
/**
 * A solution to the 8-tiles puzzle, implemented in rust
 * as a learning experience.
 */

extern mod extra;
use std::num::abs;
use std::hashmap::HashMap;
use std::hashmap::HashSet;
use extra::priority_queue::PriorityQueue;
use extra::container::Deque;
use extra::ringbuf::RingBuf;

// Use manhattan distance as heuristic for A*.
// Sum of distance in x and distance in y for all tiles.
fn manhattan(start: &[int], dest: &[int]) -> uint {
    let mut dist: uint = 0;
    for i in range(0, start.len()) {
        for j in range(0, dest.len()) {
            if start[i] == dest[j] {
                let x1 = i as int % 3;
                let y1 = i as int / 3;
                let x2 = j as int % 3;
                let y2 = j as int / 3;
                dist += abs(x2 - x1) as uint;
                dist += abs(y2 - y1) as uint;
            }
        }
    }
    // Avoid double counting so we don't overestimate.
    dist / 2
}

fn to_hash(state: &[int]) -> ~str {
    let mut hash = ~"";
    for x in state.iter() {
        hash.push_str(x.to_str());
    }
    hash
}

#[allow(dead_code)]
fn print_state(state: &[int]) {
    for i in range(0, 3) {
        for j in range(0, 3) {
            print!("{} ", state[3 * i + j]);
        }
        println!("");
    }
}

#[allow(dead_code)]
fn print(state: &[int], goal: &[int]) {
    for i in range(0, 3) {
        for j in range(0, 3) {
            print!("{} ", state[3 * i + j]);
        }

        if i == 1 { print!("-> "); }
        else { print!("   "); }

        for j in range(0, 3) {
            print!("{} ", goal[3 * i + j]);
        }
        println!("");
    }
}

// A* uses g: cost so far, h: manhattan distance to goal, f: g + h
struct State {
    state: ~[int],
    g: uint,
    h: uint,
    zero: uint,
}

impl State {
    fn new(state: ~[int], goal: ~[int], cost: uint, zero: uint) -> State {
        let h = manhattan(state, goal);
        State { state: state, g: cost, h: h, zero: zero }
    }

    // Ignore h and f.
    fn new_simple(state: ~[int], cost: uint, zero: uint) -> State {
        State { state: state, g: cost, h: 0u, zero: zero }
    }

    fn f(&self) -> uint { self.g + self.h }

    #[allow(dead_code)]
    fn print(&self) {
        println!("g: {} h: {} f: {}", self.g, self.h, self.f());
        print_state(self.state);
    }
}

impl Ord for State {
    fn lt(&self, s2: &State) -> bool {
        self.f() > s2.f()
    }
}

static dx: [uint, ..4] = [0, 1, 0, -1];
static dy: [uint, ..4] = [1, 0, -1, 0];

// Calculate the minimum distance from a state to goal with A*
fn dist(state: ~[int], goal: ~[int]) -> uint {
    let mut dist = HashMap::new();
    let mut q = PriorityQueue::new();

    dist.insert(to_hash(state), 0u);
    let zero = match state.position_elem(&0) {
        Some(x) => x,
        None => fail!("0 missing in state!"),
    };
    q.push(State::new(state, goal.clone(), 0u, zero));

    loop {
        // Ugly, but don't know how...
        let (state, cost, pos) = match q.maybe_top() {
            Some(e) => { /*e.print();*/ (e.state.clone(), e.g, e.zero) },
            None => break,
        };
        q.pop();

        if state == goal { return cost; }

        let x = pos % 3;
        let y = pos / 3;

        for d in range(0, 4) {
            let nx = x + dx[d];
            let ny = y + dy[d];

            if nx < 0 || nx >= 3 || ny < 0 || ny >= 3 { continue; }

            let npos = nx + 3 * ny;

            let mut next_state = state.clone();
            next_state[pos] = next_state[npos];
            next_state[npos] = 0;

            let next_hash = to_hash(next_state);
            let next_cost = cost + 1;
            let s = State::new(next_state, goal.clone(), next_cost, npos);

            if s.state == goal { return next_cost; }

            if !dist.contains_key(&next_hash) || s.f() < *dist.get(&next_hash) {
                dist.insert(next_hash, next_cost);
                q.push(s);
            }
        }
    }
    0u // Should never happen!
}

// Calculate the number of solutions with optimal solution length = target_dist.
// Use breadth first.
fn num_dist_from(goal: ~[int], target_dist: uint) -> uint {
    let mut count = 0u;
    let mut seen = HashSet::new();
    let mut q = RingBuf::new();

    seen.insert(to_hash(goal));
    let zero = match goal.position_elem(&0) {
        Some(x) => x,
        None => fail!("0 missing in state!"),
    };
    q.push_back(State::new_simple(goal.clone(), 0u, zero));

    loop {
        // Ugly, but don't know how...
        let (state, dist, pos) = match q.front() {
            Some(e) => { /*e.print();*/ (e.state.clone(), e.g, e.zero) },
            None => break,
        };
        q.pop_front();

        if dist > target_dist { break; }
        else if dist == target_dist { count += 1; }

        let x = pos % 3;
        let y = pos / 3;

        for d in range(0, 4) {
            let nx = x + dx[d];
            let ny = y + dy[d];

            if nx < 0 || nx >= 3 || ny < 0 || ny >= 3 { continue; }

            let npos = nx + 3 * ny;
            let mut next_state = state.clone();
            next_state[pos] = next_state[npos];
            next_state[npos] = 0;

            let next_hash = to_hash(next_state);
            let next_dist = dist + 1;
            let s = State::new_simple(next_state, next_dist, npos);

            if !seen.contains(&next_hash) {
                seen.insert(next_hash);
                q.push_back(s);
            }
        }
    }
    count
}

fn main() {
    let state = ~[5, 2, 6, 1, 7, 8, 0, 3, 4];
    let goal = ~[0, 1, 2, 3, 4, 5, 6, 7, 8];

    print(state, goal);
    let sol = dist(state, goal.clone());
    println!("optimal solution found in {} steps", sol);

    let target_dist = sol;
    println!("number of solutions on distance {}: {}", target_dist, num_dist_from(goal.clone(), target_dist));
}
```
