---
layout: post
title: Dijkstra's Algorithm
tags: IDA Summer of Code, rust
---

For [rust][], I'm updating the documentation for the standard library and specifically with the collections. For the priority queue I had the idea to use [Dijkstra's algorithm][dijkstra] as a fun example. That idea was [well received][pr] and that example is now [live][doc].

At first I wanted to use [A\*][] to solve [the eight puzzle][15-puzzle], which [I've done before][eight-post], but that example became far too big.

Here's the example code currently up in [docs][doc]:

Using `rustc 0.12.0-pre (ef352faea84fa16616b773bd9aa5020d7c76bff0 2014-07-18 21:46:32 +0000`)

```{.rust}
use std::collections::PriorityQueue;
use std::uint;

#[deriving(Eq, PartialEq)]
struct State {
    cost: uint,
    position: uint
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl Ord for State {
    fn cmp(&self, other: &State) -> Ordering {
        // Notice that the we flip the ordering here
        other.cost.cmp(&self.cost)
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for State {
    fn partial_cmp(&self, other: &State) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// Each node is represented as an `uint`, for a shorter implementation.
struct Edge {
    node: uint,
    cost: uint
}

// Dijkstra's shortest path algorithm.

// Start at `start` and use `dist` to track the current shortest distance
// to each node. This implementation isn't memory efficient as it may leave duplicate
// nodes in the queue. It also uses `uint::MAX` as a sentinel value,
// for a simpler implementation.
fn shortest_path(adj_list: &Vec<Vec<Edge>>, start: uint, goal: uint) -> uint {
    // dist[node] = current shortest distance from `start` to `node`
    let mut dist = Vec::from_elem(adj_list.len(), uint::MAX);

    let mut pq = PriorityQueue::new();

    // We're at `start`, with a zero cost
    *dist.get_mut(start) = 0u;
    pq.push(State { cost: 0u, position: start });

    // Examine the frontier with lower cost nodes first (min-heap)
    loop {
        let State { cost, position } = match pq.pop() {
            None => break, // empty
            Some(s) => s
        };

        // Alternatively we could have continued to find all shortest paths
        if position == goal { return cost }

        // Important as we may have already found a better way
        if cost > dist[position] { continue }

        // For each node we can reach, see if we can find a way with
        // a lower cost going through this node
        for edge in adj_list[position].iter() {
            let next = State { cost: cost + edge.cost, position: edge.node };

            // If so, add it to the frontier and continue
            if next.cost < dist[next.position] {
                pq.push(next);
                // Relaxation, we have now found a better way
                *dist.get_mut(next.position) = next.cost;
            }
        }
    }

    // Goal not reachable
    uint::MAX
}

fn main() {
    // This is the directed graph we're going to use.
    // The node numbers correspond to the different states,
    // and the edge weights symbolises the cost of moving
    // from one node to another.
    // Note that the edges are one-way.
    //
    //                  7
    //          +-----------------+
    //          |                 |
    //          v   1        2    |
    //          0 -----> 1 -----> 3 ---> 4
    //          |        ^        ^      ^
    //          |        | 1      |      |
    //          |        |        | 3    | 1
    //          +------> 2 -------+      |
    //           10      |               |
    //                   +---------------+
    //
    // The graph is represented as an adjecency list where each index,
    // corresponding to a node value, has a list of outgoing edges.
    // Chosen for it's efficiency.
    let graph = vec![
        // Node 0
        vec![Edge { node: 2, cost: 10 },
             Edge { node: 1, cost: 1 }],
        // Node 1
        vec![Edge { node: 3, cost: 2 }],
        // Node 2
        vec![Edge { node: 1, cost: 1 },
             Edge { node: 3, cost: 3 },
             Edge { node: 4, cost: 1 }],
        // Node 3
        vec![Edge { node: 0, cost: 7 },
             Edge { node: 4, cost: 2 }],
        // Node 4
        vec![]];

    assert_eq!(shortest_path(&graph, 0, 1), 1);
    assert_eq!(shortest_path(&graph, 0, 3), 3);
    assert_eq!(shortest_path(&graph, 3, 0), 7);
    assert_eq!(shortest_path(&graph, 0, 4), 5);
    assert_eq!(shortest_path(&graph, 4, 0), uint::MAX);
}
```

[rust]: http://rust-lang.org/
[pr]: https://github.com/rust-lang/rust/pull/15857 "#15857"
[dijkstra]: http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm "Dijkstra's algorithm"
[doc]: http://doc.rust-lang.org/std/collections/priority_queue/index.html "rust priority_queue doc"
[15-puzzle]: http://en.wikipedia.org/wiki/15_puzzle
[eight-post]: /blog/2014/01/21/8-puzzle_in_rust/ "rust code for 8-puzzle"
[A\*]: http://en.wikipedia.org/wiki/A*_search_algorithm "A* search algorithm"

