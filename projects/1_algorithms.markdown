---
title: Competitive programming algorithms
link: https://github.com/treeman/algorithms
year: 2014
---

This is a collection of algorithms used in competitive programming. There are also slightly hacky commandline submit scripts for [kattis][] and [UVa][]. See my [uHunt profile][uHunt] for a list of solved UVa problems.

```cpp
// Bellman-Ford's algorithm for single source shortest path.
// Return a vector of pair<dist, parent>.
// Take in an adjecency list of type pair<node, weight>.
template<typename T>
vector<pair<T, int>> shortest_path(vector<vector<pair<int, T>>> adj_list, int start);

// Aho-Corasick's string matching algorithm.
// Return a vector of matching positions corresponding to the pattern index.
vector<vector<int>> find(const vector<string> &patterns, string text);
```

[kattis]: https://open.kattis.com/ "kattis"
[UVa]: http://uva.onlinejudge.org/ "UVa"
[uHunt]: https://uhunt.onlinejudge.org/id/115705 "My uHunt prifle"

