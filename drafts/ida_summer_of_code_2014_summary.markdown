---
layout: post
title: IDA Summer of Code 2014: Summary
tags: IDA Summer of Code
---

Things I've done:

# Documentation

Code examples for, among others:

* HashSet
* HashMap
* TrieMap
* TrieSet
* TreeMap
* TreeSet
* BitvSet
* Bitv
* LRU
* SmallIntMap
* Some related traits
* Vec
* PriorityQueue
* BigInt
* Integer
* String
* Result/Option

Larger and more fun examples for priority queue and rand.

# Stats

```
36 pull requests, 34 merged
12 pull requests directly fixing issues
rest either documentation or cleanup (ex: remove build warnings)
6 issues filed

65 commits
6,790 ++ / 2,822 --
Entered top 50 in amount of lines/commits contributions.
```

And random discussions scattered about.

# Triage

Mark and bump bugs. Tried to fix a few larger ones, but failing.
Closed obsolete bugs.

50+ issues?

# Bug fixes

Most very easy.

# RFC

`newtype` keyword:

<https://github.com/rust-lang/rfcs/pull/186>

Might not be a keyword, not "newtype" anyway but maybe some other keyword. Either this or generalized newtype will most likely enter the language at some point. Positive feedback. Postponed until after 1.0.

