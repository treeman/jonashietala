---
layout: post
title: "My first rust Contribution"
tags: IDA Summer of Code, Rust
---

The problem with open-source for most people isn't writing code, but it's all the other things.

> How shall I push my changes? How do I handle [git][]? What should I do?

I was the same and I actually dreaded [my awesome summer job][isoc], just a little bit, because now I'm supposed to contribute and preferrably a non-trivial amount. Although I've been programming several years, I've never contributed a large open-source project. Or a small one for that matter.

But this has now officially changed. 3 days ago [my first pull request][15667] got merged into [rust][]! Here are some useful steps and resources which might be useful for someone in my shoes:

I assume you're going to contribute to [rust][], but the essence could be generalized for other projects as well.


Build
=====

Firstly we should try to [build rust][]. There are some useful things about building inside the root `Makefile` and in the [test suite notes][].

For making the documentation:

```shell
make docs NO_REBUILD=1
```

And making other things:

```shell
make -j<num-cores> NO_REBUILD=1 NO_BENCH=1 CFG_DISABLE_VALGRIND=1
```

The process can be very slow though, especially if you do `make clean` (try not to!).


Git workflow
============

Before we start hacking it's good to have an idea of the git workflow we're going to use.

Firstly, clone `git@github.com:rust-lang/rust.git` and push that repo into your github account. I have that as my `origin`. Then create an `upstream` branch:

```shell
git remote add upstream https://github.com/rust-lang/rust
```

For me it looks like this:

```shell
$ g remote -v
origin      git@github.com:treeman/rust.git (fetch)
origin      git@github.com:treeman/rust.git (push)
upstream    https://github.com/rust-lang/rust (fetch)
upstream    https://github.com/rust-lang/rust (push)
```

When we want to start working on something new, always create a new branch:

```shell
$ git checkout master -b mybranch
```

While we're working we need to update from `upstream`, to get new changes.

```shell
$ git checkout mybranch
$ git fetch upstream
$ git rebase upstream/master
```

When done, push locally to github:

```
$ git push origin mybranch
```

To file a pull request we can use github's interface. Just be sure to target the `master` integration branch.

If we want to make some changes to our pull request, simply make the changes in `mybranch` and push towards your github profile.

If you have a lot of commits in your pull request, or if they aren't very descriptive, you may be asked to squash your commits. Sounds scary, but it's fairly straightforward:

1. `git log` and check how many commits you have (or check via github).
2. `git rebase -i HEAD~2` will rebase the 2 latest commits.

When satisfied:

```
$ git push origin mybranch -f
```

And that's it! Make sure to switch branch when you're done. I accidentally pushed another commit on top of my already reviewed, and accepted, pull request. Quite embarrassing but I'll live =)


Actual work
===========

With that taken care of, we can finally do some work. But [what to do][]? Here are some tips:

1. Document the [library][].

    At the time of my writing, the library lacks a lot of documentation, and that's what I'm doing. And don't be scared, it's not as dry as it's sounds. My contributions are basically just adding code examples.

2. Write unit tests.

3. Proofread the [tutorial][].

    I did this, but the tutorial is currently getting a complete rewrite, so I'm not sure how useful that was.

4. Find and fix some [bugs][].

    rust organizes everything as issues. Feature request, updates and bugs. If you look for them, you might find an [easy bug][15780] to work on. Which is what [I did][15785].

And you can always simply start hacking on something interesting. Or find and fix a bug yourself.


[git]: http://git-scm.com/ "git"
[isoc]: /blog/2014/06/11/isoc/ "IDA Summer of Code"
[15667]: https://github.com/rust-lang/rust/pull/15667 "issue #15667"

[15780]: https://github.com/rust-lang/rust/issues/15780 "issue #15780"
[15785]: https://github.com/rust-lang/rust/pull/15785 "issue #15785"

[build rust]: https://github.com/rust-lang/rust/wiki/Note-getting-started-developing-Rust "Build rust"
[test suite notes]: https://github.com/rust-lang/rust/wiki/Note-testsuite "Test suite"
[rust]: http://www.rust-lang.org/ "rust"

[what to do]: https://github.com/rust-lang/rust/wiki/Note-guide-for-new-contributors "Note guide for new contributors"

[library]: http://doc.rust-lang.org/std/ "rust std library"

[bugs]: https://github.com/rust-lang/rust/issues?direction=desc&sort=created&state=open "rust Issues"
[tutorial]: http://doc.rust-lang.org/tutorial.html "rust tutorial"

