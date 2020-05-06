---
title: "ghc 8.8.3 cannot find cabal 3.0.0.0 packages"
tags: Haskell, Xmonad, Void Linux
---

I've been using cabal to manage my Haskell dependencies for years, but when I last updated my system it suddenly stopped working. I installed my dependencies with `cabal install xmonad`, and checked that it's installed under `~/.cabal`:

```
$ ls .cabal/bin/
xmonad@
```

But still when I go to compile my xmonad config file ghc says it cannot find it:


```
Could not find module ‘XMonad’
```

And indeed when I run `ghc-pkg list` it does not list xmonad. That's weird.

Apparently this is [a known issue][cabal-ghc] (including the "blame game" if it's a cabal or a ghc issue, but that doesn't make it any less annoying for us who just want it work). The [ghc issue][ghc-issue] is still open.

```
$ cabal --version
cabal-install version 3.0.0.0
```

```
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.8.3
```

Fortunately there's a solution.

We can manually specify a package db for ghc-pkg so it'll find our cabal modules:

```
$ ghc-pkg --package-db ~/.cabal/store/ghc-8.8.3/package.db list
...
xmonad-0.15
xmonad-contrib-0.16
...
```

And to make it permanent we can symlink the `package.db` file as a ghc config:

```
$ mkdir -p ~/.ghc/x86_64-linux-8.8.3/
$ ln -s ~/.cabal/store/ghc-8.8.3/package.db ~/.ghc/x86_64-linux-8.8.3/package.conf.d
```

The name of the folder `x86_64-linux-8.8.3` may differ, this one worked for me.

And now `ghc-pkg list` should find xmonad and compiling with ghc should find our missing cabal modules.

[cabal-ghc]: https://github.com/haskell/cabal/issues/6262
[ghc-issue]: https://gitlab.haskell.org/ghc/ghc/issues/17341
