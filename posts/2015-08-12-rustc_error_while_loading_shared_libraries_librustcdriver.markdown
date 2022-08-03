---
layout: post
title: "rustc: error while loading shared libraries: librustc_driver"
tags: Rust, Slackware
---

I had installed and used [rust][] already but today I fired it up and received:

```bash
rustc --version
rustc: error while loading shared libraries: librustc_driver-7e44814b.so: cannot open shared object file: No such file or directory
```

I've seen this before... I thought modifying `LD_LIBRARY_PATH` in my shell was enough, but no. According to [the bug report][bug] the problem is with `ldconfig`.

Edit `/etc/ld.so.conf` and add `/usr/local/lib` to it then run `ldconfig` to update the cache. Now everything works again.

```bash
rustc --version
rustc 1.3.0-dev (4b4119d5c 2015-07-29)
```

Not sure why it appeared again though.

[bug]: https://github.com/rust-lang/rust/issues/24677 "The Bug"
[rust]: https://github.com/rust-lang/rust "rust"
