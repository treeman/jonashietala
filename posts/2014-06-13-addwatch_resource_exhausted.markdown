---
layout: post
title: "addwatch: resource exhausted"
tags: Slackware
---

While tampering with Hakyll and running `site preview` I stumbled upon this error message:

```
site: addWatch: resource exhausted (No space left on device)
```

At first I tried to clear `/tmp` but, so clearly the device did have some space left. After a bit of googling I found [a solution](http://peter-butkovic.blogspot.se/2013/08/tail-inotify-resources-exhausted.html) which managed to solve my problem.

```
# for foo in /proc/*/fd/*; do readlink -f $foo; done | grep inotify | sort | uniq -c | sort -nr
      2 /proc/12871/fd/anon_inode:inotify
      1 /proc/673/fd/anon_inode:inotify
      1 /proc/601/fd/anon_inode:inotify
      1 /proc/587/fd/anon_inode:inotify
      1 /proc/27695/fd/anon_inode:inotify
      1 /proc/176/fd/anon_inode:inotify
      1 /proc/12879/fd/anon_inode:inotify
      1 /proc/12840/fd/anon_inode:inotify
      1 /proc/12806/fd/anon_inode:inotify
      1 /proc/12772/fd/anon_inode:inotify
      1 /proc/12771/fd/anon_inode:inotify
      1 /proc/12770/fd/anon_inode:inotify
      1 /proc/12769/fd/anon_inode:inotify
      1 /proc/12768/fd/anon_inode:inotify
      1 /proc/12767/fd/anon_inode:inotify
      1 /proc/12754/fd/anon_inode:inotify
```

One can examine the pid

```
ps ax
ps ax | grep 12871
...
```

And in the end I found out that spotify was the culprit. I killed the spotify processes with a simple `kill 12871` and the problem had gone away.  One can check if spotify is running with `ps ax | grep spotify`.

