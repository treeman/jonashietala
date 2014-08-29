---
layout: post
title: Undo git reset --hard
tags: IDA Summer of Code, Git
---

I purposefully and more or less idiotically executed `git reset --hard <hash>` in hopes of going back a bit. What I didn't realize then is that you throw away all the commits between now until `<hash>`. Not quite what was planned.

After a bit of panic I found [the answer](http://stackoverflow.com/questions/7374069/undo-git-reset-hard/18472148#18472148):

1. Find your hash using `git reflog`.
2. Do `git cherry-pick <hash>` to go back in time.

This saved me a bunch of time, thanks!
