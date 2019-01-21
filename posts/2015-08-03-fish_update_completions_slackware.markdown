---
layout: post
title: "fish_update_completions in Slackware 14.1"
tags: Slackware, Fish
---

I've been trying out [fish shell][] lately. A cool feature with fish is that it can automatically generate completions by parsing the installed man pages by running `fish_update_completions`.

Unfortunately this is what I got:

```
Traceback (most recent call last):
  File "/usr/local/share/fish/tools/create_manpage_completions.py", line 963, in <module>
    file_paths.extend(get_paths_from_manpath())
  File "/usr/local/share/fish/tools/create_manpage_completions.py", line 894, in get_paths_from_manpath
    proc = subprocess.Popen(['manpath'], stdout=subprocess.PIPE)
  File "/usr/lib64/python2.7/subprocess.py", line 711, in __init__
    errread, errwrite)
  File "/usr/lib64/python2.7/subprocess.py", line 1308, in _execute_child
    raise child_exception
OSError: [Errno 2] No such file or directory
```

The solution for me was to install `man-db` from Slackbuilds and add `/opt/man-db/bin` and `/opt/man-db/sbin` to PATH to get fish to find the command `manpath`. The dependency for `man-db` is undocumented but it's a known issue.

During the installation I got several strange errors `error: cannot run C compiled programs` which I have no idea why they came up. Eventually by retrial I got it to work.

Curiouly enough fish couldn't parse several of the slackware specific man pages. I manually made completions for `exlodepkg`, `installpkg`, `makepkg`, `removepkg`, `slackpkg` and `upgradepkg` which can be found at <https://github.com/treeman/dotfiles/tree/master/.config/fish/completions>. Of these removepkg was the real motivator as natively it didn't autocomplete installed packages. I added the rest more for the sake of completeness. These are not super great and I'm not sure if anyone else will find it useful. I might consider trying to add them to fish later.

[fish shell]: http://fishshell.com "fish shell"

