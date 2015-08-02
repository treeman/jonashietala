---
layout: post
title: Preventing Firefox from creating Desktop directories
tags: Firefox, Slackware
---

With a fresh firefox installation I found that it kept creating a `~/Desktop` directory. But [I found how to turn it off](http://www.kariliq.nl/misc/firefox-dirs.html).

Edit `~/.config/user-dirs.dirs` to

```
XDG_DESKTOP_DIR="$HOME/"
XDG_DOCUMENTS_DIR="$HOME/"
XDG_DOWNLOAD_DIR="$HOME/"
XDG_MUSIC_DIR="$HOME/"
XDG_PICTURES_DIR="$HOME/"
XDG_PUBLICSHARE_DIR="$HOME/"
XDG_TEMPLATES_DIR="$HOME/"
XDG_VIDEOS_DIR="$HOME/"
```

I had `XDG_DESKTOP_DIR="$HOME/Desktop"` which made a Desktop folder all the time.
