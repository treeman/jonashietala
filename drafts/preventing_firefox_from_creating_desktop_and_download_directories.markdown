---
layout: post
title: Preventing Firefox from creating Desktop and Download directories
tags: Gaming, Ludum Dare
---

<http://www.kariliq.nl/misc/firefox-dirs.html>

Edit `~/.config/user-dirs.dirs` with

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
