---
title: "Trying out Void Linux"
tags: "Void Linux"
---

# Mounted like so:

Filesystem      Size  Used Avail Use% Mounted on
devtmpfs         16G     0   16G   0% /dev
tmpfs            16G     0   16G   0% /dev/shm
tmpfs            16G  1.4M   16G   1% /run
/dev/nvme1n1p2   98G   21G   72G  23% /
cgroup           16G     0   16G   0% /sys/fs/cgroup
/dev/nvme1n1p3  356G   17G  321G   5% /home
tmpfs            16G  8.0K   16G   1% /tmp
/dev/sda1       916G  641G  229G  74% /store

# Init

```{.fish}
ln -s /etc/sv/sshd/ .
ln -s /etc/sv/wicd/ .
```

# xbps-install

among many other things:

* xorg
* cabal
* pkg-config
* dzen2
* xclip
* fzf
* zip
* zlib-devel
* git
* firefox
* chromium
* libreoffice
* neovim
* wicd
* conky
* nitrogen
* make
* alsa-utils

* kitty
* kitty-terminfo
* rxvt-unicode
* rxvt-unicode-terminfo

For xmonad:

* libX11-devel
* libXScrnSaver-devel
* libXrandr-devel
* libXinerama-devel

# cabal

```
mkdir ~/.ghc/x86_64-linux-8.8.3/
ln -s ~/.cabal/store/ghc-8.8.3/package.db ~/.ghc/x86_64-linux-8.8.3/package.conf.d
```

Install locally

* xmonad
* xmonad-contrib
* MissingH
* Hakyll

# cpan

Install locally

# Hakyll


# Spotify


# Sound?


# nvim


# Fonts

See fontconfig

Move TTF fonts into `/usr/share/fonts/TTF` and in there do:

```
mkfontscale
mkfontdir
fc-cache -fv
```


[installation]: https://wiki.voidlinux.org/Installation
[post-installation]: https://wiki.voidlinux.org/Post_Installation
[spotify]: https://wiki.voidlinux.org/Spotify
[sound]: /blog/2019/03/16/default_audio_card_in_linux/

