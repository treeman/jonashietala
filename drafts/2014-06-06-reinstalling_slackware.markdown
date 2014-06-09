---
layout: post
title: Reinstalling Slackware
category: Slackware
tags: Slackware
---

Basic steps
===========

Make slackware usb loader
-------------------------

See `README_USB.TXT`

Create

`dd if=usbboot.img of=/dev/sdX bs=1M`

Be sure /dev/sdX is the usb, dd will wipe everything!

Boot with bios f2, f10 (?)
Reset later

And, keyboard ENTER does not work!!! Just wait =)


Make partitions
---------------

Make partitions <http://slackbook.org/html/installation-partitioning.html>

`fdisk /dev/sda`

Print partitions `p`

Current setup:

```
tmpfs       4G      swap
/dev/sda2   50G     /
/dev/sda3   50G     /usr/local
/dev/sda5   rest    /home
```

Make sure that swap is of type Linux Swap, change with `t`


Setup
-----

type setup

Install from FTP/HTTP server

1. ftp://ftp.slackware.com
2. /pub/slackware/slackware64-14.1/slackware64

For example.

Don't pick KDE or Games. Use terse installation.

Config
------

`adduser`

Set zsh as basic shell. Set for root in `/etc/passwd`

Update name in HOSTNAME


Install wicd, get online
------------------------

In `/extra/wicd`

```
installpkg ...
chmod +x /etc/rc.d/rc.wicd
/etc/rc.d/rc/wicd start
wicd-curses
```

Custom Kernel
-------------

<http://alien.slackbook.org/dokuwiki/doku.php?id=linux:kernelbuilding>

Fetch latest kernel

```
wget ...
cd /usr/src
rm linux
ln -s linux-... linux
```

Use slackware custom as base

```
wget ftp://ftp.slackware.com/pub/slackware/slackware64-14.1/source/k/config-x86_64/config-generic-3.10.17.x64
make oldconfig
make menuconfig
```

Make sure to select processor type, preemptive low latency desktop. Remove nvidia/riba.

```
make bzImage modules
make modules_install
```

Update lilo conf, with fallback


Fix X
-----

```
ln -s dotfiles/.workspace .
ln -s /etc/X11/xorg.conf.d/xorg.conf .workspace/xorg.conf
```

NVIDIA drivers 337.25

Install firefox
---------------

Download latest

```
tar xf ...
mv firefox /usr/local/lib64/
cd /usr/bin
rm firefox
ln -s /usr/local/lib64/firefox/firefox .
```

Get `libflashplayer.so` into `/usr/local/lib64/firefox/browser/plugins`

Restore bookmark backup from `.mozilla/firefox/X.default/bookmarkbackups`


Install xmonad
--------------

* Install ghc
* Install hscolour from slackbuilds (for haskell-platform warnings)
* Install haskell-platform

As user:

```
cabal update
cabal install cabal-install
cabal install xmonad
cabal install xmonad-contrib
ln -s dotfiles/.xmonad ~
```

Install `conky` with dependencies from slackbuilds. Build conky with lua support.

Install `dzen2` by cloning from github. In config.mk, choose option 8 (XPM, XFT, Xinerama). This is actually not workeing as `

Get `.backgrounds` and `.icons`.

Could not get nitrogen to build properly (I stole it from my other installation...!!)

Fix borders on firefox etc `lxappearance`, get from slackbuilds.


Install fonts
-------------

Copy ttf fonts to `/usr/share/fonts/TTF`, in that dir run

```
mkfontscale
mkfontdir
fc-cache -fv
```


Install perl things
-------------------

As root:

```
cpan install cpan
cpan install App::Ack
cpan install Modern::Perl
cpan install DateTime
cpan install Data::ICal
cpan install LWP
...
```

Install minecraft
-----------------

Get OpenAL from slackbuilds.

Get jdk <http://docs.slackware.com/howtos:software:java>

Launch


Install Hakyll
--------------

```
cabal install hakyll
cabal install MissingH
```

For upload scripts fetch `python-magic` slackbuilds.


vim
---

For xterm copying and support for more plugins. Get vim source.

```
hg clone https://vim.googlecode.com/hg/ vim
cd vim
./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp \
            --enable-pythoninterp \
            --enable-perlinterp \
            --enable-luainterp \
            --prefix=/usr/local \
            --enable-gui \
            --enable-hangulinput \
            --with-x
make
make install
```

Terminal
--------

Fetch urxvt from slackbuilds. Make `.Xresources`.

Color schemes with ls listings will be ugly, so copy `DIR_COLORS` to `.dir_colors`

``` {.C}
STICKY_OTHER_WRITABLE 35;40
OTHER_WRITABLE 34;40
```

Set in .zshrc.

Fix git diff colors `git config --global core.pager "less -r"`

Also korean signs.


Prettier fonts
--------

/usr/bin/startx

`defaultserverargs="-dpi 96"`


Groovebasin
-----------

Need `libgroove`.  Which also needs `speex` from slackbuilds apart from the clear dependencies. Run groovebasin as user.


SFML
----

Get source. `cmake` and install.


Multilib
--------

<http://alien.slackbook.org/dokuwiki/doku.php?id=slackware:multilib> Also add blacklist.

When installing 32bit run `. /etc/profile.d/32dev.sh`


Skype
-----

Install multilib. Install skype from slackbuilds, use 32bit mode.


Random slackbuilds
------------------

```
s3cmd, scrot, mirage, urxvt-unicode, rtorrent
```


Better latex
------------

Install `texlive` from slackbuilds. Remove `tetex` first.


Rest
----

0. Prettier fonts!
6. Fix dual-screen/single-screen switching
7. If sound ok, spotify
7. Faster startup


Misc
----

1. mtpaint
3. spotify?
5. setup ticker (bork?)
9. korean input style
1. Schedule left screen
1. todo left screen


Change config
-------------

1. irssi, notifications and remove flashing
1. left screen
1. Groovebasin


Games etc
---------

1. Install wine
1. Install csgo
1. Install diablo
1. Install hearthstone
1. Setup treecraft autogen

Final
-----

1. Blog about this!

