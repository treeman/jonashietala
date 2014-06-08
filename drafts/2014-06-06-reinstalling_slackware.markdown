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

```
tmpfs       2G      swap
/dev/sda2   15G     /
/dev/sda3   50G     /usr
/dev/sda5   rest    /home
```

Current:

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

Get alien minecraft package: <http://www.slackware.com/~alien/slackbuilds/minecraft/>

Get jdk <http://docs.slackware.com/howtos:software:java>


Install Hakyll
--------------

```
cabal install hakyll
cabal install MissingH
```


Rest
----

5. irssi, remove flashing
6. Fix dual-screen/single-screen switching
7. If sound ok, spotify
7. Faster startup

Misc
----

1. SFML
1. lua
2. Hakyll
3. algm
4. conky
5. nextep perl dependencies (cpan)
5. setup ticker
6. mojolicious
7. mtpaint
8. skype
9. korean input style
10. groovebasin
11. xpdf (or other pdf reader)
12. mirage
13. scrot

Games etc
---------

1. Install minecraft, check sound
5. Install multilib slackware
5. Install wine
5. Install csgo
5. Install diablo
5. Install hearthstone
6. Setup treecraft autogen

Final
-----

1. Blog about this!

