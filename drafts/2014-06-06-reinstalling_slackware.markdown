---
layout: post
title: Reinstalling Slackware
category: Slackware
tags: Slackware
---

Steps
=====

Basic
-----

0. Make slackware usb loader  
    See README_USB.TXT

    `dd if=usbboot.img of=/dev/sdX bs=1M`

    Be sure /dev/sdX is the usb, dd will wipe everything!
0. Reinstall  
    Make partitions

    ```
    /dev/sda2   15G     /
    /dev/sda3   50G     /usr
    /dev/sda5   rest    /home
    tmpfs       2G      swap
    ```
1. Install wicd, get online
1. Update graphics drivers  
    NVIDIA-Linux 319.76  
    /etc/X11/xorg.conf.d/xorg.conf -> .workspace/xorg.conf  
    .workspace -> dotfiles/.workspace  
1. Recompile and update linux kernel  
    0. <http://alien.slackbook.org/dokuwiki/doku.php?id=linux:kernelbuilding>
    1. Update lilo conf, with fallback
2. startx  
    Fix keyboard switching/remapping
3. Install xmonad  
    * use ghc so we can get haskell-platform
    * gtk theme use lxappearance (slackbuild)
    * get dual screen support!
    * get toolbar
    * get background
    * dzen2
    * conky
3. Get flash
3. Get adblock etc
4. Fix fonts (??)  
    * Copy .ttf fonts into /usr/local/share/fonts
    * Set path in xorg.conf
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

