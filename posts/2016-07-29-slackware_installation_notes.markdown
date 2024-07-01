---
layout: post
title: "Slackware installation notes"
tags: Slackware
---

It seems like it's been a [yearly][slack1] [recurrence][slack2] reinstalling slackware from scratch. This time it happened during my vacation when I was mucking around with trying to compile erlang with wxWidgets support and somehow a make clean started to remove / and I only noticed it too late... Not sure how it happened but I had changed things in the make and config files. Oh well no data lost just annoyances.

This process was done with Slackware 14.2 and roughly details what I've done so I can retrace my steps in the future.

[slack1]: /blog/2015/08/02/slackware_update/ "Slackware update 2015"
[slack2]: /blog/2014/07/05/reinstalling_slackware/ "Slackware update 2014"

# Up and running

## Booting

Even with only a laptop with windows installed hope is not lost. The almighty alien has a [great tutorial][win-usb] creating an usb boot loader from windows. At first I tried to extract the contents of usbboot.img using [IZarc][] but it reported it as empty but [Winimage][] worked fine. The rest went smoothly.

[win-usb]: http://alien.slackbook.org/blog/welcome-windows-user/ "Create usb boot loader from windows"
[IZarc]: https://www.izarc.org/ "IZarc"
[Winimage]: http://www.winimage.com/winimage.htm "Winimage"

## Partitions

I used a split of `/`, `/usr/local/`, `/home`. The partitions on my laptop looks messy as I've kept some factory windows partitions.

```bash
$ df -h
Filesystem      Size  Used Avail Use% Mounted on
/dev/root        50G   27G   21G  57% /
devtmpfs        2.9G     0  2.9G   0% /dev
tmpfs           2.9G  1.1M  2.9G   1% /run
tmpfs           2.9G  688K  2.9G   1% /dev/shm
cgroup_root     2.9G     0  2.9G   0% /sys/fs/cgroup
/dev/sda5        50G   93M   47G   1% /usr/local
/dev/sda7       148G  2.0G  139G   2% /home
/dev/sda2       187G   88G   99G  48% /mnt/win
cgmfs           100K     0  100K   0% /run/cgmanager/fs
```

## Wireless

Can be found in `/extra/wicd` in the slackware release.

```bash
installpkg ...
chmod +x /etc/rc.d/rc.wicd
/etc/rc.d/rc/wicd start
wicd-curses
```

## sbopkg/slackpkg

Use slackpkg to update official Slackware packages.

```bash
slackpkg update
slackpkg update gpg
```

The vast majority of packages I install can be found on [Slackbuilds][]. Many unnamed dependencies can simply be found here. [sbopkg][] makes it a lot easier to use.

[Slackbuilds]: https://slackbuilds.org/ "Slackbuilds"
[sbopkg]: http://www.sbopkg.org/ "sbopkg"


# Environment

To quickly get up and running use xfce and `startx`.

Both `fish` and `neovim` can be installed from slackbuilds. Make sure to set fish as login shell as well as the default shell for both root and user.

## dotfiles

Store <https://github.com/treeman/dotfiles> in `~/dotfiles` and symlink from there as needed.

## Xmonad

Install `ghc` and `cabal-install` from slackbuilds and then use cabal to install the rest:

```bash
cabal install cabal-install
cabal install xmonad
cabal install xmonad-contrib
```

Use sbopkg to install conky (make sure to manually include lua support) and nitrogen.

Clone <https://github.com/robm/dzen> and edit `config.mk`, choose option 7 (XPM, XFT, Xinerama). I'm not 100% I need to do it for the laptop or if this only was needed to support Xinerama but I did it this way anyway.

When started use `lxappearance` to set a prettier look for firefox and other gui.  See [previous post][fonts] about prettifying fonts.

[fonts]: /blog/2014/07/05/reinstalling_slackware/#Fonts "Prettify fonts"

# Dev

## Perl

Use cpan as root whenever missing packages are found:

```bash
cpan install CPAN
cpan install Modern::Perl
cpan install DateTime
```

## Blog

The [blog](https://github.com/treeman/jonashietala) uses Hakyll.

```bash
cabal install missingH
cabal install hakyll
```

Also setup `~/.s3cfg` to allow syncing.

## Phoenix

1. Download the erlang-otp slackbuild and [find the latest version](http://erlang.org/download/).
2. Install [latest Elixir](https://github.com/elixir-lang/elixir/releases/) from source.
3. Follow the [installation guide](http://www.phoenixframework.org/docs/installation) and install the dependencies.

