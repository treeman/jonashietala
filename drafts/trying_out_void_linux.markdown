---
title: "Trying out Void Linux"
tags: "Void Linux"
---

After years of running primarily Slackware the last year or so I've instead moved on to Void Linux. When I recently reinstalled it on my desktop I thought I'd document the process, so I might save some time in the future.


# Installation

The installation is pretty straightforward, just follow the [installation wiki][installation]. I installed from a live usb and chose to install from the network. The one snag I ran into was I had used an old image, so it couldn't connect to the repository. This is the partition layout I ran with:

```
Filesystem      Size  Used Avail Use% Mounted on
/dev/nvme1n1p2   98G   21G   72G  23% /
/dev/nvme1n1p3  356G   17G  321G   5% /home
/dev/sda1       916G  641G  229G  74% /store
```

And a swap partition at `/dev/nvme1n1p1`. Since I had already a separate `/home` partition I could skip most of the configuration after installation.

There is also a [post installation section][post-installation] of the wiki that might be useful.


# xbps

The main reason I switched away from Slackware was the lack of package system. Well, there sort-of was one (or several) but you had to do a bunch of stuff manually. The package system that Void Linux uses, [XBPS][], is therefore a breath of fresh air.

To search for a package:

```
xbps-query -Rs neovim
```

And to install it:

```
xbps-install -Su neovim
```

The commands take some funky commands sometimes, but otherwise it works for basically everything I want.


# runit

The other nice thing about Void Linux is their init system runit.

It's pretty simple and the available scripts to be run at startup exists under `/etc/sv/`:

```
$ ls /etc/sv/
acpid/           agetty-hvsi0/   agetty-tty3/  agetty-ttyAMA0/  cupsd/        dmeventd/   slapd/    uuidd/
agetty-console/  agetty-serial/  agetty-tty4/  agetty-ttyS0/    dbus/         ip6tables/  sshd/     wicd/
agetty-generic/  agetty-tty1/    agetty-tty5/  agetty-ttyUSB0/  dhcpcd/       iptables/   sulogin/  wpa_supplicant/
agetty-hvc0/     agetty-tty2/    agetty-tty6/  alsa/            dhcpcd-eth0/  polkitd/    udevd/
```

And the scripts that will run are symlinked under `/var/service`:

```
$ ls /var/service/ -l
lrwxrwxrwx 1 root root 13 May  5 16:08 udevd -> /etc/sv/udevd/
lrwxrwxrwx 1 root root 13 May  5 16:13 wicd -> /etc/sv/wicd//
...
```

Enabling one is as easy as symlinking it:

```
$ ln -s /etc/sv/sshd/ /var/service
```

And it's just as easy to control a running service:

```
$ sv status sshd
run: sshd: (pid 1029) 11670s
```

```
$ sv down sshd
```

```
$ sv status sshd
down: sshd: 1s, normally up
```

```
$ sv up sshd
```

```
$ sv status sshd
run: sshd: (pid 21332) 1s
```


# Getting to X

* xmonad
* xmonad-contrib


# Spotify

Sound?


# Login shell


# Updating time

Change `HARDWARECLOCK` in `/etc/rc.conf` to use localtime:

```
HARDWARECLOCK="localtime"
```


[void-time]: https://docs.voidlinux.org/config/date-time.html


# Yubikey


# Ledger


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
[dotfiles]: https://github.com/treeman/dotfiles
[voidlinux]: https://voidlinux.org/
[XBPS]: https://wiki.voidlinux.org/XBPS
[dotfiles]: https://github.com/treeman/dotfiles


