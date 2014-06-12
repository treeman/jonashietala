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


Spotify
-------

Fetch from slackbuilds. Have some flickering issues.


freetype
--------

From <http://www.linuxquestions.org/questions/slackware-14/how-to-optimize-fonts-in-slackware-640468/page29.html#post5067546>, <http://blog.lysender.com/2013/11/optimizing-fonts-for-slackware-14-1-without-infinality/>

Enable subpixel rendering from source slackbuild <http://ftp.slackware.com/pub/slackware/slackware-14.1/source/l/freetype/>. Edit freetype.Slackbuild

Save as `freetype_cleartype.diff`:

``` {.diff}
diff -rupN freetype.orig/cleartype.diff freetype/cleartype.diff
--- freetype.orig/cleartype.diff	1969-12-31 16:00:00.000000000 -0800
+++ freetype/cleartype.diff	2013-11-19 15:32:04.811346576 -0800
@@ -0,0 +1,12 @@
+diff -rupN freetype-2.5.0.1.orig/include/freetype/config/ftoption.h freetype-2.5.0.1/include/freetype/config/ftoption.h
+--- freetype-2.5.0.1.orig/include/freetype/config/ftoption.h	2013-06-19 14:20:04.000000000 -0700
++++ freetype-2.5.0.1/include/freetype/config/ftoption.h	2013-11-19 15:27:47.456737625 -0800
+@@ -591,7 +591,7 @@ FT_BEGIN_HEADER
+   /*   This option requires TT_CONFIG_OPTION_BYTECODE_INTERPRETER to be    */
+   /*   defined.                                                            */
+   /*                                                                       */
+-/* #define TT_CONFIG_OPTION_SUBPIXEL_HINTING */
++#define TT_CONFIG_OPTION_SUBPIXEL_HINTING
+ 
+ 
+   /*************************************************************************/
diff -rupN freetype.orig/freetype.SlackBuild freetype/freetype.SlackBuild
--- freetype.orig/freetype.SlackBuild	2013-11-19 15:31:53.895891885 -0800
+++ freetype/freetype.SlackBuild	2013-11-19 15:33:17.885864416 -0800
@@ -78,7 +78,8 @@ zcat $CWD/freetype.illadvisederror.diff.
 # for doing so.
 # Please see this web site for more details:
 #   http://www.freetype.org/patents.html
-#zcat $CWD/freetype.subpixel.rendering.diff.gz | patch -p1 --verbose || exit 1
+zcat $CWD/freetype.subpixel.rendering.diff.gz | patch -p1 --verbose || exit 1
+patch -p1 --verbose < $CWD/cleartype.diff
 
 chown -R root:root .
 CFLAGS="$SLKCFLAGS" make setup CFG="--prefix=/usr --libdir=/usr/lib${LIBDIRSUFFIX} --build=$ARCH-slackware-linux"
```

Then

```
lftp -c 'open ftp.slackware.com ; mirror pub/slackware/slackware64-14.1/source/l/freetype/'
cd freetype
patch -p1 < ../freetype_cleartype.diff
./freetype.SlackBuild
removepkg freetype-2.5.0.1-x86_64-1
installpkg /tmp/freetype-2.5.0.1-x86_64-1.txz
```

Enable subpixel rendering. Test <http://www.lagom.nl/lcd-test/subpixel.php>, choose rgb, gbr, or whatever. Also useful: <https://wiki.archlinux.org/index.php/Font_configuration>

```
ln -s /etc/fonts/conf.avail/10-sub-pixel-rgb.conf /etc/fonts/conf.d
ln -s /etc/fonts/conf.avail/11-lcdfilter-default.conf /etc/fonts/conf.d
```

Also use `~/.config/fontconfig/fonts.conf`:

```
<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
	<match target="font">
		<edit mode="assign" name="antialias">
			<bool>true</bool>
		</edit>
		<edit mode="assign" name="hinting">
			<bool>true</bool>
		</edit>

		<edit mode="assign" name="hintstyle">
			<const>hintslight</const>
		</edit>

		<!-- Ignore any embedded bitmaps in TTF, etc (Microsoft's Calibri and others from Office 07/Vista have these) -->
		<edit mode="assign" name="embeddedbitmap">
			<bool>false</bool>
		</edit>

		<!-- MS fonts use full hinting -->

		<test name="family">
			<string>Andale Mono</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Arial</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Arial Black</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Calibri</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Cambria</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Candara</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Comic Sans MS</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Consolas</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Constantia</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Corbel</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Courier New</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Georgia</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Impact</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Symbol</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Tahoma</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Times New Roman</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Trebuchet MS</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Verdana</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Webdings</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
		<test name="family">
			<string>Wingdings</string>
		</test>
		<edit mode="assign" name="hintstyle">
			<const>hintfull</const>
		</edit>
	</match>
</fontconfig>
```

Rest
----

1. Fix dual-screen/single-screen switching
1. Faster startup


Misc
----

1. Printer via cups
1. mtpaint
9. korean input style


Change config
-------------

1. irssi, notifications and remove flashing
1. left screen
1. Groovebasin
1. calendar.pl, specify urls and filters
1. ticker.pl, more parsing or whatever?


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

