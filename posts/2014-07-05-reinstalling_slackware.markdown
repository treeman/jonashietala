---
layout: post
title: "Reinstalling Slackware"
category: Slackware
tags: Slackware
---

So I reinstalled slackware on my machine and decided to take some rough notes of the most important steps I made. I did not document the steps in detail, and some are very specific for my setup. But maybe it can be useful for someone, or myself.

1. [Basic steps]
    1. [Make partitions]
    1. [Setup]
    1. [Config]
    1. [Install wicd, get online]
    1. [Custom Kernel]
    1. [Fix X]
1. [Programs]
    1. [Firefox]
    1. [vim]
    1. [Skype]
    1. [Spotify]
    1. [Office]
    1. [cron]
1. [Appearance]
    1. [xmonad]
    1. [Fonts]
    1. [Terminal]
1. [Random Installs]
    1. [Slackbuilds]
    1. [Perl]
    1. [Multilib]
    1. [Better latex]
    1. [Groovebasin]
    1. [Java]
    1. [Anki]
1. [Programming]
    1. [Hakyll]
    1. [SFML]
1. [Games]
    1. [Minecraft]

Basic steps
===========

Make slackware usb loader
-------------------------

See `README_USB.TXT` in usb folder from slackware installation.

Create

`dd if=usbboot.img of=/dev/sdX bs=1M`bash

Be sure `/dev/sdX` is the usb, dd will wipe everything! Simple way is to `ls /dev`bash before and after plugging in device.  Boot from bios (f2 or f10).


Make partitions
---------------

Make partitions <http://slackbook.org/html/installation-partitioning.html>

`fdisk /dev/sda`. Make sure `sda` is your harddrive.  Print partitions `p`

Current setup:

```bash
tmpfs       4G      swap
/dev/sda2   50G     /
/dev/sda3   50G     /usr/local
/dev/sda5   rest    /home
```

Make sure that swap is of type Linux Swap, change with `t`. Approx 2 times your ram?


Setup
-----

Boot and run `setup`.

Install from FTP/HTTP server:

1. `ftp://ftp.slackware.com`
2. `/pub/slackware/slackware64-14.1/slackware64`

For example.

Don't pick KDE or Games. Use terse installation.


Config
------

Add a new user with `adduser`.  Set zsh as basic shell. Set for root in `/etc/passwd` (or update user info there).

Add user to groups.

```
# usermod -a -G netdev username
```

Update name in `/etc/HOSTNAME`.


Install wicd, get online
------------------------

Fetch package from `/extra/wicd`.

```bash
installpkg ...
chmod +x /etc/rc.d/rc.wicd
/etc/rc.d/rc/wicd start
wicd-curses
```

Custom Kernel
-------------

<http://alien.slackbook.org/dokuwiki/doku.php?id=linux:kernelbuilding>

Fetch latest stable kernel source: <https://www.kernel.org/>

```bash
cd /usr/src
wget https://www.kernel.org/pub/linux/kernel/v3.x/linux-3.15.1.tar.xz   # Or whatever
tar xf linux-3.15.1.tar.xz
rm linux
ln -s linux-3.15.1 linux
cd linux
```

Use slackware custom as base:

```bash
# wget or cp to dir
wget ftp://ftp.slackware.com/pub/slackware/slackware64-14.1/source/k/config-x86_64/config-generic-3.10.17.x64
mv config-generic-3.10.17.x64 .config
make oldconfig
make menuconfig
```

Make sure to select processor type, preemptive low latency desktop. Remove `nvidia` and `riba` for nvidia binary blob usage later.

```bash
make bzImage modules
make modules_install
cp arch/x86_64/boot/bzImage /boot/vmlinuz-custom-3.16.3
cp System.map /boot/System.map-custom-3.16.3
cp .config /boot/config-custom-3.16.3
cd /boot
rm System.map
ln -s System.map-custom-3.16.3 System.map
```

Update `/etc/lilo.conf`. This is mine:

```bash
# Start LILO global section
lba32 # Allow booting past 1024th cylinder with a recent BIOS
compact # Fast boot

# Append any additional kernel parameters:
append=" vt.default_utf8=1 logo.nologo"
boot = /dev/sda

# Boot BMP Image.
# Bitmap in BMP format: 640x480x8
bitmap = /boot/slack.bmp
# Menu colors (foreground, background, shadow, highlighted
# foreground, highlighted background, highlighted shadow):
bmp-colors = 255,0,255,0,255,0
# Location of the option table: location x, location y, number of
# columns, lines per column (max 15), "spill" (this is how many
# entries must be in the first column before the next begins to
# be used. We don't specify it here, as there's just one column.
bmp-table = 60,6,1,16
# Timer location x, timer location y, foreground color,
# background color, shadow color.
bmp-timer = 65,27,0,255

# Standard menu.
# Or, you can comment out the bitmap menu above and
# use a boot message with the standard menu:
#message = /boot/boot_message.txt

# Wait until the timeout to boot (if commented out, boot the
# first entry immediately):
prompt
# Timeout before the first entry boots.
# This is given in tenths of a second, so 600 for every minute:
timeout = 100

# Override dangerous defaults that rewrite the partition table:
change-rules
  reset

vga = 795

image = /boot/vmlinuz-custom-3.14.5
    root = /dev/sda2
    label = Slack
    read-only

image = /boot/vmlinuz
    root = /dev/sda2
    label = Backup
    read-only
```

Make sure to change image locations and drive location.

Then run `lilo`.


Fix X
-----

Install NVIDIA drivers 337.25.

Use custom `xorg.conf`.

```bash
ln -s dotfiles/.workspace .
ln -s /etc/X11/xorg.conf.d/xorg.conf .workspace/xorg.conf
```

This is it:

```
Section "ServerLayout"
    Identifier "Layout0"
    Screen 0 "Screen0" 1080 480
    Screen 1 "Screen1" 0 0
    InputDevice "Keyboard0" "CoreKeyboard"
    InputDevice "Mouse0" "CorePointer"
    Option "Xinerama" "1"
EndSection

Section "Files"
    FontPath "/usr/lib64/X11/fonts/misc/:unscaled"
    FontPath "/usr/lib64/X11/fonts/100dpi/:unscaled"
    FontPath "/usr/lib64/X11/fonts/75dpi/:unscaled"
    FontPath "/usr/lib64/X11/fonts/misc/"
    FontPath "/usr/lib64/X11/fonts/Type1/"
    FontPath "/usr/lib64/X11/fonts/Speedo/"
    FontPath "/usr/lib64/X11/fonts/100dpi/"
    FontPath "/usr/lib64/X11/fonts/75dpi/"
    FontPath "/usr/lib64/X11/fonts/cyrillic/"
    FontPath "/usr/lib64/X11/fonts/TTF/"
EndSection

Section "InputDevice"
    Identifier "Mouse0"
    Driver "mouse"
    Option "Protocol" "auto"
    Option "Device" "/dev/psaux"
    Option "Emulate3Buttons" "no"
# Option "ZAxisMapping" "4 5"
EndSection

Section "InputDevice"
    Identifier "Keyboard0"
    Driver "kbd"
    Option "XkbLayout" "us"
EndSection

Section "InputClass"
    Identifier "Keyboard Defaults"
    MatchIsKeyboard "yes"
    Option "XkbLayout" "us"
EndSection

Section "Module"
    Load "dbe"
EndSection

Section "Monitor"
    Identifier "Monitor0"
    VendorName "Unknown"
    ModelName "DELL U2211H"
    HorizSync 30.0 - 83.0
    VertRefresh 56.0 - 76.0
    Option "DPMS"
EndSection

Section "Monitor"
    Identifier "Monitor1"
    VendorName "Unknown"
    ModelName "DELL U2211H"
    HorizSync 30.0 - 83.0
    VertRefresh 56.0 - 76.0
    Option "DPMS"
    #Option "Rotate" "left"
EndSection

Section "Device"
    Identifier "Device0"
    Driver "nvidia"
    VendorName "NVIDIA Corporation"
    BoardName "GeForce GTX 550 Ti"
    BusID "PCI:1:0:0"
    #Option "RandRRotation" "on"
    Screen 0
EndSection

Section "Device"
    Identifier "Device1"
    Driver "nvidia"
    VendorName "NVIDIA Corporation"
    BoardName "GeForce GTX 550 Ti"
    BusID "PCI:1:0:0"
    #Option "RandRRotation" "on"
    Screen 1
EndSection

Section "Screen"
    Identifier "Screen0"
    Device "Device0"
    Monitor "Monitor0"
    DefaultDepth 24
    Option "TwinView" "0"
    Option "metamodes" "DFP-0: nvidia-auto-select +0+0"
    SubSection "Display"
        Depth 24
    EndSubSection
EndSection

Section "Screen"
    Identifier "Screen1"
    Device "Device1"
    Monitor "Monitor1"
    DefaultDepth 24
    Option "TwinView" "0"
    Option "metamodes" "DFP-2: nvidia-auto-select +0+0 { Rotation=left }"
    Option "Rotate" "cw"
    SubSection "Display"
        Depth 24
    EndSubSection
EndSection
```

Programs
========

Firefox
-------

Download latest

```bash
tar xf ...
mv firefox /usr/local/lib64/
cd /usr/bin
rm firefox
ln -s /usr/local/lib64/firefox/firefox .
```

Get `libflashplayer.so` into `/usr/local/lib64/firefox/browser/plugins`.

Restore bookmark backup from `.mozilla/firefox/X.default/bookmarkbackups` if you want.


vim
---

For xterm copying and support for more plugins. Get vim source.

```bash
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


Skype
-----

Install [Multilib]. Install skype from slackbuilds, use 32bit mode.


Spotify
-------

Install [Multilib]. Fetch from slackbuilds. I'm having some flickering issues, but no idea what to do.


Office
------

Install [Java]. Install `libreoffice` from slackbuilds.


cron
----

`crontab -e`

```
# Every 5 minutes
*/5 * * * * /home/tree/bin/ticker --update
```


Appearance
==========

xmonad
------

1. Install ghc linked from slackbuilds
1. Install hscolour from slackbuilds (for haskell-platform warnings)
1. Install haskell-platform from slackbuilds

As user:

```bash
cabal update
cabal install cabal-install
cabal install xmonad
cabal install xmonad-contrib
ln -s dotfiles/.xmonad ~
```

Install `conky` with dependencies from slackbuilds. Build conky with lua support.

Install `dzen2` by cloning from github. Edit `config.mk`, choose option 7 (XPM, XFT, Xinerama).

Get `.backgrounds` and `.icons`.

Could not get nitrogen to build properly (I stole it from my other installation...!!)

Fix borders on firefox etc with `lxappearance`, install from slackbuilds.

Fonts
-----

Copy ttf fonts to `/usr/share/fonts/TTF`, in that dir run

```bash
mkfontscale
mkfontdir
fc-cache -fv
```

In `/usr/bin/startx` mod a line with `defaultserverargs="-dpi 96"`.


From <http://www.linuxquestions.org/questions/slackware-14/how-to-optimize-fonts-in-slackware-640468/page29.html#post5067546>, <http://blog.lysender.com/2013/11/optimizing-fonts-for-slackware-14-1-without-infinality/>

Enable subpixel rendering from source slackbuild <http://ftp.slackware.com/pub/slackware/slackware-14.1/source/l/freetype/>. Edit freetype.Slackbuild

Save as `freetype_cleartype.diff`:

```diff
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

```bash
lftp -c 'open ftp.slackware.com ; mirror pub/slackware/slackware64-14.1/source/l/freetype/'
cd freetype
patch -p1 < ../freetype_cleartype.diff
./freetype.SlackBuild
removepkg freetype-2.5.0.1-x86_64-1
installpkg /tmp/freetype-2.5.0.1-x86_64-1.txz
```

Enable subpixel rendering. Test <http://www.lagom.nl/lcd-test/subpixel.php>, choose rgb, gbr, or whatever. Also useful: <https://wiki.archlinux.org/index.php/Font_configuration>

```bash
ln -s /etc/fonts/conf.avail/10-sub-pixel-rgb.conf /etc/fonts/conf.d
ln -s /etc/fonts/conf.avail/11-lcdfilter-default.conf /etc/fonts/conf.d
```

Also use `~/.config/fontconfig/fonts.conf`:

```xml
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

Terminal
--------

Fetch `rxvt-unicode` from slackbuilds. Make `.Xresources`.

Color schemes with ls listings will be ugly, so copy `DIR_COLORS` to `.dir_colors`

```C
STICKY_OTHER_WRITABLE 35;40
OTHER_WRITABLE 34;40
```

Set in .zshrc.

Fix git diff colors `git config --global core.pager "less -r"`

Also korean signs.


Stuff
=====


Random Installs
===============

Slackbuilds
------------------

Download from <http://slackbuilds.org/>. Install with `installpkg`. Remove with `removepkg`.

```
s3cmd, scrot, mirage, rtorrent
```


Perl
----

Install perl libs from cpan. As root:

```bash
cpan install cpan
cpan install App::Ack
cpan install Modern::Perl
cpan install DateTime
cpan install Data::ICal
cpan install LWP
# And possibly other things
```


Multilib
--------

<http://alien.slackbook.org/dokuwiki/doku.php?id=slackware:multilib> Also add blacklist.

When installing 32bit run `. /etc/profile.d/32dev.sh`


Better latex
------------

Install `texlive` from slackbuilds. Remove `tetex` first.


Groovebasin
-----------

Need `libgroove`.  Which also needs `speex` from slackbuilds apart from the clear dependencies. Run groovebasin as user.


Java
----

Get `OpenAL` from slackbuilds.  Get Java JDK <http://docs.slackware.com/howtos:software:java>, not OpenJDK.


Anki
----

Download anki.

```
mv anki-2.0.26 /usr/local/lib64
ln -s /usr/local/lib64/anki-2.0.26/runanki /usr/local/bin
```


Programming
===========


Hakyll
--------------

```bash
cabal install hakyll
cabal install MissingH
```

For upload scripts fetch `python-magic` slackbuilds.


SFML
----

Get source. `cmake` and install.


Games
=====

Minecraft
---------

Requires [Java]. Launch with:

```bash
java -Xmx2048M -Xms1024M -jar ~/.minecraft/launcher/Minecraft.jar
```

I copied `.minecraft` folder with saves etc.

