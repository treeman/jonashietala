---
layout: post
title: "Offset coordinates in Krita with Xinerama"
tags: Slackware, Krita
---

I recently bought an [Intuos Manga][] drawing tablet recently, because I got this fix idea that I want to learn how to draw. And what better way to do it than with a drawing tablet, while satisfying my need for new things?

![](/images/galleryimage2IntuousManga.jpg)

With little experience I boldly set forth and I found a lovely and free drawing program [Krita][]. I've used photoshop earlier but it's just so expensive and [Krita][] seems like a good replacement. [Inkscape][] is another good alternative for vector graphics.

To install [Krita][] you need KDE, which should come preinstalled with Slackware. If you're like me and decided to skip it you can install it with

```{.bash}
slackpkg install kde
```

I had some trouble installing [Krita][], but ultimately [this guide with cats][install_krita] worked when I also changed `krita/plugins/formats/tiff/kis_tiff_converter.cc` from

```{.C}
#if TIFFLIB_VERSION < 20111221
typedef size_t tmsize_t;
#endif
```

to

```{.C}
typedef size_t tmsize_t;
```

which I filed as a bug report over at KDE.

More troubling was the fact that drawing on the canvass was offset using dual screens with Xinerama. It worked fine in both Inkscape and Gimp and even on the gui controls. The problem went away if I switched to only a single screen. Now [this][bug1] [is][bug2] [a][bug3] [common][bug4] [problem][bug5] [with][bug6] Xinerama and Krita.

I did not manage to solve it using Xinerama, the bug is still unfixed. It works perfectly if instead of Xinerama RandR is used to setup the dual screen layout. I managed to set it up thanks to the [excellent arch linux wiki][xrandr_wiki].

The final result for me is to add this to `.xinitrc` and use a simple `xorg.conf`:

```{.bash}
xrandr --output DVI-I-3 --mode 1920x1080 --pos 0x0 --rotate left --output DVI-I-2 --mode 1920x1080 --pos 1080x480
```

And Krita (and Inkspace, Gimp, ...) works perfectly.

My full workspace setup is [on github][workspace].

[Intuos Manga]: http://www.wacom.com/en-us/products/pen-tablets/intuos-manga "Intuos Manga drawing tablet"
[Krita]: https://krita.org/ "Krita Digital Painting"
[inkscape]: https://inkscape.org/en/ "Inkscape"
[install_krita]: http://www.davidrevoy.com/article193/guide-building-krita-on-linux-for-cats "Install Krita"
[bug1]: https://bugzilla.gnome.org/show_bug.cgi?id=634977 "Krita dualscreen bug 1"
[bug2]: https://bugzilla.gnome.org/show_bug.cgi?id=66813 "Krita dualscreen bug 2"
[bug3]: https://forum.kde.org/viewtopic.php?f=139&t=120228 "Krita dualscreen bug 3"
[bug4]: https://bugs.kde.org/show_bug.cgi?id=298144 "Krita dualscreen bug 4"
[bug5]: https://bbs.archlinux.org/viewtopic.php?id=142144 "Krita dualscreen bug 5"
[bug6]: https://bugs.launchpad.net/ubuntu/+source/wacom-tools/+bug/301075 "Krita dualscreen bug 6"
[xrandr_wiki]: https://wiki.archlinux.org/index.php/Xrandr "Xrandr"
[workspace]: https://github.com/treeman/dotfiles/tree/master/.workspace "Workspace dotfiles"

