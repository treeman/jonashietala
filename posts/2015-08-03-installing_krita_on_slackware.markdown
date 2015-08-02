---
layout: post
title: Installing Krita on Slackware 14.1
tags: Krita, Slackware
---

This is a guide on how to build [Krita][] on Slackware 14.1. This is based on [this guide for linux][install_krita].

1. `removepkg calligra`
2. Install some dependencies from [Slackbuilds][].
    ```
    postgresql
    gsl
    libgexiv2
    libpqxx
    pstoedit
    ```
    `postgresql` needs some additional setup, see the slackbuilds readme.
3. Get Krita.  
    The [original guide][install_krita] recommends building in `~/kde4` but I moved i to `/opt/kde4`.

    ```
    mkdir -p /opt/kde4/build /opt/kde4/src /opt/kde4/inst
    cd /opt/kde4/src
    git clone git://anongit.kde.org/calligra.git
    ```
3. Configure and build.  
    There's a problem with tifflib.

    Change `/opt/kde4/src/calligra/krita/plugins/formats/tiff/kis_tiff_converter.cc` from

    ```
    #if TIFFLIB_VERSION < 20111221
    typedef size_t tmsize_t;
    #endif
    ```

    to

    ```
    typedef size_t tmsize_t;
    ```

    Then we can build

    ```
    cd /opt/kde4/build
    cmake -DCMAKE_INSTALL_PREFIX=/opt/kde4/inst /opt/kde4/src/calligra -DCMAKE_BUILD_TYPE=RelWithDebInfo -DPRODUCTSET=KRITA
    make -j5
    make install -j5
    ```

    Where `X` in `jX` is `1 + # processors`. The build process is quite slow.
4. Add `/opt/kde4/inst/bin` to PATH and `/opt/kde4/inst` to KDEDIRS.
5. Register krita to the system `kbuildsyscoca4`, but worked for me without it (I don't use kde).

And launch with `krita`.


[install_krita]: http://www.davidrevoy.com/article193/guide-building-krita-on-linux-for-cats "Install Krita"
[Krita]: https://krita.org/ "Krita Digital Painting"
[Slackbuilds]: http://slackbuilds.org "Slackbuilds"
