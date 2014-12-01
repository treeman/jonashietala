---
layout: post
title: irssi config
tags: irssi, slackware
---

* `adv_windowlist.pl`  
    Remove "AWL: Run adv_windowlist from the shell or switch to sbar mode" with

    `/set awl_viewer off`



http://www.antonfagerberg.com/archive/my-perfect-irssi-setup/
    * hilight window settings
    * nicklist

* `hilightwin.pl`  
    ```
    /window new split
    /window name hilight
    /window size 6
    /hilight -word <mynick>
    /layout save
    ```

Bitlbee
-------

* <http://www.antonfagerberg.com/archive/my-perfect-irssi-setup/>
* <http://wiki.bitlbee.org/HowtoSkype>
* <http://thepracticalsysadmin.com/introduction-to-irssi/>

Get source from <http://code.bitlbee.org/bitlbee/>. Create an updated package with `tar caaf <file.tar.gz> directory`. Run slackbuild:

```
OTR=yes SKYPE=yes ./bitlbee.SlackBuild
```

```
if [ -x /etc/rc.d/rc.bitlbee ]; then
    /etc/rc.d/rc.bitlbee start
fi
```

In `/etc/rc.d/rc.local`

Start it and then in irssi

`/connect localhost`

To `&bitlbee`: `register` with a bitlbee password.

Next time use `identify`

Get <https://github.com/awahlig/skype4py>

Install `python setup.py install` and run `skyped`. Create a config in `~/.skyped/skyped.conf`:

```
[skyped]
# change to your skype username
username = madeoftree
# use `echo -n foo|sha1sum` to generate this hash for your password
# This is only place for your skype password
password = <hash>

# you have to change the following paths to your home directory:
cert = <home-dir>/.skyped/skyped.cert.pem
key  = <home-dir>/.skyped/skyped.key.pem
port = 2727
```

If connection issues, skyped probably isn't started all too well. Debug with: `skyped --nofork --debug`.

Add `skyped` to autostart.

Things
------

`/wc` close a window. Use to remove stupid things.
`/window size x`
