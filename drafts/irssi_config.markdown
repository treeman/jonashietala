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

http://www.antonfagerberg.com/archive/my-perfect-irssi-setup/
http://wiki.bitlbee.org/HowtoSkype
http://thepracticalsysadmin.com/introduction-to-irssi/

Get source from `http://code.bitlbee.org/bitlbee/` make slackbuilds `OTR=yes SKYPE=yes ./bitlbee.SlackBuild`.

```
if [ -x /etc/rc.d/rc.bitlbee ]; then
    /etc/rc.d/rc.bitlbee start
fi
```

In `/etc/rc.d/rc.local`

Star it and then in irssi

`/connect localhost`

To `&bitlbee`: `register`

Next time use `identify`

Get <https://github.com/awahlig/skype4py>

Things
------

`/wc` close a window. Use to remove stupid things.
`/window size x`
