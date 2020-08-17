---
title: "Beakl-Hietala 0.1"
tags: Computer, Keyboards
---

# Text

This is my layout for the Gergoplex that I've been tinkering with. It uses a couple of QMK features such as combos, home row mods, one-shot layers and double taps.

For a writeup of *why* things look like they do, see this other post.

![Legend. Does not apply to combos.](/images/beakl-hietala/legend.png)

# Base layer

![Base layer](/images/beakl-hietala/base.png)

Combos are activated by pressing two keys at the same time. Because of switches with low resistance and flat keycaps it's easy to press between the keys.

![Special combos](/images/beakl-hietala/coord1.png)

# Symbols

![Symbol combos on the base layer](/images/beakl-hietala/sym-combo.png)

![Symbols layer](/images/beakl-hietala/sym.png)

# Numbers and functions

![Number layer](/images/beakl-hietala/num.png)

![Function layer](/images/beakl-hietala/fun.png)

# Left handed mouse and movements

For use together with the right hand on the mouse.

![Mouse and movements](/images/beakl-hietala/mov.png)

Mouse left + right click combos to middle click.

# Qwerty shortcuts

A one-shot left-hand QWERTY layer activated with combos.

Combo               Mods
-----               ------
Space + Y           none
Space + I           Ctrl
Space + I + A       Ctrl + Shift
Space + I + E       Ctrl + Alt
Space + E           Alt
Space + E + A       Alt + Shift
Space + I + E + A   Ctrl + Alt + Shift

# Leader sequences

After pressing the "Leader" key, issue a sequence of keypresses to trigger a command:

Sequence    Command
-------     -----------------
Return      GUI click
a           å
e           ä
i           ö
. e         é
, e         è
. a         á
, a         à
s u         Swap Unicode Linux/Win
s c         Swap Caps Lock and Escape
c l         Caps Lock
p v         Print Version
e u         €
b t c       ₿
w p m       Print Words Per Minute

# TODO

1. One-shot QWERTY layer with mods, for shortcuts
2. Unicode swapping, is it even possible?
3. Special character outputs
5. WPM

