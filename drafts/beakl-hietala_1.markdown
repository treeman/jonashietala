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

Combos:

```
Top row
                                                F + B       Delete

Home row
    E + A           Enter               S + T               Escape
I + E               Tab                     T + N           Backspace

Bottom row
. + , + K           Movement layer
```

Tap dance on Alt key:

Sequence            Command
-------             -----------------
Click               One-shot left alt
Click hold          Left alt mod
Double click        One-shot right alt
Double click hold   Right alt mod

# Symbols

![Vertical symbol combos](/images/beakl-hietala/sym-combo.png)

Combos:

```
Top row
    O + U           |               C + R               ~
H + O               +                   R + F           &
H +     U           $               C +     F           %
H + O + U           `               C + R + F           ^

Home row
I +     A           (               S +     N           )
I + E + A           "               S + T + N           '
```

# Number & extra character layer

![UP layer](/images/beakl-hietala/num.png)

```
Home row
    Ä + Å           Numpad enter
```



# Function layer

![Function layer](/images/beakl-hietala/fun.png)


# Left handed mouse and movements

For use together with the right hand on the mouse.

![Mouse and movements](/images/beakl-hietala/mov.png)

Key         Double click
----        -------
End         Escape

Combo                                       Action
-------                                     ---------------------
Left + Down                                 Tab
Down + Right                                Enter
Mouse left + right click                    Mouse middle click
Mouse left + (empty) + Mouse right click    Deactivate movement layer

# Left handed shortcut layer

![Shortcut layer](/images/beakl-hietala/short.png)

# Gaming layer

![QWERTY based gaming layer](/images/beakl-hietala/gaming.png)

# Leader sequences

After pressing the "Leader" key, issue a sequence of keypresses to trigger a command:

Sequence    Command
-------     -----------------
C           Caps lock
Escape      Swap Caps lock and escape
V           Print Version
Z           Print Words Per Minute
