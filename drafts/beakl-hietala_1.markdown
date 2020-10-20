---
title: "Beakl-Hietal 0.1"
tags: Computer, Keyboards
---

# Text

This is my layout for the Gergoplex that I've been tinkering with. It uses a couple of QMK features such as combos, home row mods, one-shot layers and double taps.

For a writeup of *why* things look like they do, see this other post.

![Legend. Does not apply to combos.](/images/beakl-hietala/legend.png)

# Base layer

![Base layer](/images/beakl-hietala/base.png)

```
Top row
                                                F + Z       Delete

Home row
    E + A           Enter               S + T               Escape
I + E               Tab                     T + N           Backspace
```

Key     Click               Click hold      Double click            Double click hold
------  ------------------  ----------      --------------------    -----------------
WIN     WIN one-shot        WIN             Left GUI                Left GUI
UP      UP one-shot         UP              FUN one-shot            FUN
Shift   Shift one-shot      Shift           Switch to MOV layer     MOV
Alt     Left alt one-shot   Left alt        Right alt one-shot      Right alt

Leader sequence     Command
-------             -----------------
C C                 Caps lock
S C                 Swap Caps lock and escape
V                   Print Version
W M                 Print Words Per Minute
G G                 Switch to gaming layer

# Symbols

![Vertical symbol combos](/images/beakl-hietala/sym-combo.png)

Combos:

```
Top row
    O + U           |               C + R               ~
                                        R + F           &
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


# Window manager layer

![Window layer](/images/beakl-hietala/win.png)

Command                 App
--------                --------
Gui + num               Switch workspace
Gui + Q/W/E             Switch screen
Gui + Shift + num       Move to workspace
Gui + Shift + Q/W/E     Move to screen
Gui + J                 Jump layout
Gui + K                 Jump layout
Gui + Shift + Enter     Terminal
Gui + Space             Switch window layout
Gui + Shift + C         Close window
Gui + Ctrl + F          Firefox
Gui + Ctrl + C          Chromium
Gui + Ctrl + M          Spotify
Gui + Ctrl + P          Screenshot
Gui + Ctrl + B          Toggle window borders
Gui + Y                 Focus urgent (unused?)


# Left handed mouse and movements

For use together with the right hand on the mouse.

![Mouse and movements](/images/beakl-hietala/mov.png)

Combo                                       Action
-------                                     ---------------------
Left + Down                                 Tab
Down + Right                                Enter
Mouse left + right click                    Mouse middle click

# Left handed shortcut layer

![Shortcut layer](/images/beakl-hietala/short.png)

# Gaming layer

![QWERTY based gaming layer](/images/beakl-hietala/gaming.png)
