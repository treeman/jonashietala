---
layout: post
title: "Building the GH60"
tags: Keyboard, Computer, Goals
---

I've finally completed my first custom made keyboard. I'm still missing stabilizers and some smaller screws to hold it all together but I've been using it the last days and I thought I'd share.

# The hardware

Several years ago I joined the [geekhack forum][] and I joined in a few group buys. I ordered some cool keycaps and I also entered the [GH60 Group Buy][] which sold plates, stabilizers, PCBs and switches so you can build a more minimalistic keyboard. Unfortunately the group buy crashed and burned and it's long overdue with the creators vanishing and leaving the group buy with not enough money. I did get my two PCBs and my switches but I'm still waiting for my plates and stabilizers... Which may never arrive.

I did source a 60% plate from [massdrop][massdrop-plate] and I used [Cherry MX Green][] switches which is a harder clicky cherry variant.

[geekhack forum]: https://geekhack.org/index.php "Geekhack forum"
[GH60 Group Buy]: https://geekhack.org/index.php?topic=41464.0 "GH60 Group Buy"
[massdrop-plate]: https://www.massdrop.com/buy/60-aluminum-plate "Sentraq 60% Keyboard Plate: Massdrop"
[Cherry MX Green]: https://deskthority.net/wiki/Cherry_MX_Green "Cherry MX Green"

# Pictures

![My beautiful solder job](/images/gh60_pink/solder.jpg){ width=400 }

![In all it's glory](/images/gh60_pink/back.jpg){ width=600 }

![You can see the plate mashed between the switches and the PCB](/images/gh60_pink/sideview.jpg){ width=600 }

![A full view of the switches from the top](/images/gh60_pink/switches.jpg){ width=600 }

![The beautiful keycaps](/images/gh60_pink/keycaps.jpg){ width=800 }

The white doesn't shine through at all when you're at the keybord, the photo isn't representative.

# Firmware

I tried out both the [tmk_keyboard][] and [qmk_firmware][] for my keyboard. qmk is basically the same as tmk but with extended features (which I don't think I'm currently using?).

The code is available on github: <https://github.com/treeman/qmk_firmware>

[qmk_firmware]: https://github.com/jackhumbert/qmk_firmware
[tmk_keyboard]: https://github.com/tmk/tmk_keyboard

Dependencies:

```{.bash}
dfu-programmer
avr-binutils
avr-gcc
avr-libc
```

And then to flash the firmware press the reset button on the PCB and do:

```{.bash}
make
dfu-programmer atmega32u4 erase
dfu-programmer atmega32u4 flash <gh60_layout.hex>
dfu-programmer atmega32u4 start
```

I'm currently using a fairly standard qwerty layout and I've moved down Escape in place of Caps Lock as I use Vim at a daily basis. I've also included a function key which if pressed will allow F1-F12 and arrow keys. Holding down Caps Lock (now Escape) also works as a function key. Holding caps lock works remarkably well, I can see why many Emacs users remap it to Ctrl.

Then I use the rightmost modifiers, which I've never even pressed?, to set up a numpad layer and a mouse layer. The mouse layer doesn't work well however... Not enough control over the movement sadly.

```{.C}
#define BASE 0
#define FUN 1
#define NUMPAD 2
#define MOUSE 3

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    /* 0: Default layer
     * ,-----------------------------------------------------------.
     * |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|BSP|DEL|
     * |-----------------------------------------------------------|
     * |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|     |
     * |------------------------------------------------------|    |
     * |Esc/Fn|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|  \|Ret |
     * |-----------------------------------------------------------|
     * |Shif|   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /| Fn|Shift |
     * |-----------------------------------------------------------|
     * |Ctrl|Gui |Alt |          Space         |Alt |    |Mous|NPad|
     * `-----------------------------------------------------------'
     */
    KEYMAP_HIETALA(
        GRV, 1,   2,   3,   4,   5,   6,   7,   8,   9,   0,   MINS,EQL, BSPC, DELETE, \
        TAB, Q,   W,   E,   R,   T,   Y,   U,   I,   O,   P,   LBRC,RBRC,            \
        FN4, A,   S,   D,   F,   G,   H,   J,   K,   L,   SCLN,QUOT,BSLS,ENT,        \
        LSFT,NUBS,Z,   X,   C,   V,   B,   N,   M,   COMM,DOT, SLSH,FN0, RSFT,       \
        LCTL,LGUI,LALT,             SPC,                  RALT,NO,  FN3,  FN1),
    /* 1: Function layer
     * ,-----------------------------------------------------------.
     * |Lr0| F1| F2| F3| F4| F5| F6| F7| F8| F9|F10|F11|F12|   |   |
     * |-----------------------------------------------------------|
     * |     |   | Up|   |   |   |Hom|PgD|PgU|End|PgU|   |   |     |
     * |------------------------------------------------------|    |
     * |      |Lef|Dow|Rig|   |   |Lef|Dow|Up |Rig|PgD|   |   |    |
     * |-----------------------------------------------------------|
     * |    |   |   |   |   |   |   |Hom|PgD|PgU|End|   |   |      |
     * |-----------------------------------------------------------|
     * |    |    |    |                        |    |    |    |    |
     * `-----------------------------------------------------------'
     */
    KEYMAP_HIETALA(
        FN2,F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9,  F10, F11, F12, TRNS,TRNS, \
        TRNS,PGDN,UP,  PGUP,TRNS,TRNS,HOME,PGDN,PGUP,END ,PGUP,TRNS,TRNS,           \
        TRNS,LEFT,DOWN,RGHT,TRNS,TRNS,LEFT,DOWN,UP  ,RGHT,PGDN,TRNS,TRNS,TRNS,      \
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,HOME,PGDN,PGUP,END ,TRNS,TRNS,TRNS,TRNS,      \
        TRNS,TRNS,TRNS,               TRNS,               TRNS,TRNS,TRNS,TRNS),
    /* 2: Numpad
     * ,-----------------------------------------------------------.
     * |Lr0|   |   |   |   |   |   |   |  *|  *|  *|  -|  +|   |   |
     * |-----------------------------------------------------------|
     * |     |   |   |   |   |   |  -|  7|  8|  9|  +|  +|   |     |
     * |------------------------------------------------------|    |
     * |      |   |   |   |   |   |  .|  4|  5|  6|  ,|  ,|  /|    |
     * |-----------------------------------------------------------|
     * |    |   |   |   |   |   |   |  0|  1|  2|  3|  .|   |KP Ent|
     * |-----------------------------------------------------------|
     * |    |    |    |                        |NLCK|    |    |    |
     * `-----------------------------------------------------------'
     */
    KEYMAP_HIETALA(
        FN2, TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,PAST,PAST,PAST,PMNS,PPLS,TRNS,TRNS, \
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,P7  ,P8,  P9,  PPLS,PPLS,TRNS,           \
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,PDOT,P4  ,P5,  P6,  COMM,COMM,PSLS,TRNS,      \
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,P0  ,P1,  P2,  P3,  PDOT,TRNS,PENT,      \
        TRNS,TRNS,TRNS,               TRNS,               NLCK,TRNS,TRNS,TRNS),
    /* 3: Mouse mode
     * ,-----------------------------------------------------------.
     * |Lr0|   |   |   |   |   |   |   |   |   |   |   |   |   |   |
     * |-----------------------------------------------------------|
     * |     |   |MUp|   |   |   |   |   |   |   |   |   |   |     |
     * |------------------------------------------------------|    |
     * |      |MLe|MDn|MRh|   |   |MLe|MDn|MUp|MRg|   |   |   |Bt1 |
     * |-----------------------------------------------------------|
     * |    |   |   |   |   |   |   |ScL|ScD|ScU|ScR|   |   |  Bt2 |
     * |-----------------------------------------------------------|
     * |    |    |    |          Bt1           |    |    |    |    |
     * `-----------------------------------------------------------'
     */
    KEYMAP_HIETALA(
        FN2 ,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS, \
        TRNS,TRNS,MS_U,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,            \
        TRNS,MS_L,MS_D,MS_R,TRNS,TRNS,MS_L,MS_D,MS_U,MS_R,TRNS,TRNS,TRNS,BTN1,        \
        TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,WH_L,WH_D,WH_U,WH_R,TRNS,TRNS,BTN2,       \
        TRNS,TRNS,TRNS,               BTN1,               TRNS,TRNS,TRNS,TRNS),
};

const uint16_t PROGMEM fn_actions[] = {
    [0] = ACTION_LAYER_TAP_TOGGLE(FUN), // Fn momentary or toggling.
    [1] = ACTION_LAYER_TOGGLE(NUMPAD), // Toggle numpad.
    [2] = ACTION_LAYER_SET(0, ON_BOTH), // Back to base layer, safety escape.
    [3] = ACTION_LAYER_TOGGLE(MOUSE), // Toggle mouse layer.
    [4] = ACTION_LAYER_TAP(FUN, KC_ESC), // On hold trigger Fn overlay. On tap act as esc.
};
```

Naturally things will change but this is what I'm rolling with for now.

