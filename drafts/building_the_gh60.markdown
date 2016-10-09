---
layout: post
title: "Building the GH60"
tags: Keyboard
---

# tmk keyboard firmware

Some dependencies:

```{.bash}
dfu-programmer
avr-binutils
avr-gcc
avr-libc
```

<https://github.com/tmk/tmk_keyboard>

With these commands:

```{.bash}
make
dfu-programmer atmega32u4 erase
dfu-programmer atmega32u4 flash gh60_lufa.hex
dfu-programmer atmega32u4 start
```

# qmk keyboard firmware

Dependencies:

```{.bash}
dfu-programmer
avr-binutils
avr-gcc
avr-libc
```

![](/images/gh60_pink/back.jpg)
![](/images/gh60_pink/keycaps.jpg)
![](/images/gh60_pink/sideview.jpg)
![](/images/gh60_pink/solder.jpg)
![](/images/gh60_pink/switches.jpg)

