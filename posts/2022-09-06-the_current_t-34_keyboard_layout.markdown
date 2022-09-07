---
title: "The current T-34 keyboard layout"
tags: [Computer, Keyboards, T-34]
---

I've been documenting my own keyboard layout [in a series][t-34-series] for a while now. But as the layout is constantly changing it's been difficult to piece together how the layout currently looks like, so this post tries to show how the layout looks right now in it's entirety.

If you want to read about the background and philosophy about the layout, please see the [original T-34 post][t-34], and to see it's evolution please refer to the [T-34 series][t-34-series]. The layout is implemented using [QMK][] and the [code is on GitHub][src].

The layout is made for a small 34 key keyboard, I use the [Ferris][] with flat choc keycaps to easily press [combos].

# Legend

![Legend](/images/t-34-curr/legend.png)

# Layers

[Layers][] are the meat and potatoes of any layout for a smaller keyboard, and mine is no different.

## Base

![Base layer.](/images/t-34-2/base.png)

The [repeat key](#repeat-key) is used to output the last pressed key. I shift keys using [auto shift][] (see [long press]) and [CAPSWORD]. The letters `Z` and `Q`, together with a bunch of other keys, are on [combos].

## Swedish overlay

![Swedish overlay.](/images/t-34/swe.png)

When I want to write Swedish I activate this layer that replaces `()_` with `åäö`, or I use [combos] from any layer.

## Navigation layer

![Navigation layer.](/images/t-34-2/nav.png)

`Gui-W`, `Gui-E` and `Gui-R` are used to switch between monitors and `Gui-J`/`Gui-K` to switch windows in [xmonad][]. `Ctrl` + arrow is used to switch windows in Vim. Tabbing is for switching tabs in Firefox.

## Workspace layer

![Workspace layer. All keys have an implicit `Gui` modifier.](/images/t-34-2/wnav.png)

This is used for all window and workspace management in [xmonad][]. Some common operations are also on the [navigation layer].

## Numbers

![Numbers. The darkened keys turn off [NUMWORD].](/images/t-34-2/num.png)

While I can activate the number layer persistently (using [leader sequences]) I typically use [combos][num-sym-combos] for single digitis (like `0`), or [NUMWORD] for larger numbers (like `1984`).

[NUMWORD] makes the number layer smart, so it will deactivate when certain keys are pressed (colored dark gray in the image). It's used to type numbers in text or code and for relative movement in Vim, where `17J` would move 17 lines down and then turn off the number layer.

## Function keys

![Function keys.](/images/t-34-2/fun.png)

## Symbols

![Symbols.](/images/t-34-2/sym.png)

Similar to the numbers layer, there are [combos] that are used to output standalone symbols. The [combos] follow the layout of the symbols layer, and combos with same-side thumb. So `Space` + left-hand key or `E` + right-hand key outputs a symbol, regardless of what layers are activated.

Some common symbol sequences, like `->`, exists as [combos] and others, like ` != `, as [long press].

## One-handed Shortcuts

![Shortcuts.](/images/t-34-2/shrt.png)

They're laid out in some sort of logical order, to give access to common QWERTY shortcuts while not disturbing the position of my layout too much.

However, after having moved `Ctrl` to a [combo](#combos) to make it much more accessible, the layer doesn't make sense anymore, and it would be more logical to just use the QWERTY layout here.

## Special symbols

![Specials (with ←↓↑→ symbols, not arrow keys).](/images/t-34-2/spec.png)

Some of these are [dead keys][], to add diacritic to any letter. To get `é` I do `´` then `e`, and the operating system will merge them together.

## Modifiers

![Modifiers.](/images/t-34-2/mods.png)

I typically use [long press] for shift and [combos] for other modifiers, this layer is a fallback for when those aren't enough.


# Combos

[Combos][qmk-combos] is another fantastic tool that I (ab)use a lot. Simply put it allows you to press multiple keys at once and acts as an additional key---very useful for smaller layouts.

Note that combos are layer independent, and work the same regardless of what layers are activated. The base layer is shown in the graphics for reference.

## Horizontal combos

![Horizontal combos with nearby keys.](/images/t-34-curr/hcombos.png)

Note that some have a separate hold behaviour; for instance holding `Escape` activates the [symbols layer](#symbols), allowing me to output `[]` easily, and holding `Tab` activates the [modifiers layer](#modifiers).

`SWE` activates the [Swedish layer](#swedish-overlay), and if prefixed with `()_` it will replace that with `åäö` and vice versa. So for example if I typed `hall(` I would press `SWE` to get `hallå`, with the Swedish layer activated.

| Split combos  | Result
| :----         | :----
| `S` + `H`     | `"`
| `N` + `I`     | `'`
| `V` + `D`     | `Alt` (one-shot)
| `L` + `)`     | [Leader key](#leader-sequences)
| `T` + `A`     | [CAPSWORD]
{ .center }

## Vertical combos

![Vertical combos. All except `;` are pressed with a single finger between the keys.](/images/t-34-curr/vcombos.png)

`vsplt` and `hsplit` splits windows in Vim, and there are also combos for closing a window and saving.

## Numbers and symbols

![Combos using `Space` and another key.](/images/t-34-2/space_combos.png)

![Combos using `E` and another key.](/images/t-34-2/e_combos.png)

Combos with a thumb key is used for digits or standalone symbols, with the logic of `same-side thumb` + `key` = `symbol` and `opposite-side thumb` + `key` = `digit`. The placements follow the [numbers], [symbols] and [Swedish](#swedish-overlay) layers. Both thumbs activates [NUMWORD].

# Long press

Most keys have a different behaviour when tapped compared to a long press. Most commonly I use this to produce shifted keys (called [auto shift][]). So to get `A` I press and hold `a` until it turns up.

There are a bunch of special cases as well (mostly on top of [combos]):

| Tap                               |  Long press
| :----                             |  :----------
| `_` `<` `>` `/` `\` `#`           |  Double, e.g `__`
| `"` `'` `=` `` ` `` `0` `.`       |  Triple, e.g `"""`
| <code>\|</code> `&` `=`           |  Double with spaces, e.g <code> \|\| </code>
| `!`                               |  ` != ` (with spaces)
| `?`                               |  `{:?}`
| `#`                               |  `{:#?}`
| `%`                               |  `%{}`
| `(` `[` `{`                       |  Close and move cursor between
| `@`                               |  `@u` (paired with `qu` combo for Vim macro execution)
{ .center }

# Leader sequences

I use the combo `l` + `)` as the [leader key]. This will wait for a sequence of key presses (in contrast to combos where keys must be pressed at the same time). This is used with mnemonics for rarely used outputs:

| Leader sequence       |  Action
| ---------             |  ----------
| `l` + `)`, `c`        |  **C**aps lock
| `l` + `)`, `t`, `n`   |  **T**oggle **N**umber layer
| `l` + `)`, `t`, `s`   |  **T**oggle **S**ymbols layer
| `l` + `)`, `t`, `c`   |  **T**oggle **C**aps lock escape swap
{ .center }

# CAPSWORD

CAPSWORD is a "smart caps lock". It works like a regular caps lock, except it automatically turns off after certain keys are typed (most commonly space).

It will not turn off on these keys: `a-z` `å` `ä` `ö` `_` `-` `Backspace` and `Repeat`.

# NUMWORD

NUMWORD is a "smart layer". It's similar to [CAPSWORD], except it's for the [numbers layer](#numbers) instead of caps lock.

It will not turn off on these keys: `0-9` `%` `+` `*` `-` `_` `.` `,` `:` `=` `x` `Backspace` `Repeat` `Reverse Repeat` and `Enter`.

# Repeat key

The repeat key simply repeats the previous key. So to type `fall` I can type `f` `a` `l` `Repeat`, using four different fingers instead of pressing `l` twice. It can also repeat things like `Ctrl-c` or `Delete`.

There's also a reverse repeat key that "reverses" the last pressed key. The idea is that if you pressed `PageUp` a bunch, but went too far, you could press `Reverse Repeat` to output `PageDown`.

See [T-34/0][] for the introduction of the [repeat key][rep-writeup] and [reverse repeat key][rev-rep-writeup] for some more information about them.

# More info

While I try to keep this post updated, [reading the code][src] will always give you a more up to date reference. If you're interested in *why* the layout looks like it does, I try to write the motivations in the [T-34 series][t-34-series].

[leader key]: https://docs.qmk.fm/#/feature_leader_key "QMK leader key"
[auto shift]: https://docs.qmk.fm/#/feature_auto_shift "QMK auto shift"
[qmk-combos]: https://docs.qmk.fm/#/feature_combo "QMK combos"
[QMK]: https://docs.qmk.fm/ "QMK"
[Layers]: https://docs.qmk.fm/#/feature_layers "QMK layers"
[Ferris]: https://github.com/pierrechevalier83/ferris "The Ferris keyboard"
[xmonad]: https://xmonad.org/ "Xmonad"
[dead keys]: https://en.wikipedia.org/wiki/Dead_key "Dead keys"

[T-34/0]:  /blog/2021/09/05/t-34-0/ "The T-34/0 keyboard layout"
[t-34-series]: /series/t-34/ "The T-34 keyboard layout"
[t-34]: /blog/2021/06/03/the-t-34-keyboard-layout/ "T-34 first post"
[num-sym-combos]: #numbers-and-symbols "Numbers and symbols"
[rev-rep-writeup]: /blog/2021/09/05/t-34-0/#reverse-repeat "Reverse repeat key"
[rep-writeup]: /blog/2021/09/05/t-34-0/#the-repeat-key "Repeat key"
[src]: https://github.com/treeman/qmk_firmware/tree/master/keyboards/ferris/keymaps/treeman "QMK source code"
