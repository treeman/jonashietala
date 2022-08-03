---
title: "The current T-34 keyboard layout"
tags: [Computer, Keyboards, T-34]
---

To avoid having to go through different blog posts and the source code of my keyboard layout to piece together how it actually looks right now, I try to collect it all in this post.

If you want to read about the background and philosophy about the layout, please see the [original T-34 post][], and to see it's evolution please refer to the [posts tagged T-34][].

# Legend

![Legend](/images/t-34-2/legend.png)

# Other things

- Combos
- Repeat + Reverse repeat
- CAPSWORD
- NUMWORD

# Layers

## Base

![Base](/images/t-34-2/base.png)

CAPSWORD

## Swedish overlay

![Swedish overlay](/images/t-34/swe.png)

Also combos.

## Navigation layer

![Navigation layer.](/images/t-34-2/nav.png)

## Workspace layer

![Workspace layer. All keys have an implicit `Gui` modifier.](/images/t-34-2/wnav.png)

## Numbers

![Numbers](/images/t-34-2/num.png)

NUMWORD.

Combos with cross side thumb. So `E` + left-hand key or `Space` + right-hand key = number.

## Function keys

![Function keys](/images/t-34-2/fun.png)

## Symbols

![Symbols](/images/t-34-2/sym.png)

Combos with same side thumb. So `Space` + left-hand key or `E` + right-hand key = symbol.

## One-handed Shortcuts

![Shortcuts](/images/t-34-2/shrt.png)

## Special symbols

![Specials (with ←↓↑→ symbols, not arrow keys)](/images/t-34-2/spec.png)

## Modifiers

![Modifiers](/images/t-34-2/mods.png)


# Combos

![Horizontal combos with nearby keys.](/images/t-34-2/hcombos.png)

![Vertical combos. All except `;` are pressed with a single finger between the keys, using fairly flat choc keycaps.](/images/t-34-2/vcombos.png)

| Other combos  | Result
| :----         | :----
| `S` + `H`     | `"`
| `N` + `I`     | `'`
| `T` + `A`     | [CAPSWORD][]
| `L` + `)`     | [Leader key](#leader-sequences)
| `Space` + `N` + `A` | `Ctrl` + `Gui` + `Space`
{ .center }

![Combos using space and one other key.  
The yellow keys are the same as the NUM or SYM layer.](/images/t-34-2/space_combos.png)

![Combos using E and one other key.  
The yellow keys are the same as the NUM or SYM layer.](/images/t-34-2/e_combos.png)

The logic here is same-side thumb + key = symbol and opposite-side thumb + key = digit (marked in yellow). Both thumb activates [NUMWORD][].

# Long press

Most keys have a different behaviour when tapped compared to a long press. Most commonly I use this to produce shifted keys (called [auto shift]). So to get `A` I press and hold `a` until it turns up.

But there are a bunch of special cases as well (including on combos):

| Tap       | Hold              | &ThinSpace; 
| :----     | :----             | :----
| `>`       | `>>`
| `<`       | `<<`
| `_`       | `__`
| `"`       | `"""`
| `.`       | `...`
| `0`       | `000`
| <code>`</code>   | <code>```</code>
| `@`       | `@u`              | Paired with `qu` combo for Vim macro execution
| <code>\|</code>       | <code> \|\| </code>           | Surrounded by spaces
| `&`       | ` && `
| `=`       | ` == `
| `!`       | ` != `
| `?`       | `{:?}`
| `#`       | `{:#?}`
| `%`       | `%{}`
| `(`       | `()←`             | Move cursor between
| `[`       | `[]←`
| `{`       | `{}←`
| `:q Enter` | `Shift` + `Gui` + `c`

Use this instead?

Tap                                           |  Long press
----                                          |  -----------
<code>\|</code> `&` `+` `*` `-` `_` `<` `>` `/` `\` `#`   |  Double symbol
`"` `'` `=` `` ` ``                           |  Triple symbol

# Leader sequences

I use the combo `l` + `)` as the [leader key].

| Leader sequence       |  Action
| ---------             |  ----------
| `l` + `)`, `c`        |  **C**aps lock
| `l` + `)`, `t`, `n`   |  **T**oggle **N**umber layer
| `l` + `)`, `t`, `s`   |  **T**oggle **S**umber layer
| `l` + `)`, `t`, `c`   |  **T**oggle **C**aps lock escape swap

[leader key]: https://docs.qmk.fm/#/feature_leader_key
[auto shift]: https://docs.qmk.fm/#/feature_auto_shift
