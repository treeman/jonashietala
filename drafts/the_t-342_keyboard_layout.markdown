---
title: "The T-34/2 keyboard layout"
tags: [Computer, Keyboards, T-34]
series: t-34
---

![The T-34/2 layout.  
Generated with the [QMK Heatmap Generator][heatmap-generator], with all layers. I logged my usage for a few weeks and this is the result.
](/images/t-34-2/all_freq.png)

It's been 6 months since my last update on the [T-34] keyboard layout, and I've made some changes I'd like to document here.

As seen in the heatmap above, the layout seems quite decent. There's still an issue with `j` and `k` being too prominent (because I'm a Vim jk-spammer). I don't have a good fix for it layout wise, I think I "just" need to get better Vim habits.

There's also a hand imbalance, but I don't feel it's an issue for me as I have problems with RSI on my right hand, so having the left hand do a little more work is even a positive.

![Legend](/images/t-34/legend.png)

# Swap AO and IU (again)

![Base](/images/t-34-2/base.png)

In my [last iteration][T-34/1] I had `iu` on my middle finger and `ao` on my ring finger. That was far from optimal as the ring finger is much stronger, and `ao` is much more common for me. It was just an oversight from me when I swapped `Repeat` to the home-row, but it has now been corrected.

I'll also say that I'm still not convinced that `Repeat` is worth it here. Avoiding double taps feels good, and repeating things like `Ctrl-c` is fantastic, but I could see that skipping `Repeat`, moving `o` to the home-row and making space for an extra key is preferable. (Maybe put `j` in a better spot...?)

# Reduce inner thumb key usage

Another big change I've made is to reduce the inner thumb key usage (where I fold my thumb to reach the `SHRT` and `SPEC` keys). This was a big issue for my right thumb where I'm suffering from RSI (I probably read too much manga on my mobile phone).

While [T-34/1] had symbols and mods on the thumbs (only used for multiples, as I have single combos for them all) I now only have rarely used keys under the layers there.

![Shortcuts](/images/t-34-2/shrt.png)

![Specials (with ←↓↑→ symbols, not arrow keys)](/images/t-34-2/spec.png)

The shortcuts are there to give me one-handed access to common shortcuts. There are some duplicates there as I could for instance do `Ctrl-c` either from the shortcut layer or do a `Ctrl` combo then `c`, but meh. I rarely if ever use the specials layer.

I've also reworked the combos to give access to the symbol layers and to add a one-shot `Ctrl` on home-row:

| Left Combo        | Tap       | Hold
| :---------        | :-        | :--
| `s` + `t`         | `Ctrl`    | `Ctrl`
|   `t` + `h`       | `Escape`  | `SYM`
| `s` +   `h`       | `"`       |
| `s` + `t` + `h`   | `Tab`     | `MODS`
| **Right Combo**   | **Tap**       | **Hold**
| `n` + `a`         | `:`       | `SYM`
|   `a` + `i`       | `Ctrl`    | `Ctrl`
| `n` +   `i`       | `'`       |
| `n` + `a` + `i`   | `Enter`   |
| `n` + `a` + `i` + `Repeat`   | Save Vim   |
| **Left mid/low Combo**    | **Tap**   | **Hold**
|   `t` + `d`       | `;`  |

- `Ctrl` is super common, so it should have prime real estate. I *vastly* prefer one-shot over keys I need to hold down.
- `SYM` being a hold is fine as I only use it to output multiple symbols anyway. It's almost exclusively used for `{}` and `[]`.
- Some combos now use 3 or even 4 keys. That's fine as they're "big" keys that I tend to slam a bit more, so in a weird way having some more resistance feels better in some cases...
- `;` is in a slightly weird place, but it's still fine IMO.

![MODS](/images/t-34-2/mods.png)

I don't really use the `MODS` layer, but it's necessary for those weird combinations or *gasp* `Alt` usage.

# Even easier ÅÄÖ

I still really like the Swedish overlay (replacing `()_` with `åäö`, but I've added some extra finess to it:

- When switching, if the previous key is one of `()_` then also backspace and replace it with `åäö` (and vice versa).

  I find that I'm often in the wrong layer, so if I type for instance `fooå`, I can just switch off the Swedish layer and I'll get `foo(`. In theory it's good, but it doesn't come super fluidly for me.

- I've added combos for the keys on the other layer.

  | Combo           | Result
  | ---------       | ------
  | `Space` + `(`   | `å`
  | `Space` + `)`   | `ä`
  | `Space` + `_`   | `ö`
  | `Space` + `å`   | `(`
  | `Space` + `ä`   | `)`
  | `Space` + `ö`   | `_`

  So if the Swedish layer is on and I want to type `(` I can use a combo, and if the Swedish layer is off and I just want to type a few Swedish words, I can use the same combo.

# Instant leader key

A [leader key][] is a function that triggers after a sequences of keys. So for instance I press `Leader`, then `t` and finally `n` to toggle the number layer. I use these sequences with the combo `l` + `)` as my leader key.

| Sequence              |  Action
| ---------             |  ----------
| `l` + `)`, `c`        |  **C**aps lock
| `l` + `)`, `t`, `n`   |  **T**oggle **N**umber layer
| `l` + `)`, `t`, `s`   |  **T**oggle **S**umber layer
| `l` + `)`, `t`, `c`   |  **T**oggle **C**aps lock escape swap

[leader key]: https://docs.qmk.fm/#/feature_leader_key
[userspace leader sequence]: https://github.com/andrewjrae/kyria-keymap#userspace-leader-sequences

# Other experiments

- Swap num/sym thumb combo locations
- <% %> <%= are hard to place...
    swap % and !
- I did not like `p` and `x` as combos, at least on `h` + `k` and `m` + `a`. They're fine where they are now.

# Future experiments

[T-34]: /blog/tags/t-34/ "T-34 tags"
[T-34/1]: /blog/2021/12/15/t-34-1/ "The T-34/1 keyboard layout"
[heatmap-generator]: https://precondition.github.io/qmk-heatmap#how-to-collect-the-required-data "QMK Heatmap Generator"
