---
title: "The T-34/2 keyboard layout"
tags: [Computer, Keyboards, T-34]
series: t-34
---

![The T-34/2 layout.  
Generated with the [QMK Heatmap Generator][heatmap-generator], with all layers and combos.
](/images/t-34-2/freq/all.png)


It's been 6 months since my last update on the [T-34] keyboard layout, and I've made some changes I'd like to document here.

As seen in the heatmap above, the layout seems quite decent. There's still an issue with `j` and `k` being too prominent (because I'm a Vim jk-spammer). I don't have a good fix for it layout wise---maybe a `j` and `m` swap---but mainly I think I "just" need to get better Vim habits.

There's also a hand imbalance, but I don't feel it's an issue for me as I have problems with RSI on my right hand, so having the left hand do a little more work is even a positive.

![Legend](/images/t-34-2/legend.png)

# Swap AO and IU (again)

![Base](/images/t-34-2/base.png)

In my [last iteration][T-34/1] I had `iu` on my middle finger and `ao` on my ring finger. That was far from optimal as the ring finger is stronger, and `ao` is much more common for me. It was just an oversight after I swapped `Repeat` to the home-row (from where `o` is now) but it has now been corrected.

I'll also say that I'm still not convinced that `Repeat` is worth it here. Avoiding double taps feels good, and repeating things like `Ctrl-c` is fantastic, but I could see that removing `Repeat`, moving `o` to the home-row and making space for an extra key is the better choice.

# Reduce inner thumb key usage

Another big change I've made is to reduce the inner thumb key usage (where I fold my thumb to reach the `SHRT` and `SPEC` keys). This was a big issue for my right thumb where I'm suffering from RSI (I probably read too much manga on my mobile phone).

## Shortcuts

While [T-34/1] had symbols and mods on the thumbs (only used for multiples, as I have single combos for them all) I now only have rarely used keys under the layers there:

![Shortcuts under the left thumb.](/images/t-34-2/shrt.png)

The shortcuts are there to give me one-handed access to common shortcuts. There are some duplicates there as I could for instance do `Ctrl-c` either from the shortcut layer or do a `Ctrl` combo then `c`, but meh. 

## Specials

![Specials under the right thumb (with ←↓↑→ symbols, not arrow keys).](/images/t-34-2/spec.png)

I'ts mostly used to add accents like `é` or `è`, but it's very rarely used.

## Home-row combos for ctrl and layer access

I've also reworked the combos to give access to the symbol layers and to add a one-shot `Ctrl` on home-row:

![Horizontal combos with nearby keys.](/images/t-34-2/hcombos.png)

- `Ctrl` is super common, so it should have prime real estate on the home-row. I *vastly* prefer one-shot over keys I need to hold down, and it's super comfortable to type things like `Ctrl-n` and `Ctrl-o`.
- `SYM` being a hold is fine as I only use it to output multiple symbols. It's almost exclusively used for `{}` and `[]`.
- Some combos now use 3 or even 4 keys. That's fine as they're "big" keys that I tend to slam a bit more, so in a weird way having some more resistance feels better in some cases...
- Not pictured above, `;` is triggered by `t` + `d` which is a bit weird, but it works.
- The modifier layer exists as an escape hatch, if I ever have to type weird modifiers like `Alt`:

![Modifiers](/images/t-34-2/mods.png)

## Symbols

Another consequence of the change is that the two symbol layers (that [used to][prev-sym-layers] have [callum style mods][callum] on the opposite side) are now combined into a single layer:

![Symbols](/images/t-34-2/sym.png)

A small change I did to the symbols layer is to swap `%` and `!`, because `%{}`, `<%` and `%>` are common sequences in Elixir.

I did try to have `%{}` as a long press on `%`, but for some reason I didn't really use it. Rolling feels more satisfying.

# Swap thumbs for number and symbol combos

Previously the easy access logic for numbers and symbols was:

| Combo                                 | Output
| --------                          | ------
| `left thumb` + `key`              | `symbol`
| `right thumb` + `key`              | `digit`

But despite struggling for many months, I still couldn't get used to it and I still made mistakes regularly. So now I've changed to this:

| Combo                                 | Output
| --------                          | ------
| `same side thumb` + `key`              | `symbol`
| `opposite thumb` + `key`              | `digit`

For some reason, this is easier for my brain.

# Even easier ÅÄÖ

I still really like the Swedish overlay (replacing `()_` with `åäö`), but I've added some extra finesse to it:

- When switching, if the previous key is one of `()_` then backspace and replace it with `åäö` (and vice versa).

  I find that I'm often in the wrong layer, so if I type for instance `fooå`, I can just switch off the Swedish layer and I'll get `foo(`. In theory it's good, but it doesn't come super fluidly for me yet.

- Combos with the opposite thumb now always output `åäö`, which is in line with the combo swap mentioned above.

  This means I always have access to `()_` and `åäö` with the same combo, regardless of what layer I'm on.

# Instant leader key

A [leader key][] is a function that triggers after a sequences of keys. So for instance I press `Leader`, then `t` and finally `n` to toggle the number layer. I use these sequences with the combo `l` + `)` as my leader key:

| Sequence              |  Action
| ---------             |  ----------
| `l` + `)`, `c`        |  **C**aps lock
| `l` + `)`, `t`, `n`   |  **T**oggle **N**umber layer
| `l` + `)`, `t`, `s`   |  **T**oggle **S**ymbols layer
| `l` + `)`, `t`, `c`   |  **T**oggle **C**aps lock escape swap

I don't use QMK's version, as I couldn't get used to the timeouts, but a [userspace implementation][userspace leader sequence] that resolves instantly.


# A failed experiment

As I find `p` and `x` to be the worst keys on the board (barring the folding thumb keys), I tried to have them as `h` + `k` and `m` + `a` combos...

But it felt so much worse. Maybe there's a way to modify the layout to remove those two keys, but this is not the way. And it's not something I care to explore further at the moment.

# More heatmaps

The [QMK Heatmap Generator][heatmap-generator] also provides heatmaps for the individual layers, which gives another way to analyze the keymap compared to an indiscriminate keylogger I've used before.

Note that I did not take these heatmaps into account making this version of the keymap.

![Base layer (this includes virtually all combos too).](/images/t-34-2/freq/base.png)

Can you feel the `j`/`k` abuse?

![Swedish layer (only ÅÄÖ are changed from base).](/images/t-34-2/freq/swe.png)

In contrast to the base layer, which is used for programming a lot, the Swedish layer is only used for writing normally, and I think it looks decent.

![Symbols layer.](/images/t-34-2/freq/sym.png)

Note that this is only used to write multiple symbols in a row, like `%{}` or `#[`, as single symbols are typed with thumb combos.

(And arrows like `->` and operations like `||` are typed in other ways, so there aren't that much usage for this layer.)

![Numbers layer.](/images/t-34-2/freq/num.png)

Note that this is only used together with [NUMWORD][], and single digits are typed with thumb combos---such as `0` that in Vim moves the cursor to the beginning of the line.

A typical usage for me is to jump around in Vim with relative line numbers, which explains the high frequencies of `j` and `k`:

![Activate [NUMWORD][], then `4j` would move the cursor 4 lines down to `.collect` and turn off the number layer.](/images/t-34-2/rel_vim.png)

![Navigation layer.](/images/t-34-2/freq/nav.png)

I switch windows in Vim with `Ctrl-Left` or `Ctrl-Right`, which is something I do all the time.

Maybe there's an argument for removing the arrows on the left, placing the tabs (switching tabs in Firefox) in a better position?

![Workspace nav layer.](/images/t-34-2/freq/wnav.png)

This is basically switching between workspaces `0` to `9` and the three monitors. It's weird how I don't use workspace `3`, although it should be in a good position finger placement wise.

There are some more layers, but I cut them out because the heatmaps were totally uninteresting to me.

As usual, the [firmware QMK code][code] is on GitHub.

[code]: https://github.com/treeman/qmk_firmware/tree/master/keyboards/ferris/keymaps/treeman "Source code for T-34"
[NUMWORD]: /blog/2021/06/03/the-t-34-keyboard-layout/#where-are-the-digits "Where are the digits?"
[leader key]: https://docs.qmk.fm/#/feature_leader_key "QMK leader key"
[prev-sym-layers]: /blog/2021/06/03/the-t-34-keyboard-layout/#mods-symbols "T-34 mods & symbols"
[userspace leader sequence]: https://github.com/andrewjrae/kyria-keymap#userspace-leader-sequences "Userspace leader sequences"
[T-34]: /blog/tags/t-34/ "T-34 tags"
[T-34/1]: /blog/2021/12/15/t-34-1/ "The T-34/1 keyboard layout"
[heatmap-generator]: https://precondition.github.io/qmk-heatmap#how-to-collect-the-required-data "QMK Heatmap Generator"
[callum]: https://github.com/callum-oakley/qmk_firmware/tree/master/users/callum#oneshot-modifiers "Callum Oakley keymap"
