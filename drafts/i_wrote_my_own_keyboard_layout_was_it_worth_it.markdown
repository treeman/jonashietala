---
title: "I wrote my own keyboard layout. Was it worth it?"
tags: [Computer, Keyboards, T-34]
series: t-34
---

Almost two and a half years ago I embarked on the journey to change keyboard layout.
At first I tried out existing ones, but it didn't take long before I figured it's better to develop my own---and things went downhill fast from there.

![(Some) horizontal combos of the [T-34 layout][t-34]. It doesn't look complicated at all.](/images/t-34-curr/hcombos.png)

But now that I haven't made any changes to the layout for over a year, I think it's time for some introspection and ask the dreaded question: was it worth it?
And should you---the dear reader---do the same?

# What was my goal in creating my own?

I essentially had two big goals with my layout:

1. Relieve my RSI

   I had started to develop RSI in my right hand that I really wanted to cure.
   Especially in the area of my right thumb, I wanted to relieve my right hand in general.

   I also wanted to reduce pinky usage---particularly for my right pinky that I had broken earlier and felt very weak.

1. Optimized for my usage

   I had three main use-cases I wanted to optimize for:

   - Vim usage
   - Write Swedish and English
   - Coding in a myriad of different programming languages

Did I succeed with the goals?

For the first I can say that in combination with the [Ferris][]---a split keyboard with 34 keys---my RSI did indeed get a lot better.

![The [Ferris][], my keyboard that helps with RSI.](/images/uses/ferris.jpg)

As for the second---if it's optimized for my usage---I don't really know how to answer.
When I was in the middle of designing the layout I probably would've pointed out the low SFB values (Same Finger Bigram), the redirects and [generated interesting graphs][symbols] and [heatmaps][].

![](/images/t-34-crazy.jpg)

But now I'm satisfied with saying that it just *feels really good*.

[Ferris]: https://github.com/pierrechevalier83/ferris

# You have to be a little crazy

I do think you have to be a little crazy to design your own layout. I know I was.

---

There I was; awake in the middle of the night carrying our baby in a harness, standing in front of the computer trying to learn this new stinking layout while ignoring his cute smile, hoping he would fall asleep soon.

I was trying to learn the [BEAKL 15][] layout but it just wasn't working for me.
Maybe I could tweak it a little---so I did.
Maybe I could tweak it some more? I did that too.

Eventually the sleep deprivation took over and I threw that away and started to design my own layout.

And that's how I mostly designed my layout: I designed, tweaked and learned it in the middle of the night as I was trying to get our little kid to sleep.

---

I don't actually think you need to be actually crazy to design a keyboard layout---but it probably helps.

# What could I have done differently?

I'm not motivated to make any large changes or experiments with the layout now, but there are some things I wish I had tried out more:

1. One-shot shift on the thumb instead of `E`.

   The one-shot shift seems like such a fantastic feature, but that would require me to move `E` away from the thumb which would snowball into an entirely different layout.

1. [Home-row mods][].

   [Home-row mods][] is another very popular feature that would unlock a lot of extra space on the keyboard as it would allow secondary effects on long-press instead of shifting.

   I did try it out a little bit, but maybe I could get used to it with more effort?

1. 32 keys instead of 34.

   It would be fairly simple to go down to 32 keys, with only a single key for each thumb.
   The idea appeals to me, but I don't think there are any practical benefits for me to do so.

1. More practice.

   I'm not close to my old QWERTY speed of +120 wpm simply because I got bored of practicing typing.

# Would I recommend you to create your own layout?

Let's summon [Betteridge's law of headlines][] that says:

> Any headline that ends in a question mark can be answered by the word *no*.

I wouldn't go quite that far, but you need to realize that there's very real diminishing returns of completely designing your own layout.

In the *vast* majority of cases it would be good enough to switch to something like [Colemak-DH][] or [Hands Down][].
And depending on what you're after, switching to a more ergonomical keyboard would probably suffice.

Or maybe not do anything at all. Even though QWERTY is a really bad layout, it probably doesn't matter in the grand scheme of things.

## When should I make a change?

I think it comes down to two things:

1. What benefits are you looking for?
2. How motivated are you?

First an important note: changing the layout because you want to type faster will probably not work out.
You've probably been using QWERTY for many years and the amount of practice you need to catch up to and surpass your QWERTY speed will be *staggering*.

The biggest benefit I see with an alternative layout is comfort.
If you're worried about RSI, and you foresee yourself spending a few decades more in front of the computer, then switching layout might be a good idea.

Another benefit is if you have some special requirements with your layout.
In my case for example I wanted to comfortably be able to type Swedish on a tiny keyboard, and just using Colemak-DH or something wouldn't really support that well.
But with my own layout I could.

And of course, if you find the idea of designing your own layout interesting or fun, you should totally do that.
I honestly thought it was really **fun** (even though learning it was mostly a pain).

## A layout is more than alphanumerics

![Symbols are very important for a programmer.](/images/t-34-2/sym.png)

Something that people seem to miss with alternative layouts---even people designing them---is that a layout is much more than just where the alpha characters goes.

I've spent much more time on where to [place the symbols][symbols], how to [handle numbers][numbers], what type of [modifiers][] I want, a [navigation layer][] and [shortcuts][] than the base layer.

And that is where I think you can extract the most value; because the symbols you use will be much more dependent on the programming languages you use it's more worth to optimize them than to try to improve [Colemak-DH][], which is already pretty darn good.

# My recommendation

If you want to make a change to your setup, these are my recommendations sorted from least to most effort:

1. Get a programmable keyboard (so you can remap things on the keyboard instead of the OS).
1. Get a more ergonomical keyboard.

   I think a split keyboard with tenting is a great start.
   If you want to go the extra length try to make it smaller.

1. Tweak the big buttons to avoid large and awkward motions.

   For a Vim user, remapping Escape is a classic.
   Ctrl, Shift, Alt, and Enter are also good candidates for moving to a thumb button, a [combo][] or a [Mod-Tap][].
1. Add a [navigation layer][].
1. Use an alternative layout such as [Colemak-DH][], [Hands Down][] or [MTGAP 2.0][].
1. Start tweaking the symbols and numbers (and other things you can come up with).
1. Make your own completely custom layout.

You don't have to do everything at once and you can try out the different levels to see how painful and time consuming the changes are.
Keep in mind that diminishing returns are real.

# How do you even learn a new layout?

![The [T-34][t-34] base layer.](/images/t-34-2/base.png)

Say that you've decided to learn a new layout. Now what?

The simple answer is that you just need to practice.
But here are some tips to make the process better:

- Don't look at a reference of the layout.

  Being forced to remember is painful but recall helps you learn faster.

- Use blank keycaps.

- Practice in small bursts.

  Small and frequent sessions is better than few but larger ones.

- Learn to touch type.

  Might as well learn it properly from the start.

- If you want to retain the ability to type QWERTY, use a different setup.

  Use a different keyboard for a different layout can help keep them separate.
  Pressing space with a different thumb is another trick that may help.

There are very good online tools to help you learn any layout.
These are some I used:

- [keybr][]: Great for learning the initial characters.
- [Monkeytype][]: Great when you sort-of know where the characters are.
- [ngram-type][]: Practice the common ngrams. Great site, although I didn't find it interesting enough to use as much as I maybe should have.
- [typelit][]: Practice by retyping entire novels. Awesome idea, although I didn't manage to get through the Count of Monte Cristo as I planned to.

It's important to stress how much motivation matters.
Even though I feel l...

![My MonkeyType stats. Note that it includes different practice settings, like numbers, symbols and programming languages.](/images/monkey_stats.png)

![My practice died off hard a year ago.](/images/monkey_sessions.png)

# FAQ

- **You dodged the question; Was it worth it?**

  Hell yes it was.
  The massive amount of nerd points alone is enough.

- **I still want to use QWERTY on a regular keyboard**

  If you just use it regularly you'll be fine.
  I can still type faster with QWERTY on my laptop than with my own layout on my ergonomical keyboard.

- **How long does it take to learn a new layout?**

  Impossible to say as we're all different.
  The first layout I learned took ~16 hours of practice time on [keybr][] until I got up to ~40 wpm and with the second layout it took ~12 hours.
  At ~40 wpm I felt I could write without wanting to throw the keyboard through the monitor.

  Beyond that it depends on how diligent you are with practice.

[t-34]: /blog/2022/09/06/the_current_t-34_keyboard_layout/
[symbols]: /blog/2021/06/03/the-t-34-keyboard-layout/#symbols
[heatmaps]: /blog/2022/08/28/the_t-342_keyboard_layout/#more-heatmaps
[Betteridge's law of headlines]: https://en.wikipedia.org/wiki/Betteridge%27s_law_of_headlines
[BEAKL 15]: https://deskthority.net/wiki/BEAKL#BEAKL_15 "BEAKL 15 keyboard layout"
[Colemak-DH]: https://colemakmods.github.io/mod-dh/ "Colemak-DH keyboard layout"
[Hands Down]: https://sites.google.com/alanreiser.com/handsdown "Hands Down keyboard layout"
[combo]: https://docs.qmk.fm/#/feature_combo
[Mod-Tap]: https://docs.qmk.fm/#/mod_tap
[MTGAP 2.0]: https://mathematicalmulticore.wordpress.com/2010/06/21/mtgaps-keyboard-layout-2-0/ "MTGAP 2.0 keyboard layout"
[keybr]: https://www.keybr.com/
[ngram-type]: https://ranelpadon.github.io/ngram-type/
[typelit]: https://www.typelit.io/
[MonkeyType]: https://monkeytype.com/
