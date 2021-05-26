---
title: "The T-34 keyboard layout"
tags: Computer, Keyboards
---

A couple of months ago I started looking into ergonomical keyboards; mainly split keyboards with much fewer keys, like the [36-key Gergoplex][GergoPlex] and the [44-key Kyria][Kyria]. I may write another post about the reasons why, but long story short I started getting pain in my thumbs, fingers, wrist and forearm, and I thought it was time to do something about it.

Because my new shiny layout has been unchanged for more than a week, I've clearly found my Ultimate Layout™ and it's time to immortalize it with a blog post!

![T-34/57 prototype](/images/t-34/T-3457-prototype-side.png)


# The journey is long and full of peril

When you start looking at it, there are **tons** of good and interesting layouts. For example [Dvorak][], [Workman][], [Colemak Mod-DH][], [Hands Down][], [MTGAP 2.0][], BEAKL (many variants) and many others...

They all have their pros and cons, and which one you prefer is subjective.  In the end I chose [BEAKL 15][], mostly because I liked the discussion on the now defunct BEAKL forums, and I had some strain on my right pinky that I wanted to minimize.

Although the layout felt much better than QWERTY, after trying out a bunch of modifications I've since moved away from BEAKL. At around 50 WPM I got increasingly annoyed at the high same finger usage (called SFU). At the same time I found that I wanted to use my pinkies more than BEAKL was designed for, which could be used to address the high SFU.

The breaking point came when I read an article about the benefits of [E on one of the thumb keys][e-thumb], which made me abandon BEAKL and try something else.

on favor of [RSTHD][] (but naturally with a bunch of tweaks).


# My preferences


# Frequency analysis

* Source code
* Keylog
* Results


# The layout

Features:

- Lot's of combos
- Bunch of layers
- One-shot non-base home-row mods 
- Auto-shift
- CAPSWORD (smart caps lock)
- NUMWORD (smart num layer)
- Long press for different symbols

![Legend](/images/kyria/legend.png)

## Base layer

The base layer is based on [RSTHD](https://xsznix.wordpress.com/2016/05/16/introducing-the-rsthd-layout/), which seemed like a well optimized layout with E on the thumb.

![Base layer](/images/kyria/base.png)

As I've run into annoyances I've made some changes to it. Most notably:

- E on other thumb. Feels much better to avoid having "here", "there" and similar combinations on the same hand.
- Q and Z moved away to combos in favor of more symbols.
- Swap D and P (as I dislike the center column and lower index is good).
- K is in a better spot as it's much more common in Swedish, and I use it a lot with Vim. It makes CK very nice to type.
- Swap L and W because L is more common (`W` is so common in the keylog because I spam save `:w` like a madman).

## Combos

A combo (sometimes called a chord) is when you press down two keys at the same time to produce something else. For instance if I press C and K at the same time I get Q. With the light choc switches I use this feels very nice, even pressing three keys or vertical combos with two keys using one finger.

I use it for lots of things. For instance:

- Hide the least common chars `Q` and `Z`.
- "Big keys" on home-row: `Tab`, `Enter` and `Escape` (perfect for Vim!).
- Delete things: `Delete` and `Backspace`.
- Symbols that are mostly stand-alone, like `;`, `'` and `;`.
- Special things, like saving in Vim (`:wq<br>`).

Here's a table (because I wasn't happy with the visualization I tried to make):

Top Left            Key             Top Right           Key
----------          -----           ------------        --------
C + K               Q               X + W               #{ ↓ }
  K + F             Z                   , + U             Backspace
C + K + F           Delete            W + , + U           Ctrl + W (backspace word)
**Home Left**       **Key**         **Home Right**      **Key**
  T + H             Escape          N + A               :
S + T               Tab               A + I             Enter
S +   H             "               N +   I             '
S + T + H           ;               N + A + I           Vim save
**Bottom Left**     **Key**         **Bottom Right**    **Key**
V + G               Vim vsplit      L + (               ^
                                      ( + )             $
V + G + P           Vim close       L + ( + )           Swedish layer

The rationale here is that home-row combos are very easy to type, so the common keys like `Enter` and `:` go there.

Having related functionality close to each other makes it a little easier to learn, so `^` (goto first non-space char in line) and `$` (goto last char in line) pairs nicely. They, and the other combo-able symbols, are mostly stand-alone so there's minimal switching between layers to type them.

There are also vertical combos with the common arrow combinations you often see in programming:

![Vertical symbol combos](/images/kyria/sym-combo.png)

Depending on the language, symbols like `>=`, `=>`, `|>` and `->` are common, but often very difficult to type. Combos solve this really well, and my fingers no longer howl with pain having to type these things all over the place.

Also, splitting windows in vim is something I do a lot. Horizontal/vertical splits are laid out to match the split direction, and it's close to closing a window as well. (Why is saving vim on the right side then? Because `);` is very common. It's not perfect I know.)


## What about shift?

Where to place shift was one of the most difficult decisions for me. I considered these options:

1. [One-shot shift][one-shot], where you press and release shift and the next letter will be shifted, is great. But it doesn't vibe well with E on the thumb (and no outer column).
2. [Home-row mods][home-row], where you press and hold a regular key to turn it into shift. Many people love it, but I found it difficult to coordinate between left/right (as you want to hold with the opposite hand).
3. [Auto Shift][], where you just do a long press to get an uppercase letter. It's convenient for single letters, but many people who have tried it says it messed up their rhythm.

I wanted to use one-shot shift, but I just couldn't get it to work well with E on the thumb. I got the fiddly home-row config to work, and I think I could learn to live with it, but for me auto shift felt better.

Sure, it's harder to type quickly with auto shift, but I want to maximize comfort over speed, and auto shift feels like the option requiring the least amount of effort. I do have one-shot shift keys, but on a separate layer, which I use for some shortcuts (more on that shortly).

Typing multiple uppercase letters in a row does suck. That's why I also use "CAPSWORD", which is a smart caps lock that turns itself off after space or some other special characters. It makes it super easy to type variables like `POST_LIMIT` for example. I activate it with a `T` + `A` combo (left + right shift).


## Mods & symbols


# Is this the perfect layout?

Hah, that's funny.

There are always things that could be done better. Here are some things that currently annoy me:

- As I press the outer top keys (Y and ,) with my ring finger, SYS is terrible to type as it's three letters in a row with the same finger.
- BJ is crazy annoying. It gets worse if I program in a codebase that uses `Object` all over the place.

And of course you can always further optimize some of my choices. But at the moment it feels pretty good, and at this point I'm feeling diminishing returns.

[GergoPlex]: https://www.gboards.ca/product/gergoplex "GergoPlex"
[Kyria]: https://splitkb.com/products/kyria-pcb-kit "Kyria PCB Kit"
[Colemak Mod-DH]: https://colemakmods.github.io/mod-dh/
[MTGAP 2.0]: https://mathematicalmulticore.wordpress.com/2010/06/21/mtgaps-keyboard-layout-2-0/
[Hands Down]: https://sites.google.com/alanreiser.com/handsdown
[Dvorak]: https://en.wikipedia.org/wiki/Dvorak_keyboard_layout
[Workman]: https://workmanlayout.org/
[BEAKL 15]: https://deskthority.net/wiki/BEAKL#BEAKL_15
[e-thumb]: https://precondition.github.io/pressing-e-with-the-thumb
[beakl wi]: http://thedarnedestthing.com/beakl%20wi
[RSTHD]: https://xsznix.wordpress.com/2016/05/16/introducing-the-rsthd-layout/
[daily beakl]: http://thedarnedestthing.com/daily%20beakl
[Auto Shift]: https://docs.qmk.fm/#/feature_auto_shift
[home-row]: https://precondition.github.io/home-row-mods
[one-shot]: https://docs.qmk.fm/#/one_shot_keys
