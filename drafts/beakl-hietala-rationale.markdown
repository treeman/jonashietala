---
title: "Rationale behind Beakl-Hietala"
tags: Computer, Keyboards
---

This is my reasoning behind the Beakl-Hietala 0.1 layout, and why it looks the way it does. If you just want to see the layout itself, see this other post.


# Initial considerations

This layout is specifically created to use with the Gergoplex, which has some unique features compared to other keyboards:

1. 36 keys, with 6 thumb keys
2. Low resistance switches and flat keycaps, making combos by pressing between two keys comfortable

And I have some personal considerations too:

1. I have preexisting pain in my right thumb and pinky.
2. I hate the Ergodox thumb clusters, and I don't want to do too much with my thumbs.
3. I hate using the pinky fingers.
4. I'll choose a "better layout" even if it's harder to learn. If I'm gonna switch layout then I'll just as well go all the way.


## Effort values

I like the analysis of both [Colemak Mod-DH][colemak-compare] and [Beakl][beakl-effort], and I find both of their effort grids fit well for me. This is the effort grid I use as a proxy:

![Effort grid](/images/beakl-hietala/diff.png)

![New effort grid](/images/beakl-hietala/effort.png)

It's important to remember that this is only ever an approximation, but the main ideas are these:

1. Prioritize the 3-by-3 square in the middle
2. Heavily punish pinky fingers
3. Middle rows are also not good
4. The third thumb button is bad

[colemak-compare]: https://colemakmods.github.io/mod-dh/compare.html
[beakl-effort]: https://ieants.cc/code/keyboard/beakl/beakl.php


# Key frequency analysis

Tracking character frequencies is difficult for a programmer because they can differ a lot depending on what programming language you use. I tend to use different languages, so optimizing for a single language seems wrong. I also do write a fair bit, so the layout should be optimized for writing English to a large extent.

Regardless I made an attempt to analyze some of my code and also did some keylogging to see if I could find something interesting.


## Code samples

I collected the files [for my book][whycrypto], this blog and a bunch of my personal projects. The file type ended up looking like this:

```
   Lines per file type
C++         49218 52.66%
Markdown    12165 13.02%
Pollen      10776 11.53%
Rust         9302 9.95%
Perl         3252 3.48%
Elixir       2091 2.24%
Sass/CSS     2036 2.18%
Python       1808 1.93%
Racket       1672 1.79%
Haskell       667 0.71%
Java          244 0.26%
Latex         219 0.23%
Bash            6 0.01%

Total       93456
```

I'm surprised that C++ is so prevalent; I haven't used it for a personal project since my University days. I did include the source for [my solutions to UVa][uHunt] and some of my game projects, which I guess were larger than I remembered.

[uHunt]: https://uhunt.onlinejudge.org/id/115705

I tried to look at digram frequencies, to see if there are any specific symbol pairs that show up often:

```
                  Symbol digrams (top 90)
); 10961    ), 1675     "] 695      [# 419      .# 315      ". 206
// 6051     ]{ 1574     && 660      ($ 403      */ 315      `( 204
() 5982     :/ 1478     ** 652      ." 401      (! 308      ?; 199
:: 4905     "% 1134     ]( 592      += 367      {} 307      %{ 196
-- 4417     ]; 1044     }; 586      >= 366      ') 272      ]] 190
)) 4376     ]) 973      != 575      =" 365      "> 257      {. 181
?? 3370     ], 902      ]: 560      "\ 364      || 252      }) 177
== 2904     `` 875      [" 556      }, 352      ', 252      !" 176
", 2898     >> 866      ## 512      #. 351      /" 250      }" 170
") 2688     ]. 813      !( 486      ![ 336      )] 239      /) 163
(" 2541     #: 812      "" 466      /* 335      }. 234      :* 156
][ 2320     ). 789      <= 452      "; 330      )? 229      '; 155
++ 2171     (& 740      << 444      (/ 329      (: 227      #[ 151
.. 1843     [] 730      (( 427      >( 319      &[ 219      `{ 149
-> 1678     => 697      </ 425      ~~ 315      (_ 207      (' 144
```

There are some observations we can make:

1. I've always been bothered by combining the greater than and lesser than symbols with others. Like typing `>=`, `<=` and `=>`, but they aren't *that* common. In contrast `->` is common, probably due to the over-representation of C++ (pointer dereference). If I had written more Elxir `|>` and `<|` would've been more common.

2. Parens `()` and square brackets `[]` combos with a ton of different things.

3. Square brackets `{}` are more standalone. `]{` is so common because of the prevalent use of the Pollen syntax `◊def[arg]{text}`{.pollen}.

But in the end I don't think they affect our layout too much.


## Keylog samples

I also ran a keylogger when I did some coding sessions (mostly with Elixir) and did some random web browsing (so things like switching workspace and arrow key scrolling was common). I'm also a heavy Vim user, so some keys are bound to be more common.

The sample size was much smaller than the code sample, to get a more accurate prediction I'd have to run it a lot longer and use other kinds of programming languages more.


## Frequencies

![Key frequencies from code analysis, excluding space](/images/beakl-hietala/single-freq.svg)


![Key frequencies from keylog session. Alt didn't register for some reason.](/images/beakl-hietala/all-freq-keylog.svg)

![Letter frequencies](/images/beakl-hietala/combined-letter-freq.svg)

![All letter frequencies](/images/keyboard/combined-se-en-letter-freq.svg)

While there were some letters with significant differences between the code and keylog samples, such as `u`, `w` and `j`, in general letters are more common than both digits and symbols. Therefore it's fine to choose an existing layout that's optimized for english, which is what I write almost exclusively.

![Special keys from keylog session](/images/beakl-hietala/spec-freq-keylog.svg)

It's no large surprise that space and enter are the most common keys together with the modifier keys (I use the meta key for switching workspaces in XMonad). Escape is common because I use it all the time in Vim and the page up, page down and arrow keys are just for scrolling in the browser.

![Digit frequencies](/images/beakl-hietala/combined-digit-freq.svg)

`0`, `1` and `2` are by far the most common digits. I think `2` is overrepresented in the keylog because of my workspace setup where I use workspace number 2 a lot. This means that these digits should go on the home row, but the others don't really matter where we place them.

![Symbol frequencies](/images/beakl-hietala/combined-symbol-freq.svg)

Symbols are the hardest to place and also the most subjective. Just look at the difference between the code sample and the keylog. While there are commonalities some symbols stick out a lot.

1. `:` is extremely overrepresented in the keylog, much more than I would've thought. I do use ex-commands like `:w` in Vim a bunch, but I also mainly programmed in Elixir during the keylog, and there you can find code that make very heavy use of colons:

    ```{.elixir}
    form = %{artist_name: :string, album_title: :string,
        artist_birth_date: :date, album_release_date: :date,
        genre: :string}
    ```

   If I had used another language like C++ `;` would've been much more common, while in Elixir you barely use if at all.

2. Brackets `()`, `[]`, `{}` and `<>` are generally very common, again depending on the chosen language.

3. The symbols `.`, `/`, `,` and `"` have predictably high usage, as they're common in both code and heavily used in Vim. I'll also add `'` to this mix.

4. `$` is an odd one. It sees barely any usage in the code (maybe because I didn't use Perl or PHP very much?) but it's very used in the keylog, even though it's not as prevalent in Elixir. I guess I use Vim's "goto-end-of-line" command (the `$` symbol) a whole lot more than I thought.

5. `-` and `_` are also high up, and they're most often used in variable names like `albums_by_artists` or `inflation-bug`.


# Beakl-Hietala 0.1

Pulling it all together, this is the layout I came up with. (Keep in mind it's bound to change the more I use it.)

![Legend. Does not apply to combos.](/images/beakl-hietala/legend.png)

I use keys with a dual purpose, when they're clicked they act like a normal key but if held down they acts like a modifier or a layer switch that gives access to more keys. Other keys are "one-shot" keys that you press once, and then the next key you press is from another layer and then it goes back to normal.

The base layer uses BEAKL 15, with four of the most common symbols easily accessible as single keys.

![Base layer](/images/beakl-hietala/base.png)

The `NUM` and `SYM` keys can either be held down or clicked for one-shot access to numbers or symbols, making them fast and easily accessible.

As for the symbol placement this is the layout priority, ordered from the most easily accessed to the most difficult:

1. Single key on the base layer.
2. Combo keys on the base layer (press between two nearby keys simultaneously).
3. On the symbols layer.
4. On the outer thumb keys.
5. On the base layer, but shifted. (I did not use these as there were enough keys already.)

Therefore I assigned the thumb keys to `%` and `=`... (change??)

The different types of brackets and the other common symbols goes on the combo layer, so they're always directly accessible without any modifier:

![Symbol combos on the base layer](/images/beakl-hietala/sym-combo.png)

And the rest of the symbols goes on the dedicated symbols layer:

![Symbols layer](/images/beakl-hietala/sym.png)

Note that there are no symbols on the same side of the thumb that activates the layer to avoid having to stretch out the hand. This is something I try to follow as much as possible.

Numbers work great in a numpad layout, but I place the common digits 0--2 directly on the easiest keys on the home row:

![Number layer](/images/beakl-hietala/num.png)

To ease the learning process I follow this theme with function keys (that I almost never use):

![Function layer](/images/beakl-hietala/fun.png)

Because I also write Swedish I have a layer for special characters (and other keys):

![Special characters](/images/beakl-hietala/spec.png)

In here if you press the one-shot key `´` and then `e` creates the `é` character. To reduce the number of layers there are also some settings here that you can access by double-tap, to avoid accidental triggers.

Because I often use programs with the laft hand on the keyboard and the right on the mouse/trackball I want access to common shortcuts, so there's a shortcut layer:

![Shortcuts and rightside cursors](/images/beakl-hietala/short.png)

In it I also have replicated Vim's movements with hjkl (but with movement arrows, they work the same).

And another movement/mouse layer:

![Mouse and movements](/images/beakl-hietala/mov.png)

This one I can make persistent, so I don't have to hold down the modifier all the time, by tapping the `MOVE BASE` key.

