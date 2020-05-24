---
title: "Deciding a keyboard layout for GergoPlex"
tags: Computer, Keyboards
---

# Initial considerations

1. Preexisting pain
2. I hate the Ergodox thumb clusters
3. Weak pinky fingers, especially the right one
4. I generally agree with [Colemak Mod-DH's][colemak-compare] effort values for keys:

   ![](/images/keyboard/keyboard_scores_matrix.png)

[colemak-compare]: https://colemakmods.github.io/mod-dh/compare.html
# Character frequencies

Tracking character frequencies is difficult for a programmer because they can differ a lot depending on what programming language you use. I tend to use different languages, so optimizing for a single language seems wrong. I also do write a fair bit, so the layout should be optimized for writing English to a large extent.

I collected the files [for my book][whycrypto], this blog and a bunch of my personal projects and counted the character frequencies:

[whycrypto]: https://github.com/treeman/why_cryptocurrencies "Source code to 'Why cryptocurrencies?'"

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

Either way this is the result:

```
    Symbols           Letters           Digits             Special
) 36514 1.21%     e 190550 6.33%    0 17956 0.60%   space  701166 23.30%
( 36495 1.21%     t 164168 5.46%    1 15860 0.53%   return 93456 3.11%
. 31678 1.05%     i 134630 4.47%    2 9613 0.32%
, 29088 0.97%     n 127105 4.22%    3 4152 0.14%
; 28465 0.95%     o 121345 4.03%    5 3321 0.11%
- 23546 0.78%     a 116737 3.88%    4 3321 0.11%
" 22835 0.76%     s 116420 3.87%    9 3157 0.10%
: 21660 0.72%     r 109968 3.65%    6 2696 0.09%
/ 20615 0.69%     l 75276 2.50%     8 2572 0.09%
= 18226 0.61%     d 68206 2.27%     7 2272 0.08%
_ 16924 0.56%     c 66960 2.23%
{ 13820 0.46%     u 55865 1.86%
} 13813 0.46%     h 51554 1.71%
[ 12740 0.42%     p 50907 1.69%
] 12736 0.42%     m 49062 1.63%
> 11419 0.38%     f 44048 1.46%
< 10419 0.35%     g 37598 1.25%
' 7778 0.26%      b 30503 1.01%
+ 7047 0.23%      y 28417 0.94%
# 6899 0.23%      w 26800 0.89%
? 5207 0.17%      v 19935 0.66%
* 5191 0.17%      k 17279 0.57%
& 5151 0.17%      x 14633 0.49%
% 3488 0.12%      j 6024 0.20%
! 2967 0.10%      q 3305 0.11%
` 2291 0.08%      z 3033 0.10%
$ 2197 0.07%      ö 96 0.00%
\ 2060 0.07%      ä 29 0.00%
| 1324 0.04%      å 20 0.00%
~ 681 0.02%
@ 604 0.02%
^ 150 0.00%

Total characters: 3009393
```

Add a diagram with letter frequencies for English, Swedish and my own data.

Add a diagram with letters, digits and common symbols.

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

It's difficult to draw big conclusions from this table, but I have some comments:

1.  Pairs like `--`, `//` and `**` come from comments, which I never really type as I use a plugin to generate them.

2. The large number of blog posts causes `--` and <code>\`\`</code> to be overrepresented.

3. I've always been bothered by combining the greater than and lesser than symbols with others. Like typing `>=`, `<=` and `=>`, but they aren't *that* common. In contrast `->` is common, probably due to the over-representation of C++ (pointer dereference). If I had written more Elxir `|>` and `<|` would've been more common.

4. Parens `()` and square brackets `[]` combos with a ton of different things.

5. Square brackets `{}` are more standalone. `]{` is so common because of the prevalent use of the Pollen syntax `◊def[arg]{text}`{.pollen}.

Another way of looking at the table is trying to track how many times a symbol is combined with other characters (since it's easy to double-click, but difficult to press different keys). Importantly I'd like to see if there are some symbols that are mostly used in conjunction with other symbols, or if they're mostly used next to letters or if they stand alone surrounded by spaces.

      Next to letters   Next to symbols (ignores itself)
    _     95.05%             2.35%
    '     73.98%            13.72%
    \     66.21%            26.33%
    [     66.13%            20.66%
    .     64.69%             7.15%
    (     58.30%            19.07%
    -     49.72%             5.59%
    <     46.32%             7.05%
    :     44.23%             9.56%
    $     40.08%            19.14%
    %     39.45%            28.08%
    @     37.42%            23.92%
    ]     34.53%            42.48%
    "     33.03%            31.77%
    &     32.82%            12.69%
    /     32.57%             9.84%
    >     31.00%            18.15%
    !     30.03%            37.34%
    #     28.28%            18.23%
    ,     26.41%            11.30%
    ~     23.49%            12.70%
    )     20.04%            34.87%
    ;     17.69%            24.23%
    +     15.76%             6.10%
    ^     15.67%            43.00%
    {     15.15%            10.87%
    *     15.07%            16.74%
    |     12.80%             9.63%
    ?     12.41%            11.66%
    `     11.79%            13.86%
    }     8.94%              8.97%
    =     2.84%              8.89%

This table lists how often a character is surrounded by other symbols or letters. A value of 100% in the letter column would mean that a symbol is always surrounded by letters on both sides. Spaces or newlines are ignored and represent the missing percentages.

This table can for example tell us that `=` almost always exist on it's own, which makes sense as I almost always surround it with spaces during assignment. Or that `_` is almost always is surrounded by letters, because it's used primarily in `snake_case` variable names.

By identifying symbols that mostly exist next to letters, we might assign them to the base layer and avoid excessive layer switching. Maybe `_`, `'`, `\`, `.`, `-` or `:`?


# What about Vim?

