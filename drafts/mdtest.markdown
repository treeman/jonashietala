---
layout: post
title: "md_test (h1)"
tags: Gaming, Ludum Dare
---

`x->y`c

> Mamma Mia! Here we go again...
> { :epigraph }

# Header 1 (h2)

font-size("xl")

## Header 2 (h3)

font-size("l")

### Header 3 (h4)

font-size("m")

#### Header 4 (h5)

font-size("s")

#### Header 5 (h6)

font-size("xs")

# Just text

Consider having your front door locked for example. You could consider your home secure if your door is locked and insecure if it’s open but that doesn’t say what you’re secure against. It’s missing context. It might prevent opportunistic thefts but a more determined attacker could instead break a window, pick the lock of the door or simply break down the door itself. [Maybe you live in a neighborhood](#) with lots of thefts and you might want an alarm system, a dog or iron bars over your windows.

At the same time it might not be practical to go all out and get the best protection possible. You might not want to give up your life and move to a bunker where your friends can’t visit and a private security force may be slightly too expensive.

---

abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz

Unicode snowman: ☃︎ and "auto quotes" enabled. It's good. --dashes--? Converts... to …

Quotes are "cool". Or are they 'cool'? 1234567890

\_

ff fi fj ffl

This text is **bold**. This is _italicized_. This is **_both_** `Here's some code.`. With \_\

[blog](/blog) and auto <https://google.com> email: <mail@jonashietala.se>

# Lists

Unordered list:

- Milk
- Bread
- Cheese

  Paragraph

- Rice

Ordered list:

1. Milk
2. Bread
3. Cheese
4. Rice

   Can also have some text here

   And here

   1. Another
   2. Nested
   3. List

5. Pew  
   This is really close  
   I think it's too close?

# Quotes and attributes

> This is an epigraph
{ :epigraph }

Text

> Just a normal quote. Where some dude says something smart.  
> And the next line.

Nesting:

> First level of quoting.
>
> > This is a nested blockquote
>
> Hmm  
> Back to the first level.

Text

> This is [a notice](#)
{ :notice }

Text

> With author attribution
{ author=John Doe }


# Tables

Table:

| Left aligned |  Centered  | Right aligned |
| :----------- | :--------: | ------------: |
| First row    |    Data    |          Data |
| Second row   | **Cell** 1 |          1111 |
| Third row    | **Cell** 2 |          3333 |
| Fourth row   | **Cell** 3 |          6789 |

Easy table So cool

| One | Two |
| --- | --- |
| 1   | 2   |
{ .class }

Movie | Rank this year | Rank last year | change
:----- | -------------- | -------------- | ------
Avengers: Endgame                   | 1 | - | -
Guardians of the Galaxy             | 2 | 5 | +3
Avengers: Infinity War              | 3 | 4 | +1
Iron Man                            | 4 | 1 | -3
Captain America: The Winter Soldier | 5 | 3 | -2
{ .movie-table }

Movie | Rank this year | Rank last year | change
:----- | -------------- | -------------- | ------
The Incredible Hulk     | 21 | 19 | -2
Thor 2: The Dark World  | 22 | 17 | -5
{ .movie-table }

# Code

So we should do `foo` then `bar` then `quux`

```fish
python3 -m pygments -l pollen.py:PollenLexer -x -f html pollen.html.pm
```

```bash
python3 -m pygments -l pollen.py:PollenLexer -x -f html pollen.html.pm -t some_long_argument_that_should overflow
```

```
Code without a language is here
```

```css
/* Gruvbox inspired */
code,
pre {
  background-color: #f2e5bc;
}
code.sourceCode span.kw {
  color: #9d0006;
} /* Keyword red */
code.sourceCode span.dt {
  color: #b57614;
} /* Datatype yellow */
code.sourceCode span.dv {
  color: #8f3f71;
} /* DecVal purple */
code.sourceCode span.bn {
  color: #8f3f71;
} /* BaseN purple */
code.sourceCode span.fl {
  color: #8f3f71;
} /* Float purple */
code.sourceCode span.ch {
  color: #4070a0;
} /* Char purple */
code.sourceCode span.st {
  color: #79740e;
} /* String green */
code.sourceCode span.co {
  color: #928374;
  font-style: italic;
}
code.sourceCode span.ot {
  color: #427b56;
} /* OtherToken aqua */
code.sourceCode span.fu {
  color: #79740e;
} /* Function green */
```

```css
@font-face {
  font-family: "Source Code";
  src: url("/fonts/SourceCodeVariable-Roman.woff") format("woff"); /* 80 ch */
  src: url("/fonts/SourceCodeVariable-Roman.woff") format("woff"); /*------ 90 ch ----*/
  src: url("/fonts/SourceCodeVariable-Roman.woff") format("woff"); /*---------- 100 ch ---------*/
  src: url("/fonts/SourceCodeVariable-Roman.woff") format("woff"); /*------------------ 120 ch ------------------------*/
}
```

```cpp
int i = 2;
```

```C
#pragma once
#include "foo.h"

/* Awesome function */
int main(int *argc, int *argv[]) {
    for (int i = 1; i < argc; ++i) {
        printf("%s\n", argv[i]);
    }

    float x = 1.3;
    char *my_string = ['a', 'b', '\n', '\0'];

    /*
    **x int = 42;
    */
    #if x
    char *action = "foo";
    #else
    char *action = "quux";
    #endif

    /* I has fold?: {{{ */
    int **pp = 0;
    /* }}} */
    //BEGIN
    /* Here be region marker
       TODO use this to make good stuff
    */
    typedef struct {
        int i;
        char k;
    } elem_t;
    elem_t user;
    //END
    #badsyntax

    <= => !==

    // This is a very quite a long line really: asd asd asd asd asdasd asd asd asdasd asd as asd asd asd asdasd asd asd asd asd ads asd asd asd asd asd asd asd asd asd asd asd asd asd asd asd asdasd asd

    /* multiline
     * comment
     */
}
```

# Images

Figure:

![FTL is such an amazing game](/images/ftl_start.png)

Set properties:

![A small led cube displayed in 400px](/images/small_led_cube.jpg){ width=400px }

Flex images

::: Flex
/images/configura14/octree1.png
/images/configura14/octree2.png
:::

Image gallery:

::: Gallery
/images/inside1.jpg
/images/case.jpg
/images/cables.jpg
/images/inside2.jpg
/images/das1.jpg
:::

End gallery. Ooh so pretty! :)

# Embed YouTube

http://www.youtube.com/watch?v=eoKDyhxCVm0

