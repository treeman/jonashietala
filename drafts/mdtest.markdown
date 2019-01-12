---
layout: post
title: md_test (h1)
tags: Gaming, Ludum Dare
---

Preface

# Header 1 (h2)

Something

## Header 2 (h3)

Others

### Header 3 (h4)

Consider having your front door locked for example. You could consider your home secure if your door is locked and insecure if it’s open but that doesn’t say what you’re secure against. It’s missing context. It might prevent opportunistic thefts but a more determined attacker could instead break a window, pick the lock of the door or simply break down the door itself. [Maybe you live in a neighborhood](#) with lots of thefts and you might want an alarm system, a dog or iron bars over your windows.

At the same time it might not be practical to go all out and get the best protection possible. You might not want to give up your life and move to a bunker where your friends can’t visit and a private security force may be slightly too expensive.

abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz

http://www.youtube.com/watch?v=eoKDyhxCVm0


So we should do `foo` then `bar` then `quux`

ff fi fj ffl

This text is **bold**. This is *italicized*. This is ***both*** `Here's some code.`.

[I'm a link](asdf). <https://so.am.i> Email: <a@b.c>

# First Header

Quotes are "cool". Or are they 'cool'? 1234567890

> Just a normal quote. Where some dude says something smart.  
> And the next line.

Some other text

> First level of quoting.
>
> > This is a nested blockquote
>
> Hmm  
> Back to the first level.

## Second Header ##

Unordered list:

* Milk
* Bread
* Cheese
* Rice

Ordered list:

1. Milk
2. Bread
3. Cheese
5. Rice

    Can also have some text here

    But then the paragraph destroys?
6. Pew  
    This is closer  
    To what I want?

### Third Header ###

Table:

| Left aligned | Centered      | Right aligned |
| :----------- | :-----------: | -----------:  |
| First row    | Data          | Data          |
| Second row   | **Cell** 1    | 1111          |
| Third row    | **Cell** 2    | 3333          |
| Fourth row   | **Cell** 3    | 6789          |

<http://example.com>

<mail@jonashietala.se>

------------------

wow_great_stuff

~~Mistaken text.~~

| The limerick packs laughs anatomical
| In space that is quite economical.
|    But the good ones I've seen
|    So seldom are clean
| And the clean ones so seldom are comical

| 200 Main St.
| Berkeley, CA 94718

$x^2 = 3$

Here is a footnote reference,[^1] and another.[^longnote]

```{.css}
/* Gruvbox inspired */
code, pre { background-color: #f2e5bc; }
code.sourceCode span.kw { color: #9d0006; } /* Keyword red */
code.sourceCode span.dt { color: #b57614; } /* Datatype yellow */
code.sourceCode span.dv { color: #8f3f71; } /* DecVal purple */
code.sourceCode span.bn { color: #8f3f71; } /* BaseN purple */
code.sourceCode span.fl { color: #8f3f71; } /* Float purple */
code.sourceCode span.ch { color: #4070a0; } /* Char purple */
code.sourceCode span.st { color: #79740e; } /* String green */
code.sourceCode span.co { color: #928374; font-style: italic; }
code.sourceCode span.ot { color: #427b56; } /* OtherToken aqua */
code.sourceCode span.fu { color: #79740e; } /* Function green */
```

```{.css}
@font-face {
    font-family: 'Source Code';
    src: url('/fonts/SourceCodeVariable-Roman.woff') format('woff'); /* 80 ch */
    src: url('/fonts/SourceCodeVariable-Roman.woff') format('woff'); /*------ 90 ch ----*/
    src: url('/fonts/SourceCodeVariable-Roman.woff') format('woff'); /*---------- 100 ch ---------*/
}
```

```
empty
```

```{.C}
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

[^1]: Here is the footnote.

[^longnote]: Here's one with multiple blocks.

    Subsequent paragraphs are indented to show that they
belong to the previous footnote.

        { some.code }

    The whole paragraph can be indented, or just the first
    line.  In this way, multi-paragraph footnotes work like
    multi-paragraph list items.

This paragraph won't be part of the note, because it
isn't indented.

