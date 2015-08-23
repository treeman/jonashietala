---
layout: post
title: md_test
tags: Gaming, Ludum Dare
---

# Header 1

## Header 2

### Header 3

#### Header 4


So we should do `foo` then `bar` then `quux`

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

    // This is a very quite a long line really: asd asd asd asd asdasd asd asd asdasd asd as asd asd asd asdasd asd asd asd asd ads asd asd asd asd asd asd asd asd asd asd asd asd asd asd asd asdasd asd 
}
```

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
code.sourceCode span.co { color: #928374; font-style: italic; } /* Comment medium */
code.sourceCode span.ot { color: #427b56; } /* OtherToken aqua */
code.sourceCode span.fu { color: #79740e; } /* Function green */
```

This text is **bold**. This is *italicized*. `Here's some code.`

[I'm a link](asdf). <https://so.am.i> Email: <a@b.c>

# First Header

> First level of quoting.
>
> > This is a nested blockquote
  that runs into the next line.
>
> # Header in quoting
>
> Hmm
>
> Back to the first level.

## Second Header ##

* Milk
* Bread
* Cheese
    * Cheddar
    * Camambert
* Rice

1. Milk
2. Bread
3. Cheese
    1. Cheddar
    2. Camambert
5. Rice

    Can also have some text here

    But then the paragraph destroys?
6. Pew  
    This is closer

    To what I want?

### Third Header ###

| First Header | Second Header | Third Header |
| :----------- | :-----------: | -----------: |
| First row    | Data          | Data         |
| Second row   | **Cell**      | *Cell*       |
| Third row    | **Cell**      | *Cell*       |

**Pew?**

http://example.com

------------------

wow_great_stuff

~~Mistaken text.~~

