---
title: "How I wrote a book using Pollen"
tags: Cryptocurrency, Programming, Pollen, Racket, Why cryptocurrencies?
---

I wrote an online book using [Pollen][pollen], a static site generator in [Racket][racket]. [An earlier post][impressions] contains my first impressions of it, but as the book is now completed I think I can summarize some implementation details in more detail with this post.

I *really* like the markup language and the usage of X-expressions. You do need to write code yourself for a bunch of things, but if you want a more complex markup for your site it's a trade-off I'd make every time.

# Configuring vim

First off I do my writing and coding in vim (or rather neovim). I use plugins for Racket and Pollen (but truthfully I don't remember what they do, syntax highlighting perhaps?) and I also use two special chars I don't normally use:

```
" Plugin handling using vim-plug 
Plug 'https://github.com/wlangstroth/vim-racket'
Plug 'https://github.com/otherjoel/vim-pollen.git'

" Easy insertion of special chars
imap <C-L> λ
imap <C-E> ◊
```

Pollen markup uses `◊` extensively so having an easy way to insert it is very important. And in Racket instead of writing lambdas `(lambda (x) ...)` you can write `(λ (x) ...)`. It's not necessary but I thought, why not?

# Configuring Pollen

The Pollen documentation does a decent job of walking you through initial setup, there's just a few gotchas I ran into.

I wanted to turn three dots into a single character "..." and also reroute links from `/index.html` to `/`, while doing the default paragraph expansion and converting quotes and dashes to their special chars as described in the Pollen docs. I ended up with these decoding functions:

```{.lisp}
(define (ellipses x)
  (string-replace x "..." "…"))

(define (de-indexify x)
  (string-replace x "/index.html" ""))

(define string-proc (compose1 smart-quotes
                              smart-dashes
                              ellipses
                              de-indexify))
(define (std-decode args)
  (decode-elements args
                   #:txexpr-elements-proc decode-paragraphs
                   #:string-proc string-proc
                   #:exclude-tags `(figure pre)))
```

After a while I wanted to organize my Racket files into a subfolder, and to get Pollen to recognize this I had to tell it to add them to the watchlist:

```{.lisp}
(module setup racket/base
  (require file/glob)
  ...
  (define rkt-files (glob "rkt/*.rkt"))
  (define cache-watchlist rkt-files))
```

# The markup language

Pollen's markup language can at first look extremely cumbersome, but I ended up liking it. Just prefix with `◊` and you'll automatically create html elements:

```
◊ul{
  ◊li{First item}
  ◊li{Second item}
  ◊li{Third item}
}
```

You can also specify attributes such as classes, which is quite handy:

```
◊div[#:class "my-class"]{ ... }
```

But the real power of the markup language is how easy it is to create your own tags. Just define regular Racket functions and provide them in pollen.rkt like so: `(provide (all-from-out "rkt/tags.rkt"))`{.lisp}

Here are some examples of tags I've implemented:

1. Code

   Non-highlighted code is simple and is provided by:
   
   ```{.lisp}
   (define (code . args)
     `(pre (code ,@args)))
   ```
   
   But for a more interesting example say that you want to include an external file and highlight it like so:
   
   ```{.lisp}
   ◊(code-hl "python3" "scripts/gambling.py")
   ```
   
   I implemented this by by calling out to `pygmentize`:
   
   ```{.lisp}
   (define (code-hl lang path)
       (define cmd (string-append
                   "pygmentize -f html"
                   " -l " lang
                   " " path))
       (string->xexpr
       (with-output-to-string (λ () (system cmd)))))
   ```
   
   There is an implementation in Pollen that does something similar, but it's *much* more complex.

3. Quotes
    
   I use quotes heavily throughout the text, either as a standalone or wrapped in an epigraph (just used to italize quotes that begins a chapter):
   
   ```{.lisp}
   ◊epigraph{
     ◊qt[#:author "Attributed to Michael Cassius McDonald"]{
       There's a sucker born every minute
     }
   }
    ```
    
    Producing:
    
    ```{.html}
    <div class="epigraph">
      <blockquote>
        <p>There’s a sucker born every minute</p>
        <footer><span class="author">Attributed to Michael Cassius McDonald</span></footer>
      </blockquote>
    </div>
    ```
    
   There's a large list of arguments to customize the quote, but that's a little overkill to go through here.

5. Links

# Sidenotes

I wrote a post about [Tufte style sidenotes and marginnotes in Pollen][sidenotes] in an earlier post, but I've since then changed them from being toggle-able on smaller screens to be placed beneath the text instead of to the side.

---

![On a wider screen a sidenote is placed to the right.](/images/whycrypto/sidenotes_wide.png)

---

![If the sidenote doesn't fit, it's instead placed below.](/images/whycrypto/sidenotes_below.png)

---

Now it would be fairly easy (or at least it's possible to style it in such a way with a lot of trial and error) to just move it to the right if it doesn't fit. But notice that on the narrower screen the sidenote is placed below the last paragraph!

Indeed what I wanted is to be able to customize the placement on the narrower screen as I wanted, while the floating sidenote should be as close to their reference as possible. I solved it, but it's not very pretty...

My first thought was to just insert two sidenotes, and just set `display:none` to hide one of them. But this would break screen readers or simplified readers such as the "reader view" in Firefox. So I opted for a more complex solution of manually modifying the top margin for each sidenote.

In practice it means I insert a sidenote using `◊sn{my-ref}`, which by default inserts it below the current paragraph. If I want to manually place it somewhere else I use `◊note-pos[#:top -9]{my-ref}`. So for example:

```
First paragraph.◊sn{my-ref}

Second paragraph.

◊note-pos[#:top -9]{my-ref}
```

The text for the sidenote is given by `◊ndef["my-ref"]{Sidenote text here}`, which can be paced anywhere in the source file.

There's a bunch of sidenote specific styling, but the important parts are given by:

```{.css}
.side-space {
    /* Serves to take up space. Inline content from the chapter is floated on top. */
    width: 420px;
}
@media (max-width: $sidenote-float-width - 1) {
    .sidenote {
        display: block;
        margin: 1em 4em 1.4em 4em !important;
    }
}
@media (min-width: $sidenote-float-width) {
    .side-space {
        display: block;
    }
    .sidenote {
        float: right;
        clear: right;
        /* Approximate adjustment for notes at the last line of a paragraph. */
        /* Will get overridden by local styles. */
        margin-top: -2em;
        margin-bottom: 4em;
        margin-right: -420px;
        width: 380px;
        position: relative;
        display: inline-block;
    }
}
```

And margins are overridden by `<div class="sidenote" style="margin-top:-9em;">`{.html}.

The actual implementation of `◊sn{}` and `◊ndef` has grown surprisingly large and I went through the old version in the [previous post][sidenotes], so I'll skip it here. The implementation has changed a little but not in any major way. You can always find the [latest code on GitHub][sidenote-code] if you're interested.


# Local markup

Another very powerful feature is that you can easily create custom markup for individual pages. For example in the chapter [What is money?][what-is-money] when giving a number of examples of money I wanted to add custom styling, like this:

---

![I wanted to have an image associated with each example, alternated from left to right. I also wanted to give each example a title with an associated timeframe.](/images/whycrypto/ex_money_slim.png)

---

This is the markup I came up with:

```{.lisp}
◊(define (money title #:img img #:date [date #f] . text)
   (define xdate
     (cond
       [(and (list? date) (not (null? date)))
        `(div ((class "date")) ,@date)]
       [date
        `(div ((class "date")) ,date)]
       [else
        ""]))
   `(div ((class "example"))
      (img ((src ,img)))
      (div ((class "txt"))
        (div ((class "header"))
          (h3 ,title)
          ,xdate)
        ,@text)))
```

Which can be called like this:

```
◊money["Dogecoin"
       #:date "2013"
       #:img "images/doge.png"]{
    Dogecoin is a cryptocurrency, while created as a "joke currency", it quickly gained popularity as a tipping tool online. You can still find merchants who accept it today for things like domain names, web hosting, VPNs or games.
}
```

To produce this html:

```{.html}
<div class="example"><img src="images/doge.png">
    <div class="txt">
        <div class="header">
            <h3>Dogecoin</h3>
            <div class="date">2013</div>
        </div>
        <p>Dogecoin is a cryptocurrency, while created as a “joke currency”, it quickly gained popularity as a tipping tool online. You can still find merchants who accept it today for things like domain names, web hosting, VPNs or games.</p>
    </div>
</div>
```

So easy! So powerful! So great!

Of course you could've done this manually in for example Markdown, but with 10 different examples that's a ton of copy-pasting making any changes to the html *really* annoying to make.

I use local custom markup all over the place, another example is styling transcripts of a Youtube video:

---

![The timestamps should be aligned in a certain way and have a horizontal bar.](/images/whycrypto/transcript.png)

---

We could be as explicit as we were with the money example, but here I'd like it to be simpler and to auto detect the timestamps. So I can write it like this:

```{.html}
◊div[#:class "transcript-wrapper"]{

  ◊transcript{
      02:34  He's got an RPG [Rocket Propelled Grenade]?
      02:35  All right, we got a guy with an RPG.
  }

  Except now we know it's not an RPG but a camera held by a Reuters journalist.

  ◊transcript{
      03:45  All right, hahaha, I hit [shot] 'em...
      ...
      04:55  Oh, yeah, look at those dead bastards.
      05:00  Nice.
      ...
      06:57  Come on, buddy.
      07:01  All you gotta do is pick up a weapon.
  }

  After killing a bunch of people they're looking at an injured person crawling on the ground wanting him to pick up a weapon---so they're allowed to kill him.
```

This can be accomplished by writing a bit of lisp code that splits the strings on the double spaces:

```{.lisp}
◊(define (transcript . rows)
   (define (make-row row)
     (if (string=? row "\n")
       ""
       (let ((cols (string-split row "  ")))
         `(div ((class "row"))
            (span ((class "time")) ,(car cols))
            (span ((class "txt")) ,@(cdr cols))))))
   `(div ((class "transcript"))
      ,@(map make-row rows)))
```

(Yes I know that the "..." row will generate a `<span class="time">...</span>` and a `<span class="txt"></span>` element, but it doesn't really matter for our case.)


# Unsolved annoyances

1. Styling
1. Lack of routing control
2. Slow
3. Not really practical to generate two different output formats

[main]: https://whycryptocurrencies.com/ "Why cryptocurrencies?"
[pollen]: https://docs.racket-lang.org/pollen/ "Pollen: the book is a program"
[racket]: https://racket-lang.org/ "Racket"
[sass]: https://sass-lang.com/ "Sass: CSS with superpowers"
[what-is-money]: https://whycryptocurrencies.com/what_is_money.html "What is money?"

[impressions]: /blog/2019/03/03/first_impressions_of_pollen/ "First impressions of Pollen"
[sidenotes]: /blog/2019/03/04/pollen_sidenotes/ "Tufte style sidenotes and marginnotes in Pollen"
[sidenote-code]: https://github.com/treeman/why_cryptocurrencies/blob/master/rkt/sidenotes.rkt "Github sidenotes"

