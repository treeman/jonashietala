---
title: "Pollen sidenotes"
tags: Programming, Pollen, Racket
---

When evaluating Pollen [I complained][prev-post] about markdown/pandoc's lack of sidenote handling. I have solved it for Pollen but felt it deserved it's own post.

So in Pollen markup I want to be able to write this:

```
Lisp is a pretty nice ◊sn{cult} language.

◊ndef["cult"]{
    Some may say it's the language to rule them all.
}
```

And generate sidenotes like [Tufte CSS][]. Which is html like this:

```{.html}
Lisp is a pretty nice
<label for="cult"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="cult"
       class="margin-toggle"/>
<span class="sidenote">
    Some may say it's the language to rule them all.
</span>
language.
```

By having the sidenote span right in the middle of the text it allows us to [toggle it without javascript][toggle-noscript].

But how do we generate it in Pollen?


[prev-post]: #
[Tufte CSS]: https://edwardtufte.github.io/tufte-css/
[toggle-noscript]: https://stackoverflow.com/questions/11023816/toggle-divs-without-using-javascript

