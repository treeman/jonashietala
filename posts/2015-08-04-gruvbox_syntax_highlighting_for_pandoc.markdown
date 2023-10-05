---
layout: post
title: "Gruvbox Syntax Highlighting for Pandoc"
tags: ["Webpage", "CSS"]
---

**Edit:** I have [updated the highlighting][update] again so the inline code used as examples have changed. I guess I should've used images to record the look.

[update]: /blog/2019/01/25/site_restyle_and_update#changes-to-code-display "Site restyle and update: Changes to code display"


Recently when I [reinstalled Slackware][reinstall] I decided to restyle my workspace as well. I settled on [gruvbox][] with neovim and using [the generalized][gruvbox-generalized] package I also styled my terminal.

Then on a whim I wanted to restyle this site. When I built it I kinda left it without any styling at all, I guess the thinking was that it works and I can just tweak it later. Well it's been more than 2 years now and I never got around to it. I guess what bothered me the most was the fact that I had a really crappy styling syntax highlighting and styling of code. So I decided to change it.

I couldn't find any reference to highlighting pandoc styled like gruvbox, so I tried to emulate on myself. I use the darker one in neovim and in the terminal, but for the web page I used the lighter one:

```css
code, pre { background-color: #f2e5bc; } /* Hard contrast */
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
code.sourceCode span.re { color: #af3a03; } /* Region marker orange */
code.sourceCode span.er { color: #9d0006; font-weight: bold; } /* Error red */
code.sourceCode span.al { color: #9d0006; font-weight: bold; } /* Alert red */
```

This works with the [highlighting-kate package][] which is what [pandoc][], and therefore [Hakyll][], is using.

Additionally to get nicer spacing and font these styles are used:

```css
code {
    font-family: Consolas,"Courier New",monospace;
    font-size: 0.9em;
    padding: 0.2em;
    line-height: 1.3;
}

pre {
    word-wrap: normal;
    overflow: auto;
    padding: 0.6em 1.0em;
    line-height: 1.3;
}

pre code {
    padding: 0;
}
```

Currently I'm happy with the result and you can view the source of this site at <https://github.com/treeman/jonashietala>.

[reinstall]: /blog/2015/08/02/slackware_update "Reinstall Slackware"
[gruvbox]: https://github.com/morhetz/gruvbox "Gruvbox for vim"
[gruvbox-generalized]: https://github.com/morhetz/gruvbox-generalized "Gruvbox generalized"
[highlighting-kate package]: http://hackage.haskell.org/package/highlighting-kate "The highlighting-kate package"
[pandoc]: http://pandoc.org/index.html "Pandoc"
[Hakyll]: http://jaspervdj.be/hakyll/ "Hakyll"
