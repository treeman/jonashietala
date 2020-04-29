---
title: "Writing a Pollen lexer in Pygments"
tags: Pollen, Programming
---

After writing a few blog posts about Pollen I started getting annoyed that I didn't have syntax highlighting for the code snippets. So I did a bit of fooling around with Pygments, and it turns out writing a custom lexer isn't that unreasonable, so here's how I did it.

# Pollen markup

Pollen's rules are pretty simple as it's basically just some extra syntax for embedding Racket in a text file:

1. Comments starts with `◊;`
2. You can insert variables with `◊|my-var|`
3. Run arbitrary Racket code with `◊( ... )`
4. There's an extra construction that transforms `◊fun[arg1 arg2]{some text}` to `◊(fun arg1 arg2 "some" text")`, which is useful when you want to send a bunch of text to a function. (I use it everywhere in my book.)

So basically I want to be able to highlight this type of code:

```
◊; A link can just be a standard reference
◊(define dune-audible "https://www.audible.com/pd/Dune-Audiobook/B002V1OF70")

I'm ◊strong{really} looking forward to the upcoming Dune movie!

◊div[#:class "extra-sand"]{
    I also recommend the Dune audiobook ◊link[#:ref dune-audible]{on Audible}.
}
```

# Setup and parsing comments

The simpest lexer might look like this:

```{.python3}
from pygments.lexer import *
from pygments.token import *
import re

class PollenLexer(RegexLexer):
    """
    Lexer for Pollen
    """

    name = 'Pollen'
    aliases = ['pollen']
    filenames = ['*.html.pm']

    tokens = {
        'root': [
            (r'.', Text)
        ],
    }

```

Which if placed in `pollen.py` can be run on a file `pollen.html.pm` like this:

```{.fish}
$ python3 -m pygments -l pollen.py:PollenLexer -x -f html pollen.html.pm
```

That produces html output. Right now it doesn't do anything interesting, as it only returns a `Text` token for everything, so let's change that shall we?

I would like to support highlighting comments:

```
◊; Standard comment
Regular text ◊; Trailing comment
```

Which should be pretty straightforward. We just need to add a single clause to the root token that matches everything from `◊;` to the end of the line:

```{.python3}
tokens = {
    'root': [
        (r'◊;.*?$', Comment),
        (r'.', Text)
```

And comments are highlighted!

```{.fish}
$ python3 -m pygments -l pollen2.py:PollenLexer -x -f html pollen.html.pm
```

```{.html}
<div class="highlight"><pre><span></span><span class="c">◊; Standard comment</span>
Regular text <span class="c">◊; Trailing comment</span>
</pre></div>
```

That's hard to read so I'll embed the output from now on:

<div class="highlight"><pre><span></span><span class="c">◊; Standard comment</span>
Regular text <span class="c">◊; Trailing comment</span>
</pre></div>

(Of course this assumes you've got a stylesheet that colors the output.)


# Embedding variables

Let's move on to embedding variables via `◊|var|`.

A first attempt could be like this:

```{.python3}
'root': [
    (r'(◊\|)(\w+)(\|)',
        bygroups(Name.Variable.Magic, Name.Variable, Name.Variable.Magic)),
```

Which splits out highlighting into three groups; `◊|`, `var` and `|` and gives them different tokens using the special `bygroups` construction. The tokens you choose are slightly arbitrary, but should generally follow the advice in [Pygments tokens reference][py-tokens]. I chose `Name.Variable.Magic` instead of something like `Punctuation` because I wanted them to stand out more.

This works, but there are two changes I'd like to make. The immediate problem is that we now only match against characters with `\w+`, but a Racket variable can contain a bunch of different symbols. This is for example perfectly valid:

```{.racket}
(define a2-!+*# 2)
(print a2-!+*#)
```

If we look at the [existing Racket lexer][racket-lexer] they have defined a variable like this:

```{.python3}
valid_symbol_chars = r'[\w!$%*+,<=>?/.\'@&#:-]'
variable = r'[A-Z]%s*' % valid_symbol_chars
```

Which we can steal and copy to our class and use when we build our regex:

```{.python3}
(r'(◊\|)(%s)(\|)' % variable,
    bygroups(Name.Variable.Magic, Name.Variable, Name.Variable.Magic)),
```

To make this work out of the box we also need to add the regex flags:

```{.python3}
flags = re.IGNORECASE | re.MULTILINE
```

The other thing I want to do is introduce another state. It's not strictly needed in this case, but as `◊` can be followed by different cases it makes the lexer easier to follow. Like this:

```{.python3}
'root': [
    (r'◊;.*?$', Comment),
    ('◊', Name.Variable.Magic, 'post-magic'),
    (r'.', Text)
],

'post-magic': [
    (r'(\|)(%s)(\|)' % variable,
        bygroups(Name.Variable.Magic, Name.Variable, Name.Variable.Magic),
        '#pop'),
],
```

Now what happens when we parse the `◊` we mark it as `Name.Variable.Magic` and the push the state `post-magic` onto the stack. Important to note is that we don't replace the existing state, so the stack will now have `post-magic` on top and `root` below.

Then when we try to parse the next character, `post-magic` is responsible to match against it. The new thing there is the magic `'#pop'` variable that pops from the stack, so after we're done matching `◊|var|` we hand back control to `root`.

This should now be able to highlight embedding variables:

<div class="highlight"><pre><span></span>Text <span class="vm">◊|</span><span class="nv">a1</span><span class="vm">|</span> more text <span class="vm">◊|</span><span class="nv">a2-!+*#</span><span class="vm">|</span>.
</pre></div>

I unfortunately didn't figure out how to debug the state transitions in an easy manner. If we mess up Pygments will insert `err` classes but you can also play around with different colored tokens during development, for example letting `root` return a `Keyword` token so you can see we return to the right state.


# Highlighting Racket code

Our next step is to try to highlight Racket code inside `◊( ... )`!

I thought this was going to be really hard, but Pygments supports this in various ways. The way I chose was to delegate the lexer of different parts to the existing `RacketLexer`.

First the import:

```{.python3}
from pygments.lexers.lisp import RacketLexer
```

And the case is simply:

```{.python3}
'post-magic': [
    ...
    (r'(\()(.+)(\))',
        bygroups(Name.Variable.Magic,
                 using(RacketLexer, state='unquoted-datum'),
                 Name.Variable.Magic),
        '#pop')
```

The interesting line is `using(RacketLexer, state='unquoted-datum')` which delegates the lexer to `RacketLexer`, starting in state `unquoted-datum`. How did I figure out which initial state to start in? I tried to [read the code][racket-lexer] and make an educated guess...

But we also need to ensure we use the regex flag of allowing the dot to match newlines as well, otherwise we won't match multiline racket expressions:

```{.python3}
flags = re.IGNORECASE | re.DOTALL | re.MULTILINE
```

And all embedded Racket code is highlighted:

<div class="highlight"><pre><span></span><span class="vm">◊(</span><span class="k">define</span> <span class="p">(</span><span class="n">fun</span> <span class="kd">#:ref</span> <span class="n">x</span> <span class="o">.</span> <span class="n">y</span><span class="p">)</span>
  <span class="p">(</span><span class="nb">string-append</span> <span class="n">x</span> <span class="s2">&quot;-&quot;</span> <span class="n">y</span><span class="p">)</span><span class="vm">)</span>
</pre></div>


# Recursive brackets

Now lexing `◊var[arg1]{text args}` is a bit more involved, but builds on the concepts we've already seen.

First let's support the simpler `◊var{text args}` case.

Matching `◊var` is straightforward:

```{.python3}
'post-magic': [
    ...
    (r'%s' % variable, Name.Variable, ('#pop', 'curly-start')),
],
```

We could do more here, but we're preparing for the future where we can also match against an optional `[...]` after the variable, so we'll delegate to another state. The `('#pop', 'curly-start')` essentially means we'll replace the `post-magic` state with the `curly-start` state.

```{.python3}
'curly-start': [
    (r'\{', Name.Variable.Magic, ('#pop', 'curly-end'))
],
```

Here again we could've done more, but we want to be able to do recursive matching inside `{ ... }` as well. This is what the `curly-end` state does:

```{.python3}
'curly-end': [
    (r'\}', Name.Variable.Magic, '#pop'),
    include('root'),
],
```

`include('root')` does what you might expect it to do: it copies all cases from our `root` state into the `curly-end` sate. This to avoid code duplication.

And this can indeed highlight `◊var{ ... }` recursively!

<div class="highlight"><pre><span></span><span class="vm">◊</span><span class="nv">foo</span><span class="vm">{</span>some <span class="vm">◊</span><span class="nv">bar</span><span class="vm">{</span>random<span class="vm">}</span> text<span class="vm">}</span>
</pre></div>

To support an optional `[ ... ]` we can add another state before `curly-end` that either matches against square brackets or curly brackets:

```{.python3}
'post-magic': [
    ...
    (r'%s' % variable, Name.Variable, ('#pop', 'post-var')),
],

'post-var': [
    (r'(\[)(.+?)(\])',
        bygroups(Name.Variable.Magic,
                    using(RacketLexer, state='unquoted-datum'),
                    Name.Variable.Magic),
        ('#pop', 'curly-start')),
    include('curly-start'),
],
```

We've already seen these things before. The only thorn in my side here is that we use a non-greedy match `(.+?)` to match between brackets, but we used a greedy `(.+)` earlier. I don't have a good answer for this... So my code is probably broken some way here.

But hey! Think positive! It works for the cases I've tried, for example:

<div class="highlight"><pre><span></span><span class="vm">◊</span><span class="nv">div</span><span class="vm">[</span><span class="kd">#:class</span> <span class="s2">&quot;my-class&quot;</span><span class="vm">]{</span>
    I <span class="vm">◊</span><span class="nv">strong</span><span class="vm">[</span><span class="kd">#:class</span> <span class="s2">&quot;super-strong&quot;</span><span class="vm">]{</span>really<span class="vm">}</span> like tacos.
<span class="vm">}</span>
</pre></div>


# The result

Putting it all together we can now highlight the code we looked at in the start of this post:

<div class="highlight"><pre><span></span><span class="c">◊; A link can just be a standard reference</span>
<span class="vm">◊(</span><span class="k">define</span> <span class="n">dune-audible</span> <span class="s2">&quot;https://www.audible.com/pd/Dune-Audiobook/B002V1OF70&quot;</span><span class="vm">)</span>

I&#39;m <span class="vm">◊</span><span class="nv">strong</span><span class="vm">{</span>really<span class="vm">}</span> looking forward to the upcoming Dune movie!

<span class="vm">◊</span><span class="nv">div</span><span class="vm">[</span><span class="kd">#:class</span> <span class="s2">&quot;extra-sand&quot;</span><span class="vm">]{</span>
    I also recommend the Dune audiobook <span class="vm">◊</span><span class="nv">link</span><span class="vm">[</span><span class="kd">#:ref</span> <span class="n">dune-audible</span><span class="vm">]{</span>on Audible<span class="vm">}</span>.
<span class="vm">}</span>
</pre></div>

And this is the complete lexer:

```{.python3}
from pygments.lexer import *
from pygments.token import *
from pygments.lexers.lisp import RacketLexer
import re

class PollenLexer(RegexLexer):
    """
    Lexer for Pollen
    """

    name = 'Pollen'
    aliases = ['pollen']
    filenames = ['*.html.pm']

    flags = re.IGNORECASE | re.DOTALL | re.MULTILINE

    valid_symbol_chars = r'[\w!$%*+,<=>?/.\'@&#:-]'
    variable = r'[A-Z]%s*' % valid_symbol_chars

    tokens = {
        'root': [
            (r'◊;.*?$', Comment),
            ('◊', Name.Variable.Magic, 'post-magic'),
            (r'.', Text)
        ],

        'post-magic': [
            (r'(\|)(%s)(\|)' % variable,
                bygroups(Name.Variable.Magic, Name.Variable, Name.Variable.Magic),
                '#pop'),
            (r'(\()(.+)(\))',
                bygroups(Name.Variable.Magic,
                         using(RacketLexer, state='unquoted-datum'),
                         Name.Variable.Magic),
                '#pop'),
            (r'%s' % variable, Name.Variable, ('#pop', 'post-var')),
        ],

        'post-var': [
            (r'(\[)(.+?)(\])',
                bygroups(Name.Variable.Magic,
                         using(RacketLexer, state='unquoted-datum'),
                         Name.Variable.Magic),
                ('#pop', 'curly-start')),
            include('curly-start'),
        ],

        'curly-start': [
            (r'\{', Name.Variable.Magic, ('#pop', 'curly-end'))
        ],

        'curly-end': [
            (r'\}', Name.Variable.Magic, '#pop'),
            include('root'),
        ],
    }

```

[racket-lexer]: https://github.com/pygments/pygments/blob/master/pygments/lexers/lisp.py#L459 "RacketLexer on Github"
[py-tokens]: https://pygments.org/docs/tokens/ "Pygments tokens"

