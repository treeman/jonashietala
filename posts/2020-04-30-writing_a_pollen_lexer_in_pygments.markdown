---
title: "Writing a Pollen lexer in Pygments"
tags: Pollen, Programming
---

After writing a [few blog posts][pollen-tags] about Pollen I started getting annoyed that I didn't have syntax highlighting for the code snippets. So I did a bit of fooling around with Pygments, and it turns out writing a custom lexer isn't that unreasonable, so here's how I did it.

# Pollen markup

Pollen's rules are pretty simple as it's basically just some extra syntax for embedding Racket in a text file:

1. Comments starts with `◊;`
2. You can insert variables with `◊|my-var|`
3. Run arbitrary Racket code with `◊( ... )`
4. There's an extra construction that transforms `◊fun[arg1 arg2]{some text}` to `◊(fun arg1 arg2 "some" text")`, which is useful when you want to send a bunch of interpolated text to a function. (I use it everywhere in [my book][main].)

[main]: https://whycryptocurrencies.com/ "Why Cryptocurrencies?"

So the end goal is to be able to highlight this type of code:

```
◊; A link can just be a standard reference
◊(define dune-audible "https://www.audible.com/pd/Dune-Audiobook/B002V1OF70")

I'm ◊strong{really} looking forward to the upcoming Dune movie!

◊div[#:class "extra-sand"]{
    I also recommend the Dune audiobook ◊link[#:ref dune-audible]{on Audible}.
}
```

# Setup and parsing comments

The simpest [Pygments][] lexer might look like this:

[Pygments]: https://pygments.org/

```python
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

```fish
python3 -m pygments -l pollen.py:PollenLexer -x -f html pollen.html.pm
```

That produces html output. Right now it doesn't do anything interesting, as it only returns a `Text` token for everything, so let's change that shall we?

I would like to support highlighting comments:

```
◊; Standard comment
Regular text ◊; Trailing comment
```

Which should be pretty straightforward. We just need to add a single clause to the root state that matches everything from `◊;` to the end of the line:

```python
tokens = {
    'root': [
        (r'◊;.*?$', Comment),
        (r'.', Text)
```

And comments are highlighted!

```fish
python3 -m pygments -l pollen2.py:PollenLexer -x -f html pollen.html.pm
```

```html
<div class="highlight"><pre><span></span>
<span class="c">◊; Standard comment</span>
Regular text <span class="c">◊; Trailing comment</span>
</pre></div>
```

That's hard to read so I'll embed the output from now on:

![](/images/pollen_rework/comment.png)

If you're reading this but don't understand why it works you might want to lookup [regular expressions in Python][py-regex], in this post I'll assume you're familiar.


# Embedding variables

Let's move on to embedding variables via `◊|var|`.

A first attempt could be like this:

```python
'root': [
    (r'(◊\|)(\w+)(\|)',
        bygroups(Name.Variable.Magic, Name.Variable, Name.Variable.Magic)),
```

Which splits out highlighting into three groups; `◊|`, `var` and `|` and gives them different tokens using the special `bygroups` construction. The tokens you choose are slightly arbitrary, but should generally follow the advice in [Pygments tokens reference][py-tokens]. I chose `Name.Variable.Magic` instead of something like `Punctuation` because I wanted them to stand out more.

This works, but there are two changes I'd like to make. The immediate problem is that we now only match against characters with `\w+`, but a Racket variable can contain a bunch of different symbols. This is for example perfectly valid:

```racket
(define a2-!+*# 2)
(print a2-!+*#)
```

If we look at the [existing Racket lexer][racket-lexer] they have defined a variable like this:

```python
valid_symbol_chars = r'[\w!$%*+,<=>?/.\'@&#:-]'
variable = r'[A-Z]%s*' % valid_symbol_chars
```

Which we can steal and copy to our class and use when we build our regex:

```python
(r'(◊\|)(%s)(\|)' % variable,
    bygroups(Name.Variable.Magic, Name.Variable, Name.Variable.Magic)),
```

To make this work out of the box we also need to add the regex flags:

```python
flags = re.IGNORECASE | re.MULTILINE
```

The other thing I want to do is introduce another state. It's not strictly needed in this case, but as `◊` can be followed by different cases it makes the lexer easier to follow. Like this:

```python
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

![](/images/pollen_rework/vars.png)

I unfortunately didn't figure out how to debug the state transitions in an easy manner. If we mess up Pygments will insert `err` classes but you can also play around with different colored tokens during development, for example letting `root` return a `Keyword` token so you can see that we return to the right state.


# Highlighting Racket code

Our next step is to try to highlight Racket code inside `◊( ... )`.

I thought this was going to be really hard, but Pygments supports this in various ways. The way I chose was to delegate the lexer of different parts to the existing `RacketLexer`.

First the import:

```python
from pygments.lexers.lisp import RacketLexer
```

And the case is simply:

```python
'post-magic': [
    ...
    (r'(\()(.+)(\))',
        bygroups(Name.Variable.Magic,
                 using(RacketLexer, state='unquoted-datum'),
                 Name.Variable.Magic),
        '#pop')
```

The interesting line is `using(RacketLexer, state='unquoted-datum')`python which delegates the lexer to `RacketLexer`, starting in state `unquoted-datum`. How did I figure out which initial state to start in? I tried to [read the code][racket-lexer] and make an educated guess...

But we also need to ensure we use the regex flag of allowing the dot to match newlines as well, otherwise we won't match multiline racket expressions:

```python
flags = re.IGNORECASE | re.DOTALL | re.MULTILINE
```

And all embedded Racket code is highlighted:

![](/images/pollen_rework/racket.png)


# Recursive brackets

Now lexing `◊var[arg1]{text args}` is a bit more involved, but builds on the concepts we've already seen.

First let's support the simpler `◊var{text args}` case.

Matching `◊var` is straightforward:

```python
'post-magic': [
    ...
    (r'%s' % variable, Name.Variable, ('#pop', 'curly-start')),
],
```

We could do more here, but we're preparing for the future where we can also match against an optional `[...]` after the variable, so we'll delegate to another state. `('#pop', 'curly-start')`python essentially means we'll replace the current state `post-magic` with the new `curly-start` state.

```python
'curly-start': [
    (r'\{', Name.Variable.Magic, ('#pop', 'curly-end'))
],
```

Here again we could've done more, but we want to be able to do recursive matching inside `{ ... }` as well. This is what the `curly-end` state does:

```python
'curly-end': [
    (r'\}', Name.Variable.Magic, '#pop'),
    include('root'),
],
```

`include('root')`python does what you might expect it to do: it copies all cases from our `root` state into the `curly-end` sate. This to avoid code duplication.

And this can indeed highlight `◊var{ ... }` recursively!

![](/images/pollen_rework/var_rec.png)

To support an optional `[ ... ]` we can add another state before `curly-end` that either matches against square brackets or curly brackets:

```python
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

We've already seen these things before. The only thorn in my side here is that we use a non-greedy match `(.+?)` to match between brackets, but we used a greedy `(.+)` earlier. I don't have a good answer for this... This could probably be improved some way.

But hey! Think positive! It works for the cases I need. For example:

![](/images/pollen_rework/div_rec.png)


# The result

Putting it all together we can now highlight the code we looked at in the start of this post:

![](/images/pollen_rework/big_res.png)

And this is the complete lexer:

```python
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
[pollen-tags]: /blog/tags/pollen/ 
[py-regex]: https://docs.python.org/3/library/re.html
