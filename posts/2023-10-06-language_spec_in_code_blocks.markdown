---
title: "Language spec in code blocks"
tags: ["Webpage", "CSS"]
---

Even though I just recently [restyled the blog][] and wasn't going to touch it, there was one thing I wanted to add, but it felt a little bothersome so I skipped it.
It was to display the highlighted language in code blocks:

![The code spec displays what programming language the code block contains.](/images/code_spec.png)

As it happens, my little girl---the fountain of joy that never stops giving---decided to not sleep last night.
(I can't really be angry with her, she's only 10 months and she's getting sick.)

So there I was, carrying her in a harness and trying not to play with her because I just want her to fall asleep.
I usually do this in front of the computer so she can listen to music and fall asleep (her favorite is **In Flames: Only for the Weak**).
But that's also time I can use to to work on weird things, and in this case I reworked the code block display.

> This is also how I created the [T-34 keyboard layout][t-34]. Another of our kids was awake for 1--2 hours every night for like half a year, so I filled that time by creating my layout.
>
> A sleep deprived state is a recipe for doing weird shit.
{ :notice }

[t-34]: /series/t-34/

Previously, code blocks were generated like this:

```html
<pre>
  <code class="highlight viml">
    ...
  </code>
</pre>
```

The problem was that I couldn't just add a `div` inside the `pre` tag because I want the code to be scrollable when it overflows:

```scss
pre {
  overflow-wrap: normal;
  overflow: auto;
}
```

But then the contents cannot exist partially outside of the tag, since that would be hidden, and scrolling would also move the language spec element.

So instead I used another `div` to wrap everything:

```html
<div class="code-wrapper">
  <div class="lang viml"></div>
  <pre>
    <code class="highlight viml">
      ...
    </code>
  </pre>
</div>
```

This allows me to use the `::before` pseudo-class to insert the language description depending on the class (`viml` in this example).
See the relevant styling:

```scss
.code-wrapper .lang {
  // Move the language spec to the right.
  display: flex;
  justify-content: right;
  align-items: flex-start;
  // Use the ::before pseudo-class for the actual language spec.
  &::before {
    // Match the code block style.
    @include font-size("s");
    font-family: $code-font-family;
    color: var(--melange_a_ui);
    background-color: var(--melange_a_float);
    // Give the text some surrounding space.
    padding: var(--space-3xs) var(--space-xs);
    border-radius: var(--space-3xs) var(--space-3xs) 0 0;
    // Offset so it partially overlaps the code block and move it toward the middle a little.
    position: relative;
    top: 1.2rem;
    left: -2rem;
    // This ugly looking things tries to remove the height of the child element,
    // so we get consistent spacing around the code block.
    margin-block-start: calc(var(--flow-space) - (2ch + 2 * var(--space-xs)));
  }

  // For each class insert the corresponding language text.
  &.python::before {
    content: "python";
  }
  // etc.
}
```

The reason I used a pseudo-class for the language description instead of directly like `<div class="lang">viml</div>`html is to hide it from other reading modes, such as the RSS feed, Firefox's reader view or screen readers.
These readers typically remove most or all of the styling, but then the language description would appear in the middle of nowhere without context.
Using a pseudo-class is a bit annoying as I need to insert some CSS for every single language I want to display, but it provides a better experience for people using these readers.

I also changed the font to my [custom Iosevka][iosevka] for the website as well.
Now I'm happy with how things look.

[restyled the blog]: /blog/2023/10/04/giving_the_blog_a_facelift/
[iosevka]: /iosevka
