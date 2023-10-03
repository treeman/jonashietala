---
title: "Giving the blog a facelift"
tags: [Tag1, Tag2]
---

When I [rewrote the blog in Rust][rust] I tried not to touch any of the styling, but some things annoyed me:

1. It wasn't pretty (even ugly in some parts).
2. No dark mode support.
3. Some elements were broken, for instance images or code blocks overlapping the header when it was floating to the side.
4. I wanted to promote my series and projects more.

# A better landing page

![The old landing page was too simple for my taste.](/images/site_restyle/new_homepage.png)

My thinking when [I last updated the style][last] was to keep things as simple as possible.
I think I mostly succeeded with that, but I really don't think the landing page was pretty.

So this time I wanted to:

1. Make it prettier
1. Spotlight ports, series and projects

As it happens I had remade the [archive][], [projects][] and [series][] pages already, and it just fell out well to simply include some partial contents and shove them into the homepage.

::: Flex
/images/facelift/landing_dark.png
/images/facelift/landing_light.png
:::

I'm sure you could make a bunch of improvements, but the design fulfills my goals at least.


# Automatic dark mode

I've been using dark mode when coding for ages, but it's only recently I tried to switch everything to dark by default.

Some sites use a dark mode switcher button, but I think it's better if the sites detect your preference automatically.
It's surprisingly easy in CSS, and here's a small example:

```scss
:root {
  --color-text: black;
}
@media (prefers-color-scheme: dark) {
  :root {
    --color-text: white;
  }
}

// Then you just use the variables like so:
.element {
   color: var(--color-text);
}
```

To test this out, in Firefox (and I'm sure other browsers) you can change your preference and the site will update immediately:

![You can change dark mode preference in Firefox.](/images/facelift/firefox_settings.png)

::: Flex
/images/facelift/dark.png
/images/facelift/light.png
:::

I prefer the dark variant, but the light is also quite nice.

# New colorscheme

I recently fell in love with the colorscheme [melange-nvim][] for Neovim.
This was a **big deal** for me so naturally I want to use the colorscheme *everywhere*.

::: Flex
/images/facelift/qr_dark.png
/images/facelift/qr_light.png
:::

This doesn't just apply to the code highlighting, but to the colorscheme of the entire blog itself.
Go big or go home.

# Systematic CSS

People love to hate on CSS, and maybe I'm damaged somewhere but I quite like CSS.
Things have come a *long* way since I styled my first webpage.
Back then you had to do all types of weird incantations even to do simple things, and IE 6 still found a way to mess it up.

While there are browser incompatibilities today, it's not nearly as bad and the browsers today support a lot of really nice features that makes modern CSS quite neat.

Here are some things I utilized for this restyle:

## Fluid size scales

Sizing fonts is more annoying than one might think.
One thing you'd like to do is to have a slightly larger font for larger screens, and smaller sizes for smaller screens (such as your phone).
It makes it easier to read on both devices, but it's a bit annoying to setup.

Previously I used media queries, but a neater solution is fluid size scales.
I just used some generator I found online and it seems to work well:

```scss
// From https://www.fluid-type-scale.com/
:root {
  --font-size-2xs: clamp(0.51rem, 0.03vw + 0.5rem, 0.53rem);
  --font-size-xs: clamp(0.64rem, 0.12vw + 0.61rem, 0.7rem);
  --font-size-s: clamp(0.8rem, 0.25vw + 0.74rem, 0.94rem);
  --font-size-m: clamp(1rem, 0.45vw + 0.89rem, 1.25rem);
  --font-size-l: clamp(1.25rem, 0.76vw + 1.06rem, 1.67rem);
  --font-size-xl: clamp(1.56rem, 1.2vw + 1.26rem, 2.22rem);
  --font-size-2xl: clamp(1.95rem, 1.83vw + 1.5rem, 2.96rem);
}
```

Now it's not perfect, and on my monitor I feel the jump from `m` to `s` is a bit too big, but this is good enough (for now).

I also utilize a fluid scale for spacing:

```scss
// From https://utopia.fyi/
:root {
  --space-3xs: clamp(0.31rem, calc(0.31rem + 0vw), 0.31rem);
  --space-2xs: clamp(0.56rem, calc(0.54rem + 0.11vw), 0.63rem);
  --space-xs: clamp(0.88rem, calc(0.85rem + 0.11vw), 0.94rem);
  --space-s: clamp(1.13rem, calc(1.08rem + 0.22vw), 1.25rem);
  --space-m: clamp(1.69rem, calc(1.62rem + 0.33vw), 1.88rem);
  --space-l: clamp(2.25rem, calc(2.16rem + 0.43vw), 2.5rem);
  --space-xl: clamp(3.38rem, calc(3.24rem + 0.65vw), 3.75rem);
  --space-2xl: clamp(4.5rem, calc(4.33rem + 0.87vw), 5rem);
}
```

## Lobotomized owl

It's always fun to find weird things that are also very useful.
The [lobotimized owl][owl-selector] selector is just that.

```scss
// Hoot-hoot
     * + *
```

For a great explanation see [My favourite 3 lines of CSS][stack-flow],
but in short it enables you to set consistent spacing between child elements.

It's super useful, and I use this mixin for it:

```scss
@mixin flow($space: false) {
  & > * + * {
    margin-block-start: var(--flow-space);
    @if $space {
      --flow-space: $space;
    }
  }
}
```

Why didn't I use the [stack + flow][stack-flow] as described in the linked blog post?
Honestly, I had forgot about it and only searched for the post now as I'm writing this.
Their way is probably better, but I can't be bothered to refactor it right now. 🤷


## Measure to limit text width

A big readability tip is to [limit the line length to 45--90 characters][line-length].
I do this using `max-inline-size` and a variable to control the length that's on by default:

```scss
:root {
  // Measure limits the width of text to be more readable.
  --measure: 60ch;
}
* {
  // Limit everything to --measure by default.
  max-inline-size: var(--measure);
}
// Opt-out instead of opt-in, makes it a bit more manageable honestly.
html,
body,
div,
header,
nav,
main,
footer {
  max-inline-size: none;
}
```

## CSS Grid is awesome

I haven't used [grid][] that much previously, but it's one of these newer features that makes CSS a pleasure rather than a pain to work with.
You can accomplish things that were previously hard to do with just some commands.
Here are some examples of how I use grid:

### Image gallery: 3-column layout

![A 3-column layout with grid.](/images/facelift/gallery.png)

```scss
figure.gallery {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  grid-gap: var(--space-3xs);
}
```

### Tags: variable number of columns

If you want to fit as many columns as possible, you can use this setup that I use on the [tags page][]:

```scss
.tags {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(12em, 1fr));
}
```

### Post: overflow images and code blocks

Constraining the width of text is good, but I wanted to allow images and code blocks to extend past that a little if needed. See this image:

![Most content exists between grid lines 3 and 4, but the image may span between lines 2 and 5, essentially overflowing the article width.](/images/facelift/grid_article.png)

In my previous design I used media queries and relative offsets to accomplish it, but with grid it's easier and fluent.
With this setup you can also allow an image to extend the entire screen ("full bleed"), but so far I haven't find a use for it.

The main `article` includes this wrapper:

```scss
@mixin full-bleed-wrapper {
  display: grid;
  grid-template-columns:
    1fr
    minmax(0, var(--overflow-size))
    min(var(--measure), 100%)
    minmax(0, var(--overflow-size))
    1fr;
  max-inline-size: none;

  // Place all content in the main content area by default
  & > * {
    grid-column: 3;
  }
}
```

This sets up 5 areas: the main middle content, constrained by `--measure`, the overflow with `--overflow-size` and the rest.
On narrow screens everything except the content collapses to zero width.

Then you override `grid-column` for the items you want to overflow:

```scss
// For overflowing the article.
@mixin overflow-bleed {
  width: 100%;
  grid-column: 2 / 5 !important;
  // This resets the --measure constrain I have on by default.
  max-inline-size: none;
}
// For items that should span the entire screen.
@mixin full-bleed {
  width: 100%;
  grid-column: 1 / -1 !important;
  max-inline-size: none;
}
```

Mixins like these are why I prefer [sass][] over raw CSS, it makes creating these systems more practical and maintainable.

[sass]: https://sass-lang.com/
[rust]: /blog/2022/08/29/rewriting_my_blog_in_rust_for_fun_and_profit/
[melange-nvim]: https://github.com/savq/melange-nvim
[last]: /blog/2019/01/25/site_restyle_and_update/
[archive]: /archive
[series]: /series
[projects]: /projects
[owl-selector]: https://alistapart.com/article/axiomatic-css-and-lobotomized-owls/
[grid]: https://css-tricks.com/snippets/css/complete-guide-grid/
[tags page]: /blog/tags
[stack-flow]: https://andy-bell.co.uk/my-favourite-3-lines-of-css/
[every-layout]: https://every-layout.dev/
[line-length]: https://practicaltypography.com/line-length.html
