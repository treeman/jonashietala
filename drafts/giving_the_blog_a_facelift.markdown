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

# Systematic css

## Fluid size scales

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

## Flow

[Lobotomized owl][owl-selector].

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

## Measure

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

### Overflow in a post

Constraining the width of text is good, but I wanted to allow images and code blocks to extend past that a little if needed. See this image:

![Most content exists between grid lines 3 and 4, but the image may span between lines 2 and 5, essentially overflowing the article width.](/images/facelift/grid_article.png)

In my previous design I used media queries and relative offsets to accomplish it, but with grid it's easier.
With this setup you can also allow an image to extend the entire screen ("full bleed"), but so far I don't use it.

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

This sets up 5 areas: the main middle content, constrained by `--measure`, the nearby overflow and the sides. 

```scss
@mixin full-bleed {
  width: 100%;
  grid-column: 1 / -1 !important;
  max-inline-size: none;
}
@mixin overflow-bleed {
  width: 100%;
  grid-column: 2 / 5 !important;
  max-inline-size: none;
}
```

### Image gallery

```scss
figure.gallery {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  grid-gap: var(--space-3xs);
}
```

### Tags

```scss
.tags {
  @include overflow-bleed;
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(12em, 1fr));
}
```

### Footer

```scss
body > footer {
  display: grid;
  grid-template-columns: 1fr minmax(auto, var(--measure)) 1fr;
  row-gap: var(--space-xs);
  grid-auto-rows: min-content;
}
```

[rust]: /blog/2022/08/29/rewriting_my_blog_in_rust_for_fun_and_profit/
[melange-nvim]: https://github.com/savq/melange-nvim
[last]: /blog/2019/01/25/site_restyle_and_update/
[archive]: /archive
[series]: /series
[projects]: /projects
[owl-selector]: https://blog.logrocket.com/css-lobotomized-owl-selector-modern-guide/
