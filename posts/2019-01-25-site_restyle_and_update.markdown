---
title: "Site restyle and update"
tags: ["Webpage", "Web design"]
---

I'm not sure how I got started but I recently read an excellent book about typography: [Butterick's Practical Typography][Practical Typography]. It's free and there's a section [Typography in ten minutes][pt-in-10] if you want the important bits.

He makes a pretty good case for [why it matters][pt-why] when he points out the vast majority of a webpage is text. Despite this it seems focus is on color schemes, images and effects. As I was already leaning towards minimalistic sites over bloated crap I decided to do a makeover of this site after reading the book.


# Problems with my previous site

I was pretty happy with the site I had. But as I looked closer there were issues.

Butterick lists [5 anti-habits][pt-websites] websites do and I managed to tick four out of five ant-habits:

1. Tiny font size for body text
1. Huge size for headings
1. Relied on system fonts like Arial and Georgia
1. Huge block of eye catching color for header and footer

The only one I managed to avoid was having page edges crammed with wads of navigational links.

The site also wasn't responsive so it looked like shit on mobile and had very long lines on bigger screens making it hard to read.


# Text improvements

The major changes were naturally to correct the big anti-habits and other no-nos the book brings up.

::: Flex
/images/site_restyle/old_post.png
/images/site_restyle/new_post.png

Old post style vs new post style
:::

When comparing the old post style with the new we can pick out some noticeable changes:

* The body text is larger to make it easier to read.
* Margins are larger to keep lines at 2-3 alphabets&mdash;an easy way to gauge how long lines should be.
* Headers aren't enormous compared to everything else yet easily identifiable.
* Tighter whitespace

If you look closely enough you can also see the fonts are different. [Century Supra][] is the new body text replacing Georgia and [Concourse][] is used for the date and navigation links. They are both professional fonts made by the author of [Practical Typography][].

> I didn't know this before, and it bothered me, but take a look at the old post. Some of the numbers are off centered, 5 is slightly lower than the others for example. They're numbers in an old style some fonts support. [Century Supra][] supports it as well via a setting which I haven't turned on.
{ :notice }

Now here's the kicker: the fonts cost $239. And still I bought them.

Are the free fonts so bad? Absolutely not and Butterick [agrees][pt-free]. I do like the fonts but the main reason I bought them was to support the book. The book is completely free and instead he's trying to make money by selling fonts. I want to support him and if I get something good in return then great.

[pt-free]: https://practicaltypography.com/free-fonts.html "Practical Typography: Free fonts"


# Responsive layout

I'm not really a web designer so this was my first time trying to make a responsive design. [Flexbox][] combined with some media queries made the process pretty straight forward.

Reading the site on a phone should now resize the font from 22px to 16px&mdash;or something in between&mdash;and re-flow the layout. I tried a multi-column layout for the [homepage](/) and [projects page](/projects) to better utilize space on wider screens which turns into a single-column on smaller screens.

::: Flex
/images/site_restyle/old_homepage.png
/images/site_restyle/new_homepage.png

The old home page vs the new home page
:::

I'm not completely happy with how the tags are laid out but it will do for now. I do think the homepage is an improvement at least.

[Flexbox]: https://css-tricks.com/snippets/css/a-guide-to-flexbox/ "A guide to flexbox"


# Changes to code display

While I was looking to change font for my main text I started looking at programming fonts as well. It's a huge rabbit hole I might write more about in the future.

For the site I changed the default coding font from Consolas —which is proprietary to Windows— to [Hack][]. As a bonus [Hack][] is completely free and hackable.

I use dark background color personally but I'm not a fan of it on websites as they draw too much attention from the body text. The color [I had used][post-gruvbox] from the gruvbox theme was also quite eye catching so I chose a softer one. After updating pandoc I found a couple of new highlighting classes as well.

::: Flex
/images/site_restyle/old_code.png
/images/site_restyle/new_code.png

Old code vs new code
:::

See the [source][code-source] if you're curious.

[code-source]: https://github.com/treeman/jonashietala/blob/master/css/code.scss "Sass markup for code"
[post-gruvbox]: /blog/2015/08/04/gruvbox_syntax_highlighting_for_pandoc/ "Gruvbox syntax highlighting for pandoc"


# Other changes

I did a bunch of other things as well while I was at it. Such as adding game images to the [projects](/projects) page, correcting html5 markup and rewrote the styling in sass.

One annoyance I never bothered to fix was tag links generated links with mixed case and spaces. Now instead of `/blog/tags/Yearly Review` we get `/blog/tags/yearly_review`. It can be accomplished in Hakyll by using a custom route:

```haskell
tagRoute :: Routes
tagRoute = (customRoute $ (map toLower) . (replace " " "_") . toFilePath) `composeRoutes`
           gsubRoute "tags/" (const "blog/tags/") `composeRoutes`
           dropIndexRoute
```

[Century Supra]: https://practicaltypography.com/century-supra.html "Century Supra font"
[Concourse]: https://practicaltypography.com/concourse.html "Concourse font"
[Hack]: https://sourcefoundry.org/hack/ "Hack font"
[pt-websites]: https://practicaltypography.com/websites.html "Practical Typography: websites"
[Practical Typography]: https://practicaltypography.com/ "Practical Typography"
[pt-why]: https://practicaltypography.com/why-typography-matters.html "Practical Typography: Why typography matters"
[pt-in-10]: https://practicaltypography.com/typography-in-ten-minutes.html "Practical Typography: Typography in ten minutes"

