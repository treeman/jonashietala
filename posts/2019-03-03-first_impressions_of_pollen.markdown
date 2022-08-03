---
title: "First impressions of Pollen"
tags: Programming, Pollen, Racket
---

After having consumed [Practical Typography][] I took a look at the library he wrote to create books for the web called [Pollen][] written in [Racket][]. I've only used it a little but I feel I can give my first impressions of it.


# What I like

The first thing I really like is Pollen enables powerful text transformation using X-expressions. They're like regular lisp S-expressions which models your document as a tree where transformations are easy.

For example this expression:

```racket
`(blockquote
  "Everyday life is like programming, I guess. If you love something you can put beauty into it."
  (footer (span ((class "author"))
                "Donald Knuth")))
```

Corresponds to this html:

```html
<blockquote>
  Everyday life is like programming, I guess. If you love something you can put beauty into it.
  <footer>
    <span class="author">Donald Knuth</span>
  </footer>
</blockquote>
```

Complex transformations really should be done with structured data, like X-expressions or ASTs in compilers, and not on raw html. It's easier and less error prone.

Secondly I actually like Pollen's markup. It is more verbose than markdown---or others---but not unbearably so:

```pollen
◊h1{Pollen markup}

A link to ◊link[my-site]{my site}.

◊ul{
  ◊li{First item}
  ◊li{Second item}
  ◊li{Third item}
}

◊(define my-site "http://jonashietala.se")
```

```markdown
# Markdown markup

A link to [my site][my-site].

* First item
* Second item
* Third item

[my-site]: http://jonashietala.se
```

What it gives you is flexibility to freely define your tags however you want.

See for example how I handled [embedding youtube links][yt] on this site. I basically run a regex on the html to convert bare links (surrounded by a paragraph!):

```
https://www.youtube.com/watch?v=NIbr-mLi4DU
```

To

```html
<div class="video-wrapper">
  <div class="video-container">
    <iframe src="//www.youtube.com/embed/NIbr-mLi4DU" frameborder="0" allowfullscreen></iframe>
  </div>
</div>
```

In Pollen you simply define a tag that returns the video-wrapper directly, so the markup is simple without magic:

```pollen
◊yt{https://www.youtube.com/watch?v=NIbr-mLi4DU}
```

Of course you *could* do string replacements as well if you want to, like if you want to convert "\-\-\-" to "—" or "\.\.\." to "…". But Pollen allows you to leave that to when it makes sense.

Another consequence is you're not limited to the features supported by your chosen markup. For instance if you want to add a footer in a quote with author and source links in markdown you need to drop down to raw html.

An example that's bothered me quite a bit is sidenotes. Markdown doesn't support sidenotes. Neither does pandoc extended markdown, it only support footnotes but that's not what I want. So my choice is to either create pandoc extensions myself or settle for raw html. With pollen you have full power to implement them in exactly the form you want.

The drawback here is you have to implement the functionality yourself.

[yt]: /blog/2014/09/01/embedding_youtube_videos_with_hakyll/


# What I don't like

Other static site generators allow you to route input files to output files. For example this site has post files like:

    posts/2019-03-01-home_office_renovation.markdown

Which are routed to:

    blog/2019/03/01/home_office_renovation/index.html

To generate pretty urls. You're also not limited to one output file or even an input file, you're in full control.

Pollen however has a one-to-one mapping of input and output files. So I cannot have a `posts` directory with all my posts so I can list them in any easy manner but I would have to replicate the folder structure if I want to have any control over the output urls. This is bothersome.

Another annoyance is output files are generated in the same directory as your input files. So while Hakyll collects outputs in a `_site` directory, making it very easy to deploy a site, Pollen pollutes the base directory:

```
site.html.pm    <- input file in Pollen markup
site.html       <- generated output file
```

This probably bothers me much more than it should. I want order and cleanliness.

Finally Pollen doesn't handle dependencies very well. For example to update the output file I need to update the exact input file, even if the input file uses data from other files. Which it will do in for example table of content files.

I had some trouble integrating sass for my css files. I used a system command which combines all .scss files to a single .css output file. I managed to get Pollen to do the generation for me but I couldn't get it to track dependencies properly (so if I change any .scss file I want the .css to always regenerate).

In the end I just threw together a simple script using `inotifywait` which calls sassc when a .scss file change:

```bash
#!/bin/bash

sassc sass/main.scss --style compressed > css/main.css
echo "created: css/main.css"

inotifywait -e close_write,moved_to,create -m sass/ |
while read -r directory events filename; do
    sassc sass/main.scss --style compressed > css/main.css
    echo "updated: css/main.css"
done
```

This touches on my final annoyance with Pollen. Pollen only regenerates files when you ask for them. So you need to switch to the browser, refresh and Pollen only then starts creating your file. This usually takes a second or two but that's long enough to be very annoying.

Instead I want all my files to regenerate as soon as I save the file. This is how `inotifywait` and to my knowledge all good static site generators including Hakyll work.

Where Pollen makes the choice power over simplicity with it's markup language it makes the opposite trade-off with the file handling. I would've liked it better if it chose power in both cases.


# Should you use it?

The drawbacks I see are all related to file management and the benefit is the powerful markup.

If you're happy with the features markdown provides there are plenty of better alternatives. There are dirt simple site generators if you just want a site with minimal effort. Then there are libraries like Hakyll where you need to program a bit but you get more power.

But if you require a more powerful markup language than what the alternatives like pandoc flavored markdown (which has a number of extensions compared to regular markdown) and you're willing to put in work to make it the way you want it then Pollen is a good choice.

If I were to write a book---which is Pollen's reason of existance---I would probably choose Pollen. Maybe I'd try to improve the file handling or maybe learn to live with it.

Will I migrate this blog to Pollen? While I would love the markup I'm not sure the effort is worth it. I would absolutely have to improve the file handling before I even consider it which probably means I won't be bothered.

[Pollen]: https://docs.racket-lang.org/pollen/ "Pollen site generator"
[Practical Typography]: https://practicaltypography.com/ "Practical Typography"
[Racket]: https://racket-lang.org/ "Racket programming language"

