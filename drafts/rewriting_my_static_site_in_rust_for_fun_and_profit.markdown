---
title: "Rewriting my blog in Rust for fun and profit"
tags: [Rust, Webpage, Hakyll, Haskell]
---



# Background

I've used [Hakyll] as my static site generator for around 9 years now. Before that I think I used [Jekyll] and also more dynamic pages with [Mojolicious] in Perl and [Kohana] in PHP, but I can't be completely sure as the git history doesn't go back that far.

But all good things come to an end and I've now migrated to my own custom site generator written in Rust.

## Problems with my previous setup

These are the main annoyances I wanted to solve with this rewrite:

1. It was starting to get slow

   On my crappy laptop a full site rebuild took 75 seconds. I only have 240 posts so I don't think it should be that slow. While there is a good caching system and a watch command that only updates the changed post during writing, it was still noticeable.

2. Several external dependencies

   While the site generator itself is written in Haskell, there were other dependencies apart from a number of Haskell libs. My blog helper script was written in Perl, I used [sassc] to convert sass, [pygments] in Python for syntax highlighting, and [s3cmd] for uploading the generated site to S3.

   It's annoying to install all these and to keep them up-to-date, it would be great to just have a single thing to worry about.

3. Setup problems

   Related to the previous point, sometimes things just break and I have to spend time to debug and fix them. This is especially frustrating when I have some good idea of something to write about, just to find out that I have some issue with my site generator.

   You might think, what could possibly go wrong?

   Well, sometimes I update some packages that might break things in weird ways. For example:

   - After gch is updated it can no longer find cabal packages.

   - When I run the Haskell binary I get this:

     ```
     [ERROR] Prelude.read: no parse
     ```

     (Only on my desktop, it works fine on my laptop.)

   - Or this Perl error:

     ```
     Magic.c: loadable library and perl binaries are mismatched (got handshake key 0xcd00080, needed 0xeb00080)
     ```

     (Only on my laptop, it works fine on my desktop.)

   - Default Hakyll Pandoc arguments changed between versions, breaking code rendering in my Atom feed.

   I know that these are all solvable, but I just want something that just works™.

4. Mental overhead with Haskell

   I kind of like Haskell---especially the purely functional parts of it. And I am a fan of the declarative approach Hakyll takes to site configuration. Take the generation of static (i.e. standalone pages) for instance:

   ```haskell
   match "static/*.markdown" $ do
       route   staticRoute
       compile $ pandocCompiler streams
           >>= loadAndApplyTemplate "templates/static.html" siteCtx
           >>= loadAndApplyTemplate "templates/site.html" siteCtx
           >>= deIndexUrls
   ```

   Even if you don't understand the `$`haskell and `>>=`haskell, I still think it's clear that we're finding files from the `static/` folder, sending them to `pandocCompiler` (to convert from markdown), to some templates and then de-indexing urls (to avoid links ending with `index.html`).

   Simple and clear!

   But I haven't used Haskell in years, and the overhead for me to add slightly more complex things to the site is quite large.

   For example, I wanted to add next/prev links to posts, but then I had to spend some time relearning Haskell and Hakyll. And even then the solution I came up with was super slow because it was a linear search to find the next/previous posts, but I couldn't figure out how to do this properly with the way Hakyll is setup.

   I'm sure you can do it in an efficient manner, but for me it was too much effort for such a small feature for me to bother.

[pygments]: https://pygments.org/ "Python syntax highlighter"
[s3cmd]: https://github.com/s3tools/s3cmd "S3cmd tool for Amazon Simple Storage Service (S3)"

## Why Rust?

1. I enjoy using Rust, and it's important for a hobby project like this.
2. Rust is quite performant and transforming text is a task it should be good at.
3. Cargo is fire and forget. As long as you have Rust installed, it should be enough to just do `cargo build` and the generator should build.

## Why reinvent the wheel again?

I wanted to write my own static site generator as a fun and interesting project. It shouldn't be *that* hard and it would give me complete control over the site, which should give me a bit more flexibility than if I used an existing site generator.

(Yes I know that projects like [cobalt] allows you to make any kind of transformation, using whatever language you want, to your pages.)


# Implementation details

## Delegate the hard things

At first I was worried that it would be hard to replicate all the features I enjoyed with Hakyll, like the templating engine, the syntax highlighting for numerous languages or the `watch` command that automatically regenerates any pages you edit and acts as a file server so I can view the post in the browser during the writing process.

Turns out that it's not that hard if you let existing crates handle the hard parts. While it's out of scope to describe how I used everything, here are some of the libraries I used to good effect:

- [tera][] for a templating engine.

  It's more powerful than what Hakyll provides, as it can do things like loop:


  ```html
  <div class="post-footer">
    <nav class="tag-links">
        Posted in {% for tag in tags %}{% if loop.index0 > 0 %}, {% endif %}<a href="{{ tag.href }}">{{ tag.name }}</a>{% endfor %}.
    </nav>
  </div>
  ```

- [pulldown-cmark][] for parsing Markdown.

  It's really for [CommonMark][] which is a standard, unambiguous syntax specification for Markdown.

  While fast, it doesn't support as many things as [Pandoc] does, so I had to extend it with some more features. More on that later.

- [syntect][] for syntax highlighting, supporting Sublime Text's grammars.
- [yaml-front-matter][] to parse metadata from posts.
- [grass][] as a Sass compiler purely in Rust.
- [axum][] to create [a static file server][static-site-server] used to host the site locally.
- [hotwatch][] to watch for file changes, so we can update pages whenever a file is updated.
- [scraper][] to parse the generated html. Used in some of my tests and for some specific transformation.

Even with these libraries the Rust source files themselves contained over 4000 lines. In some cases Rust can be verbose, and my code is really not beautiful, but I did make some enhancements as well.

## Markdown transformations

While it would be easy if my posts were just standard markdown, over the years I've included various features and extensions that my markdown parsers of choice [pulldown_cmark] don't support. So I had to code them myself.

### Preprocessing

I have a preprocessing step that I use to create figures with multiple images. It's a generalized processing step that takes the form:

    ::: <type>
    <content>
    :::

I use `Flex`, `Figure` and `Gallery` for different kinds of image collections. For instance this:

    ::: Flex
    /images/img1.png
    /images/img2.png
    /images/img3.png

    Figcaption goes here
    :::

Would be transformed to:

```html
<figure class="flex-33">
<img src="/images/img1.png" />
<img src="/images/img2.png" />
<img src="/images/img3.png" />
<figcaption>Figcaption goes here</figcaption>
</figure>
```

And how did I implement this magic? Using Regex of course!

```rust
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use std::borrow::Cow;

lazy_static! {
    static ref BLOCK: Regex = Regex::new(
        r#"(?xsm)
        ^
        # Opening :::
        :{3}
        \s+
        # Parsing id type
        (?P<id>\w+)
        \s*
        $

        # Content inside
        (?P<content>.+?)

        # Ending :::
        ^:::$
        "#
    )
    .unwrap();
}

pub fn parse_fenced_blocks(s: &str) -> Cow<str> {
    BLOCK.replace_all(s, |caps: &Captures| -> String {
        parse_block(
            caps.name("id").unwrap().as_str(),
            caps.name("content").unwrap().as_str(),
        )
    })
}

fn parse_block(id: &str, content: &str) -> String {
    ...
}
```

(The image and figure parsing is a lot more verbose, so let's skip that shall we?)

[embed_youtube]: /blog/2014/09/01/embedding_youtube_videos_with_hakyll/ "Embedding youtube videos with Hakyll"

### Extending pulldown_cmark

- [Embed bare YouTube links][embed_youtube].

  It would've been much easier to make this a pre- or post-processing step, but it's the first transformation I made so I didn't make the most optimal decision. I can't be bothered to change it though.

- Demote and auto-generate ids for headers.

  Convert this:

  ```md
  # My header
  ```

  To this:

  ```html
  <h2 id="my-header">My header</h2>
  ```

  Historically I've reserved `h1` tags for post titles. While it's not really necessary, I wanted to try to replicate the appearance of the Hakyll site as much as possible. (This later went out the window though.)

- Convert standalone images to figures and allow attributes for images.

  For instance this:

  ```md
  ![](/images/img1.png){ height=100 }
  ```

  Would be transformed into this:

  ```html
  <figure>
    <img src="/images/img1.png" height="100">
  </figure>
  ```

  Super useful!

- Allow attributes for quotes, with custom transformations.

  Once upon a time I had the ambition of using sidenotes in my posts. They looked like this:

  ```md
  <aside>
  My *super* special note
  </aside>
  ```

  The problem is that CommonMark does not parse markdown inside html tags, so `*super*` would not be emphasized.

  At first my plan was to do this with the generalized preprocessing step, but then I'd lose track of link references. So in this example:

  ```md
  ::: Aside
  My [link][link_ref]
  :::

  [link_ref]: /some/path
  ```

  `link` would not be turned into a link as we would only parse inside `:::`.

  So instead I added attributes to blockquotes, like so:

  ```md
  > Some text
  { :notice }
  ```

  Which would call a `notice` parser, which in this case would create a `<aside>`html tag instead of a `<blockquote>`html tag, while preserving the parsed markdown.


- Allow attributes for tables.

  The use-case is to add classes to tables for some unique formatting rules. The implementation was a big hack, and I only used this feature in a single of my posts, but at least I solved the problem...

- Highlight code using [syntect].

  Instead of wrapping with a `<span>`html tag I wrap it a `<code>`html tag, and I also support inline highlighting. For example "`let x = 2;`rust" is produced by

  ```md
  `let x = 2;`rust
  ```

While they're all different, the principle of them all is the same. Take a look at [the code][] if you want to see how they're all implemented.

## Local watcher

## Performance improvements

So how's the performance?

I'm pleased to say that the site is fully rebuilt in around 1.7 seconds on my crappy laptop, which is a 40x speedup over my previous solution. This is quite good as I only did some small performance tweaks.

The first thing is that if you use [syntect], you really should [compress `SyntaxSet`rust to a binary format][syntect-compress]. This saved me around 5 seconds of raw startup time.

The other thing was to parallelize rendering using [rayon][]. Rendering is the markdown parsing, applying templates and creating the output file. Rayon is great is this task is limited by CPU and it's very easy to use (if the code is structured correctly). This is for instance a simplified view of the rendering:

```rust
fn render(&self) -> Result<()> {
    let mut items = Vec::new();

    // Add posts, archives, and all other files that should be generated here.
    for post in &self.content.posts {
        items.push(post.as_ref());
    }

    // Render all items.
    items
        .iter()
        .try_for_each(|item| self.render_item(*item))
}
```

To parallelize this all we need to do is change `iter()`rust into `par_iter()`rust:

```rust
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

items
    .par_iter() // This line
    .try_for_each(|item| self.render_item(*item))
```

And that's it!

Admittedly, my performance improvements are quite minor, and the big performance gains comes from the libraries I use. For instance my old site used an external [pygments] process written in Python for syntax highlighting, while I now have a highlighter in Rust that's *much* faster, and can easily be parallized.

[syntect-compress]: https://docs.rs/syntect/latest/syntect/dumps/index.html

## Cargo "just works"

## Tests and panics


# Ending thoughts


[Mojolicious]: https://mojolicious.org/ "Perl real-time web framework"
[Kohana]: https://kohanaframework.org/ "The Switft PHP Framework"
[Jekyll]: https://jekyllrb.com/ "Simple, blog-aware, static sites"
[tera]: https://tera.netlify.app/ "A powerful, easy to use template engine for Rust"
[CommonMark]: https://commonmark.org/ "A strongly defined, highly compatible specification of Markdown"
[static-site-server]: https://github.com/tokio-rs/axum/tree/main/examples/static-file-server
[grass]: https://crates.io/crates/grass "A near-feature-complete Sass compiler written purely in Rust"
[yaml-front-matter]: https://crates.io/crates/yaml-front-matter "YAML Front Matter (YFM) parser for Markdown files "
[zola]: https://www.getzola.org/ "Your one-stop static site engine"
[cobalt]: https://cobalt-org.github.io/docs/ "cobalt.rs site generator"
[scraper]: https://crates.io/crates/scraper "HTML parsing and querying with CSS selectors"
[hotwatch]: https://crates.io/crates/hotwatch "A Rust library for conveniently watching and handling file changes"
[pulldown-cmark]: https://crates.io/crates/pulldown-cmark "A pull parser for CommonMark"
[syntect]: https://crates.io/crates/syntect "library for high quality syntax highlighting and code intelligence using Sublime Text's grammars"
[axum]: https://crates.io/crates/axum "Web framework that focuses on ergonomics and modularity"
[sassc]: https://github.com/sass/sassc "libsass command line driver"
[100-rust]: https://kerkour.com/rust-static-site-generator "Building a static site generator in 100 lines of Rust"
[rayon]: https://docs.rs/rayon/latest/rayon/ "Data-parallelism library that makes it easy to convert sequential computations into parallel"
