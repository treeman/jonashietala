---
title: "Rewriting my blog in Rust for fun and profit"
tags: [Rust, Webpage, Hakyll, Haskell]
---

# Background

I've used [Hakyll] as my static site generator for around 9 years now. Before that I think I used [Jekyll] and also more dynamic pages with [Mojolicious] in Perl and [Kohana] in PHP, but I can't be completely sure as the git history doesn't go back that far.

But all good things come to an end and I've now migrated to my own custom site generator written in Rust.

## Problems with my previous setup

These are the main annoyances I wanted to solve with this rewrite:

1. It was starting to get slow.

   On my crappy laptop a full site rebuild took 75 seconds. (Not compile, just to generate the site.) I only have 240 posts so I don't think it should be that slow. While there is a good caching system and a watch command that only updates the changed post during editing, it was still annoyingly slow.

2. Several external dependencies.

   While the site generator itself is written in Haskell, there were other dependencies apart from a number of Haskell libs. My blog helper script was written in Perl, I used [sassc] to convert sass, [pygments] in Python for syntax highlighting, and [s3cmd] for uploading the generated site to S3.

   It's annoying to install all these and to keep them up-to-date, it would be great to just have a single thing to worry about.

3. Setup problems.

   Related to the previous point, sometimes things just break and I have to spend time to debug and fix them. This is especially frustrating when I have some good idea of something to write about, just to find out that I have some issue with my site generator.

   You might think, what could possibly go wrong?

   Well, sometimes I update some packages that might break things in weird ways. For example:

   - After GHC is updated it [can no longer find cabal packages][ghc-cabal-issue].

   - When I run the Haskell binary on I get this:

     ```
     [ERROR] Prelude.read: no parse
     ```

     (Only on my desktop, it works fine on my laptop.)

   - Or this Perl error:

     ```
     Magic.c: loadable library and perl binaries are mismatched (got handshake key 0xcd00080, needed 0xeb00080)
     ```

     (Only on my laptop, it works fine on my desktop.)

   - When Hakyll changed Pandoc arguments between versions, [breaking code rendering in my Atom feed][hakyll-syntax-issue].

   I know that these are all solvable, but I just want something that just works™.

4. Mental overhead with Haskell.

   I kind of like Haskell---especially the purely functional parts of it, and I am a fan of the declarative approach Hakyll takes to site configuration. Take the generation of static (i.e. standalone pages) for instance:

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
3. Cargo is fire and forget. As long as you have Rust installed, it should be enough to just do `cargo build` and it should just work™.

## Why reinvent the wheel again?

I wanted to write my own static site generator as a fun and interesting project. It shouldn't be *that* hard and it would give me complete control over the site, which should give me a bit more flexibility than if I used an existing site generator.

(Yes I know that projects like [cobalt] allows you to make any kind of transformation, using whatever language you want, to your pages.)


# Implementation details

I'm not going to describe everything I did here, take a look at the [source code][src] if you're interested.

## Delegate the hard things

At first I was worried that it would be hard to replicate all the features I enjoyed with Hakyll, like the templating engine, the syntax highlighting for numerous languages or the `watch` command that automatically regenerates any pages you edit and acts as a file server so I can view the post in the browser during the writing process.

Turns out that it's easy if you let existing crates handle the hard parts. Here are some of the libraries I used to good effect:

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
- [scraper][] to parse the generated html. Used in some of my tests and for some specific transformations.
- [rust-s3][] to upload the generated site to S3 storage.

Even with these libraries the Rust source files themselves contained over 6000 lines. In some cases Rust can be verbose, and my code is really not beautiful, but writing this project was still more work than expected. (Isn't that always the case?)

## Markdown transformations

While it would be easy if my posts were just standard markdown, over the years I've included various features and extensions that my markdown parsers of choice [pulldown_cmark] don't support. So I had to code them myself.

### Preprocessing

I have a preprocessing step that I use to create figures with multiple images. It's a generalized processing step that takes the form:

    ::: <type>
    <content>
    :::

I use this for different kinds of image collections, such as `Flex`, `Figure` and `Gallery`. For example this:

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

(The image and figure parsing is more verbose, so let's skip that shall we?)

[embed_youtube]: /blog/2014/09/01/embedding_youtube_videos_with_hakyll/ "Embedding youtube videos with Hakyll"

### Extending pulldown_cmark

I've also extended [pulldown_cmark] with transformations of my own:

```rust
// Issue a warning during the build process if any markdown link is broken.
let transformed = Parser::new_with_broken_link_callback(s, Options::all(), Some(&mut cb));
// Demote headers (eg h1 -> h2), give them an "id" and an "a" tag.
let transformed = TransformHeaders::new(transformed);
// Convert standalone images to figures.
let transformed = AutoFigures::new(transformed);
// Embed raw youtube links using iframes.
let transformed = EmbedYoutube::new(transformed);
// Syntax highlighting.
let transformed = CodeBlockSyntaxHighlight::new(transformed);
let transformed = InlineCodeSyntaxHighlight::new(transformed);
// Parse `{ :attr }` attributes for blockquotes, to generate asides for instance.
let transformed = QuoteAttrs::new(transformed);
// parse `{ .class }` attributes for tables, to allow styling for tables.
let transformed = TableAttrs::new(transformed);
```

Demoting headers and [embedding bare YouTube links][embed_youtube] is something I used to do before, and were fairly straightforward do implement. (It might have been better to embed YouTube links in a pre- or post-processing step though.)

Pandoc had support for adding attributes and classes to arbitrary elements, which was very useful. So for instance this:

```md
![](/images/img1.png){ height=100 }
```

Would be transformed into this:

```html
<figure>
  <img src="/images/img1.png" height="100">
</figure>
```

I used it all over the place, so I reimplemented it... in less general and hacky manner.

Another feature I used in Pandoc that wasn't supported was evaluating markdown inside html tags. So this would no longer render correctly:

```md
<aside>
My [link][link_ref]
</aside>
```

At first my plan was to do this with the generalized preprocessing step, but then I'd lose track of link references. So in this example:

 ```md
 ::: Aside
 My [link][link_ref]
 :::

 [link_ref]: /some/path
 ```

`link` would not be turned into a link as we would only parse inside `:::`.

```md
> Some text
{ :notice }
```

That would call a `notice` parser, which in this case would create a `<aside>`html tag instead of a `<blockquote>`html tag, while preserving the parsed markdown.

While there are existing crates that adds code highlighting using [syntect], I wrote my own that wraps it in a `<code>`html tag and supports inline code highlighting. For example "Inside row: `let x = 2;`rust" is produced by:

```md
Inside row: `let x = 2;`rust
```

## Performance improvements

I didn't spend that much time into improving the performance, but two things had a significant impact:

The first thing is that if you use [syntect] and have custom syntaxes, you really should [compress `SyntaxSet`rust to a binary format][syntect-compress].

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

Admittedly, my performance improvements are quite minor, and the big performance gains comes from the libraries I use. For instance my old site used an external [pygments] process written in Python for syntax highlighting, while I now have a highlighter in Rust that's *much* faster, and can easily be parallelized.

[syntect-compress]: https://docs.rs/syntect/latest/syntect/dumps/index.html

## Sanity checks

One thing that always bothered me with the site is how easy it was to make a mistake. For example linking to a non-existent page or image, or not defining a link reference at all with `[my link][todo]`, and forgetting to update it before publishing.

So in addition to testing basic functionality like the watch command, I also parse the whole site and check that all internal links exists and are correct (so it validates `some-title` in `/blog/my-post#some-title` too). I also check external links, but that's with a manual command.

During generation I also take a hard stance and `panic` early and often, just to reduce the risk that something weird slips by.

# How did it go?

In the beginning of this post I listed some issues I had with my previous setup, so let's see if I managed to improve them?

1. **Performance**

   On my crappy laptop a full site rebuild (not including compilation time) now takes 4 seconds. An 18x performance improvement is not too shabby I'd say. I'm sure this could be improved further---for example I use [rayon][] for file IO while async would be more beneficial, and I don't have a caching system so I regenerate all files every time I build. (During watching it's smarter though.)

   Please note that this is not to say that Rust this much faster than Haskell, rather a comparison of two implementations. I'm sure someone could make it super fast in Haskell too.

2. **A single dependency**

   Now I've got everything in Rust, with no external scripts or tools I need to install and keep working.

3. **Cargo "just works"**

   As long as I have rust on the system, `cargo build` is just rock solid. I think this is low-key one of Rust's biggest strengths---the build system *just works*.

   You don't have to manually hunt down missing dependencies, sacrifice a child to make it cross-platform or pull your hair out when the build system automagically pulls down an update that breaks everything again. You just lean back and wait for your code to compile.

4. **Rust is simpler?!**

   Umm... Yeah, but also no.

[ghc-cabal-issue]: /blog/2020/05/09/ghc_cannot_find_cabal_packages/
[hakyll-syntax-issue]: https://github.com/jaspervdj/hakyll/issues/662
[src]: https://github.com/treeman/jonashietala

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
[rust-s3]: https://crates.io/crates/rust-s3 "Rust library for working with Amazon S3 and compatible object storage APIs"
[hotwatch]: https://crates.io/crates/hotwatch "A Rust library for conveniently watching and handling file changes"
[pulldown-cmark]: https://crates.io/crates/pulldown-cmark "A pull parser for CommonMark"
[syntect]: https://crates.io/crates/syntect "library for high quality syntax highlighting and code intelligence using Sublime Text's grammars"
[axum]: https://crates.io/crates/axum "Web framework that focuses on ergonomics and modularity"
[sassc]: https://github.com/sass/sassc "libsass command line driver"
[100-rust]: https://kerkour.com/rust-static-site-generator "Building a static site generator in 100 lines of Rust"
[rayon]: https://docs.rs/rayon/latest/rayon/ "Data-parallelism library that makes it easy to convert sequential computations into parallel"

