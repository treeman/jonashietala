---
title: "Minor site updates"
tags: Webpage
---

As it happens I got a little tired of the syntax highlighter Pandoc uses so on a whim I started looking at moving to Pygments as my highlighter. With some searching around I found a [blog post][jip] about just that! That's lucky because while I like Haskell, I've never *really* grocked it and I've only dabbled enough with it to (mostly) make things work like I want to.

Often I'll get stuck longer than with any other language just trying to understand WTH is happening. See this snippet for example:

```haskell
responseLength <- read . U8.toString . fromJust <$> (S.lines >=> S.read) is
```

What did `.` do again? And the `<$>` and `>=>` have something to do with monads...

Even though I figured it out after a while, getting stuck on surface level things like this really makes me wonder if it wouldn't be better to just rewrite the site in some other language.

Or put more effort into actually learning Haskell. Maybe the next time...


# Highlighting code via Pygments

The core idea of the Pygments implementation is to reroute all parsing of code elements out to a separate process. For speed reasons it's implemented as a long running process instead of calling out to the shell all the time.

I didn't come up with the approach, but I did some changes to it. One of them was to highlight both code blocks and inline code. The core transform function gets called by overriding the pandoc compiler:

```haskell
pandocCompiler :: Streams -> Compiler (Item String)
pandocCompiler streams = do
  pandocCompilerWithTransformM defaultHakyllReaderOptions
                               defaultHakyllWriterOptions
                               (pygments streams)
```

The transformer simply walks over blocks, matches against code and passes the content to the pygments process in `streams`.

```haskell
pygments :: Streams -> Pandoc -> Compiler Pandoc
pygments streams = walkM (generateCodeBlock streams)

generateCodeBlock :: Streams -> Block -> Compiler Block
generateCodeBlock streams (CodeBlock (_, classes, keyvals) contents) = do
  let lang = unpackLang classes keyvals
  code <- highlightCode streams lang contents

  return $ RawBlock "html" $ pack $ renderHtml $ H.pre $ hCode lang code
```

The thing I added was a clause to walk over inline elements as well:

```haskell
generateCodeBlock streams x = walkM (generateCodeInline streams) x

generateCodeInline :: Streams -> Inline -> Compiler Inline
generateCodeInline streams (Code (_, classes, keyvals) contents) = do
  let lang = unpackLang classes keyvals
  code <- highlightCode streams lang contents

  return $ RawInline "html" $ pack $ renderHtml $ hCode lang code
generateCodeInline _ x = return x
```


# Updated gruvbox syntax highlighting

Pygments use a different markup, so I had to update my gruvbox inspired css scheme. It's not perfect, and the choices are arbitrary, but I think the output is fairly good. If you're interested the stylesheet itself, see the sources for [gruvbox.scss][] and [code.scss][].

[gruvbox.scss]: https://github.com/treeman/jonashietala/blob/master/css/gruvbox.scss
[code.scss]: https://github.com/treeman/jonashietala/blob/master/css/code.scss


# Git commit hash in post footers

Another pretty cool idea I got from the blog post was embedding a git commit hash in the footer of each post. It's probably not particularly useful, but it was a fun idea nonetheless.

![This is how it looks when embedded. Clicking on it takes you to the history of the post file.](/images/commit_footer.png)

The idea is to use `readProcess` and `unsafeCompiler` to launch a git process to retrieve info. Something like this:

```haskell
gitTag :: String -> Context String
gitTag key = field key $ \item -> do
  let fp = (toFilePath $ itemIdentifier item)
      gitLog format =
        readProcess "git" [
          "log"
        , "-1"
        , "HEAD"
        , "--pretty=format:" ++ format
        , "--date=format:%b %e, %Y"
        , fp
        ] ""

  unsafeCompiler $ do
    sha     <- gitLog "%h"
    message <- gitLog "%s"
    date    <- gitLog "%ad"
  ...
```

And add it to post contexts:

```haskell
postCtx tags = mconcat
    [ siteCtx
    , gitTag "git"
    ...
    ]
```

Again, not my idea.


[jip]: https://jip.dev/posts/the-switch-to-hakyll/

