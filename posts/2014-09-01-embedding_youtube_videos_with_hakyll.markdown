---
layout: post
title: "Embedding youtube videos with Hakyll"
tags: Hakyll, Haskell
---

A while ago a made a timelapse of my entry to Ludum Dare. This is how I parse and embed videos using [Hakyll][].

I don't want to remember any special syntax for embedding, I don't want to bother (remember?) to use something like `{% youtube: <link> %}` or whatever syntax we might find pretty. The simplest solution I could think of is simply to have a bare link separated by newlines:

```
Some text...

<link>

Some other text...
```

Would embed `<link>` as a video. A youtube link seems to always start like: `https://www.youtube.com/watch?v=` which we can parse with a simple regex. If we run this after compilation the link will also be surrounded by `<p>` tags.

```{.haskell}
-- Need to import the regex
import Text.Regex (subRegex, mkRegex)

-- Find and replace bare youtube links separated by <p></p>.
youtubeFilter :: String -> String
youtubeFilter x = subRegex regex x result
  where
    regex = mkRegex "<p>https?://www\\.youtube\\.com/watch\\?v=([A-Za-z0-9_-]+)</p>"
    result = "<div class=\"video-wrapper\">\
                \<div class=\"video-container\">\
                  \<iframe src=\"//www.youtube.com/embed/\\1\" frameborder=\"0\" allowfullscreen/>\
                \</div>\
             \</div>";
```

The rendering part is not pretty and I'm sure one could move it to a template somehow.

We can use with:

```{.haskell}
applyFilter :: (Monad m, Functor f) => (String-> String) -> f String -> m (f String)
applyFilter transformator str = return $ (fmap $ transformator) str
```

And use it right after we compile:

```{.haskell}
match "posts/*.markdown" $ do
    route   postRoute
    compile $ pandocCompiler
        >>= applyFilter youtubeFilter
        >>= saveSnapshot "content"
        >>= return . fmap demoteHeaders
        >>= saveSnapshot "demoted_content"
        >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
        >>= saveSnapshot "post"
        >>= loadAndApplyTemplate "templates/site.html" (postCtx tags)
        >>= deIndexUrls
```

I did not write this code originally, I found it on someone else's blog, but I managed to loose the link and now I can't find it.

The styling of the video is [from another blog post][style]:

```{.css}
.video-container {
    position: relative;
    padding-bottom: 56.25%;
    padding-top: 30px;
    height: 0;
    overflow: hidden;
}

.video-container iframe, .video-container object, .video-container embed {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
}

.video-wrapper {
    width: 600px;
    max-width: 100%;
}
```

Now in the end this:

```
https://www.youtube.com/watch?v=NIbr-mLi4DU
```

Will be transformed to:

https://www.youtube.com/watch?v=NIbr-mLi4DU

The full source can be found [on github][].

[Hakyll]: http://jaspervdj.be/hakyll/ "Hakyll"
[style]: http://webdesignerwall.com/tutorials/css-elastic-videos "CSS elastic videos"
[on github]: https://github.com/treeman/jonashietala "My github"

