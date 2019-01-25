---
title: "Regex substitution with unicode in Haskell"
tags: Hakyll, Haskell
---

While [remaking the site][restyle] I noticed [my automatic embedding of bare youtube links][embed] sometimes didn't work. The culprit was unicode in the document which the regex library couldn't handle.

Apparently while this would be supported by default by almost all modern languages it's not the case with Haskell. I tried several regex libraries which either didn't support substitutions or couldn't handle unicode properly.

As usual [others][a1] have [had][a2] this before me. This is the most elegant working solution I could find:

```{.haskell}
import qualified Text.Regex.PCRE.Light as RL
import qualified Text.Regex.PCRE.Heavy as RH

-- Find and replace bare youtube links separated by <p></p>.
youtubeFilter :: String -> String
youtubeFilter txt = RH.gsub rx replace txt
    where
      rx = RL.compile "<p>\\s*https?://www\\.youtube\\.com/watch\\?v=([A-Za-z0-9_-]+)\\s*</p>"
                      [RL.utf8]
      replace = \[g] -> "<div class=\"video-wrapper\">\
                 \<div class=\"video-container\">\
                   \<iframe src=\"//www.youtube.com/embed/" ++ g ++ "\" frameborder=\"0\" allowfullscreen/>\
                 \</div>\
              \</div>";
```

Which is to say it's not very elegant but it does get the job done.

[restyle]: /blog/2019/01/25/site_restyle_and_update/ "Site restyle and update"
[embed]: /blog/2014/09/01/embedding_youtube_videos_with_hakyll/ "Embedding youtube videos with Hakyll"
[a1]: https://stackoverflow.com/questions/45067622/how-to-find-and-replace-unicode-chars-in-haskell
[a2]: https://stackoverflow.com/questions/3847475/haskell-regex-substitution

