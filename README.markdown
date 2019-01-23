Personal Weblog
===============

My site <http://www.jonashietala.se> written in [Hakyll][].

[Hakyll]: http://jaspervdj.be/hakyll/

Sitemap
=======

    /
    /projects
    /blog
    /blog/tags/:tag
    /blog/:year/:month/:day/:title
    /feed.xml
    /archive
    /contact
    /404
    /masters_thesis

Dependencies
============

1. cabal: hakyll, missingH, hakyll-sass, pcre-utils
2. python-magic (for MIME type)
3. s3cmd  
   If you're using `sync` to upload to S3 bucket.
   Change preferences in script and set keys ins `~/.s3cfg`, see s3cmd.
4. sassc

TODO
====

* Restyle
    * Responsive header         DONE
    * Header & footer styling   DONE
    * Static page styling       DONE
    * Archives styling          DONE
    * Responsive projects       DONE
    * Responsive homepage       DONE
    * Homepage styling
        smaller screens wraps header badly              DONE
        h1, h2 spacing. Also for static pages           DONE
        cleanup homepage.scss                           DONE
        wrap long code lines inside body text           DONE
        wrap images inside body text                    DONE
        style tags                                      DONE
    * Post content styling
        Some youtube videos aren't showing up?          DONE
        Cleanup old header manual styling               DONE
        Tag links are missing                           DONE
        Style tags                                      DONE
        Style lists                                     DONE
        Style footnotes                                 DONE
        Style tables                                    DONE
        Style math symbols                              SKIP
        Style figures
    * Better images
        https://pandoc.org/MANUAL.html#images
        Use figures instead of manual centering of images
        Image gallery
        Update existing posts
    * Multiple post styling
    * Properly design html semantics, see 
        https://gsnedders.html5.org/outliner/ 
        https://internetingishard.com/html-and-css/semantic-html/
    * Locate game downloads or remove download links

* Remove spaces from tag links e.g "Yearly Review"      DONE
* Convert to woff2 https://stackoverflow.com/questions/22958700/woff2-conversion-from-standard-woff
* Fix broken links
    * Images offpage
    * Broken links to sites                             DONE
    * Game download links (or remove)
    * Internal links


