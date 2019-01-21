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

1. cabal: hakyll, missingH
2. python-magic (for MIME type)
3. s3cmd  
   If you're using `sync` to upload to S3 bucket.
   Change preferences in script and set keys ins `~/.s3cfg`, see s3cmd.

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
        style tags
    * Post content styling
        Some youtube videos aren't showing up?          DONE
        Cleanup old header manual styling               DONE
        Tag links are missing
        Style lists
        Style tables
        Style footnotes
        Style math symbols
    * Multiple post styling
    * Properly design html semantics, see 
        https://gsnedders.html5.org/outliner/ 
        https://internetingishard.com/html-and-css/semantic-html/
    * Locate game downloads or remove download links

* Convert to woff2 https://stackoverflow.com/questions/22958700/woff2-conversion-from-standard-woff

* Write posts about some good read books/games etc
* Remove links to images offpage!
* Remove spaces from tag links e.g "Yearly Review"

