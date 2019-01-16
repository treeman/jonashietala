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
    * Responsive projects
    * Responsive homepage
    * Homepage styling
    * Post content styling
    * Multiple post styling

    * Properly design html semantics, see 
        https://gsnedders.html5.org/outliner/ 
        https://internetingishard.com/html-and-css/semantic-html/
    * figcapture in markdown?
    * aside in markdown?
    * Style lists
    * Style tables
    * Nicely designed site: https://www.blaenkdenum.com/posts/extra-dependencies-in-hakyll/
    * Header
    * Footer

* Convert to woff2 https://stackoverflow.com/questions/22958700/woff2-conversion-from-standard-woff

* Write posts about some good read books/games etc
* Remove links to images offpage!
* Remove spaces from tag links e.g "Yearly Review"

