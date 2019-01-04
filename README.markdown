Personal Weblog
===============

My site <http://www.jonashietala.se> written in [Hakyll][].

[Hakyll]: http://jaspervdj.be/hakyll/

Sitemap
=======

    /
    /projects
    /recommendations
    /blog
    /blog/tags/:tag
    /blog/:year/:month/:day/:title
    /archive
    /contact
    /404
    /feed.xml
    /masters_thesis

Dependencies
============

1. cabal: hakyll, missingH, hakyll-sass
2. python-magic (for MIME type)
3. s3cmd  
   If you're using `sync` to upload to S3 bucket.
   Change preferences in script and set keys ins `~/.s3cfg`, see s3cmd.

TODO
====

* Restyle
    * Explore fonts
        Century Supra + Concourse https://practicaltypography.com/century-supra.html $239
        Source Code Pro, for code  free
        Fira Sans, https://bboxtype.com/typefaces/FiraSans/#!layout=specimen free
        IBM Plex, https://www.fontsquirrel.com/fonts/ibm-plex
        Convert to woff2 https://stackoverflow.com/questions/22958700/woff2-conversion-from-standard-woff
    * Read <https://internetingishard.com/html-and-css/>, also has a very nicely designed site!
    * Properly design html semantics, see https://gsnedders.html5.org/outliner/ https://internetingishard.com/html-and-css/semantic-html/
    * figcapture in markdown?
    * aside in markdown?
    * Style lists
    * Style quotes
    * Padding around headings
    * Nicely designed site: https://www.blaenkdenum.com/posts/extra-dependencies-in-hakyll/

* SASS for Hakyll?

* Write posts about some good read books/games etc
* Remove links to images offpage!
* Check style for markdown tables.
* Remove spaces from tag links e.g "Yearly Review"

