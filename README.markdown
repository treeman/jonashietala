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
    /archive
    /masters_thesis
    /404
    /feed.xml

Dependencies
============

1. Haskell libs: hakyll, missingH, hakyll-sass, pcre-heavy
2. sassc

To sync with S3:
1. s3cmd  
   Change preferences in script and set keys ins `~/.s3cfg`, see s3cmd.
2. Perl libs: Modern::Perl
3. python-magic (for MIME type)

To write about
==============

* Site update
* Youtube parsing with Haskell unicode regex library
* Yearly review

TODO
====

* Center footer when it wraps on small screens
* Hide header nav on small screens?

* Restyle
    * Responsive header         DONE
    * Header & footer styling   DONE
    * Static page styling       DONE
    * Archives styling          DONE
    * Responsive projects       DONE
    * Responsive homepage       DONE
    * Homepage styling          DONE
        smaller screens wraps header badly              DONE
        h1, h2 spacing. Also for static pages           DONE
        cleanup homepage.scss                           DONE
        wrap long code lines inside body text           DONE
        wrap images inside body text                    DONE
        style tags                                      DONE
    * Post content styling                      DONE
        Some youtube videos aren't showing up?          DONE
        Cleanup old header manual styling               DONE
        Tag links are missing                           DONE
        Style tags                                      DONE
        Style lists                                     DONE
        Style footnotes                                 DONE
        Style tables                                    DONE
        Style math symbols                              SKIP
        Style figures                                   DONE
    * Better images                             DONE
        https://pandoc.org/MANUAL.html#images
        Use figures instead of manual centering         DONE
        Image gallery                                   DONE
        Update existing posts                           DONE
    * Multiple post styling                             DONE
    * Properly design html semantics, see               DONE
    * Locate game downloads or remove download links    DONE
    * Unicode stopped working                           DONE
* Remove spaces from tag links e.g "Yearly Review"      DONE
* Convert to woff2                                      DONE

