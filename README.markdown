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

Blog script:
1. Perl libs: DateTime

To sync with S3:
1. s3cmd  
   Change preferences in script and set keys ins `~/.s3cfg`, see s3cmd.
2. Perl libs: Modern::Perl
3. python-magic (for MIME type)

To write about
==============


TODO
====


