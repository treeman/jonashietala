#  Blog ideas

- "Uses" page (hardware, software etc): https://rusingh.com/uses/
- Post series: https://fasterthanli.me/series
- Multiple RSS feeds (one for T-34 for instance)
- Styled RSS feeds: https://interconnected.org/home/feed

# Blog rewrite

Sitemap:

    /
    /projects
        /blog
        /blog/:year
        /blog/:year/:month
        /blog/:year/:month/:day/:title
        /blog/tags
        /blog/tags/:tag
        /archive
    /series
    /series/:serie
    /games
        /masters_thesis
        /gpg
        /404
    /feed.xml
        /drafts (only when run with arg)


MVP:
- Fix feed

- Manual deploy script for files not tracked in git
- Deploy to S3 via GitHub actions
    Need to verify cargo test first

- T-34 series
    DONE /series/t-34
         /blog/tags/t-34 should include a note to redirect to the series page
    DONE Each post should include a series notice
         Each post should also include a note to redirect to the current t-34 blog post
         /blog/current-t-34  detail all 
             Should include a note to redirect to the series page

- Handle create/moves in watch command

- ARIA for accesibility https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA

- Neovim plugin for interactive editing + updating + scrolling
  Like this: https://github.com/iamcco/markdown-preview.nvim

- Styling
    Prev/Next in posts
    Include prog lang in codeblocks

- Markdown parsing things
    - Auto TOC

- Check external urls
    https://stackoverflow.com/questions/14154753/how-do-i-make-an-http-request-from-rust
    Important for embedded images, but can be useful for others as well

- Git commit per post

- Post series
- Prev/Next in monthly archives

- Nice to have but not prio
    - Tests for archive generation
    - Tests for draft
    - Tests for watch functionality?

- Unsupported markdown features:
    - `|` inside a table breaks the table! (see t-34)
      We have `<code>\|</code>` workarounds

Things to update:
    - Initial toc looks bad (RTS, reinstalling slackware, game design, ...)

1. Rust rewrite for speed and reproducability
2. Post series
3. Rewrite projects page
4. No drafts in release

Zola static site geneartor: https://github.com/getzola/zola

https://kerkour.com/rust-static-site-generator

# Post ideas

1. How I wrote a book using Pollen

