#  Blog ideas

- "Uses" page (hardware, software etc): https://rusingh.com/uses/
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

- Hash tags aren't added by sync script?
- Broken link detection breaks if they're inside code blocks

- Deploy to S3 via GitHub actions
    Need to verify cargo test first

- T-34 series
    DONE /series/t-34
         /blog/tags/t-34 should include a note to redirect to the series page
    DONE Each post should include a series notice
    DONE Each post should also include a note to redirect to the current t-34 blog post
    DONE /blog/current-t-34  detail all

- Handle create/moves in watch command

- ARIA for accesibility https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA

- Neovim plugin for interactive editing + updating + scrolling
  Like this: https://github.com/iamcco/markdown-preview.nvim

- Styling
    Prev/Next in posts
    Include prog lang in codeblocks

- Markdown parsing things
    - Auto TOC

- Git commit per post

- Prev/Next in monthly archives

- Nice to have but not prio
    - Tests for archive generation
    - Tests for watch functionality

- Unsupported markdown features:
    - `|` inside a table breaks the table! (see t-34)
      We have `<code>\|</code>` workarounds

Things to update:
    - Initial toc looks bad (RTS, reinstalling slackware, game design, ...)

