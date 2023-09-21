---
title: "Rewriting my Neovim config in Lua"
tags: [Tag1, Tag2]
---

I've got tons of things to do; clean the bathrooms, prototype an idea for a SaaS and ponder world peace.
So naturally the procrastination took over and I rewrote my Neovim configuration in Lua.

![](/images/rewrite_neovim_lua/cool_kids_lua.jpg)


# But why?

No, but seriously, I've had my Neovim configuration since before Neovim existed and it's been working fine.
Why would I then rewrite it, and using Lua to boot?

My config has collected a lot of cruft over the years, and it was about time to do a cleanup.
So I figured the best way was to do it all from scratch, only adding tings from the old config if I really needed them (and if I knew what they were doing there!).

I was also curious on how well it would work to configure Neovim using Lua instead of Vimscript, and if there were some new fancy things I was missing.


# Just a few plugins or many?

As is common knowledge, there are these great divides in the developer community.
Issues so great that it makes the Israeli-Palestinian conflict feel like kids fighting in the sandbox.

I'm of course talking about the age-old questions of Vim vs Emacs, if you should use an IDE or a Text Editor and *gasp* if you should use tabs or spaces.

And even inside the Vim community we have our own great question:
should you amass as many plugins as possible, pimping out your Vim so it can be mistaken for an IDE, 
or should you keep it minimalistic, staying true to the Raw Vim experience?

So where do I stand in this?
On which hill will I die on?

As I'm writing this I'm up to 77 plugins, with a bunch more on my "plugins to check" list, so I guess I'm dying on the "*use all the plugins*" hill.

If that makes you feel like I've betrayed Bram the creator, then my only defense is this:

I tried, but I failed. They all seemed so useful you know? I try not to be dogmatic either way; if it's useful I'll try it, otherwise I'll skip it.


# Where to start?

The inspiration for this was ThePrimeagen's video on [0 to LSP: Neovim RC From Scratch][primeagen] that I watched several months ago.

While ThePrimeagen's video was great as inspiration (look how easy it is!) I instead referenced other existing configs:

- [LazyVim][] is a popular Neovim distro that you can use if you don't want to configure everything from scratch or---the horror---anything at all.

  A distro is absolutely not my thing, but the [LazyVim][] source was quite useful as a reference for me.

- There are lots of people who have their configs on GitHub.
  I used [catgoose][] as the config was really clean, including lots of plugins to inspire me.

- Speaking of plugins, [neovimcraft][] is a good site to find plugins.

- [dotfyle][] is another great site with plugins, configurations and the fantastic [This Week in Neovim][] newsletter.

[catgoose]: https://github.com/catgoose/nvim
[primeagen]: https://www.youtube.com/watch?v=w7i4amO_zaE
[primeagen_init]: https://github.com/ThePrimeagen/init.lua
[neovimcraft]: https://neovimcraft.com/
[LazyVim]: https://github.com/LazyVim/LazyVim/tree/main
[dotfyle]: https://dotfyle.com/
[This week in Neovim]: https://dotfyle.com/this-week-in-neovim
[lazy.nvim]: https://github.com/folke/lazy.nvim "lazy.nvim is a modern plugin manager for Neovim."


# Structured configuration

One of the big problems with my previous setup was that `init.vim` was huge and it was hard to organize it. I tried using folds and having a logical grouping, but it never felt good.

With lua and [lazy.nvim][] you can organize settings and plugins in different files quite nicely. This is how I did it:

I wanted to have a split of `config/` and `plugins`, so `~/.config/nvim/init.lua` just loads `lua/config/init.lua`:

```lua
require("config")
```

Then in `~/.config/nvim/lua/config/init.lua` I load the configurations and plugin manager:

```lua
-- Setup <leader> and <localleader> before loading plugins
require("config.leader")

-- Use lazy.nvim for plugin management
require("config.lazy")

-- Continue with the configuration, possibly overriding settings that some
-- plugins might set.
require("config.options")
require("config.colorscheme")
require("config.keymaps").init()
require("config.commands")
```

With [lazy.nvim][] you can split up plugin specifications into separate files:

```lua
-- Basic lazy.nvim setup as copied from the readme
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Load plugin specs from lua/plugins/*.lua
require("lazy").setup("plugins", {
  defaults = {
    lazy = true, -- Default to lazy loading, optional
  },
  change_detection = {
    notify = false, -- I find the config changed notification super annoying
  },
})
```

This will automatically load any `.lua` file in the `lua/plugins` folder. For example this is `lua/plugins/replacer.lua`:

```lua
return {
  "gabrielpoca/replacer.nvim",
  opts = { rename_files = false },
  keys = require("config.keymaps").replacer,
}
```

Incredibly nice when you have lots of plugins, and some have large configurations (like lsp, treesitter or cmp).

One last big thing is I wanted to have all global keymaps in one single file. [lazy.nvim][] supports adding keymaps in the plugin specification using `keys = { }`lua option. I accomplished this by simply returning a "module" table from `config/keymaps.lua`:

```lua
M = {}

M.replacer = {
  {
    "<leader>rq",
    function()
      require("replacer").run()
    end,
    silent = true,
    desc = "Make quickfix editable for replacing in",
  },
}

-- And other plugin keymaps like this
M.trouble = {
  ...
}

return M
```

Not all keymaps can be defined using the [lazy.nvim][] specification, in which case I returned a function and called that where applicable. For instance for LSP:

```lua
M.buf_lsp = function(_, buffer)
  local map = vim.keymap.set
  map("n", "<localleader>D", vim.lsp.buf.declaration, { silent = true, buffer = buffer, desc = "Declaration" })
  map("n", "<localleader>d", vim.lsp.buf.definition, { silent = true, buffer = buffer, desc = "Definition" })
  -- etc
end
```

And set it up in `plugins/lspconfig.lua` in the `on_attach` callback:

```lua
local on_attach = function(client, buffer)
  keymaps.buf_lsp(client, buffer) -- Will add the keymaps when LSP attaches to the buffer
  lsp_status.on_attach(client)
end
```

One last thing; for the regular mappings you don't want to just remap them in `config/keymaps.lua` because multiple files will run `require("config.keymaps")`lua, so I wrapped it in an init function:

```lua
M.init = function()
  local map = vim.keymap.set
  map("n", "<leader>p", '"*p', { silent = true, desc = "Paste from mouse" })
  map("n", "<leader>P", '"*P', { silent = true, desc = "Paste before from mouse" })
  -- etc
end
```

Which is why `config/init.lua` looked a bit weird:

```lua
require("config.options")
require("config.colorscheme")
require("config.keymaps").init() -- Notice the weird init() call here
require("config.commands")
```

It's true that `keymaps.lua` has grown quite large and isn't super easy to read. Maybe I'll break it out into more keymap files (`config/keymaps/lsp.lua` etc) in the future, but right now I think it's nice to have all the maps in one single file.


# Favorite new-to-me plugins

While doing the rewrite I went through my existing plugins to see if they were still relevant, or if I could replace or just remove them. And of course, if I could add some new ones.

I won't create a list of my favorite plugins, as it would be boring to see yet another list with LSP, treesitter and cmp, I'll instead highlight some good plugins that were new to me:

- [lazy.nvim][]: A modern plugin manager

  ![Observe the glory of a modern package manager](/images/rewrite_neovim_lua/lazy.png)

  I've been using [vim-plug][] as my plugin manager since forever. It's been working well, but I figured I should try one of these newer managers to see if I was missing something. And boy, did I. [lazy.nvim][] has some really nice features that I now can't live without:

  - Split plugin specifications in separate files as I wrote about [earlier in the post](#structured-configuration).
  - Specify plugin dependencies.
  - Pin plugin versions.
  - Track plugin versions in `lazy-lock.json`, so you can identify what plugin version breaks your setup so you can pin it to a known good version.
  - Lazy loading. It's nice to have for those rarely used but heavy plugins.

- [melange-nvim][]: An amazing colorscheme

  ![It's just pleasing to my eyes.](/images/rewrite_neovim_lua/melange2.png)

  ![The design idea is that control flow should use warm colors and data should use cold colors.
  So here, functions are warm yellow and strings and data are colder purple and green.](/images/rewrite_neovim_lua/melange3.png)

  I've been a gruvbox user since I started using Vim almost 15 years ago. I've tried tons of different colorschemes but nothing has ever come close to gruvbox for me. Most of the popular colorschemes are just too blue, or have too little (or too much) contrast or I just don't like them for some random reason.

  I had almost resigned to becoming a gruvbox-lifer, unable to ever change colorscheme.

  And then, [melange-nvim][] appeared. And it's glorious!
  Now after many long years, I've finally switched to a new colorscheme, and I'm content.

  (No, switching from the original [gruvbox][] to [gruvbox.nvim][] to [gruvbox-material][] and back again doesn't count.)

  Also a shout-out to [kanagawa][] (dragon, the dark variant without blue background) which is the only colorscheme I've found that I didn't eject in horror after 5 minutes, except for gruvbox and melange.

  ![Sorry [kanagawa][]. It's not you, it's me.](/images/rewrite_neovim_lua/kanagawa.png)

- [vim-cool][]: Turn off search highlight

  Forget `<C-l>`, this plugin will turn off search highlight when you move your cursor away from it.
  Simple, but raises the quality of life a *lot*.

- [conform][]: Format on save

  I've used [neoformat][] before, but had some issues where I saved then quickly stared modifying something, but then formating kicked in and removed my changes.

  With [conform][] I haven't had these issues, and configuration was quick and easy and LSP fallback is super sweet. Excellent plugin.

- [nvim-treesitter-textobjects][]

  A common misconception about treesitter is that it only adds semantics to syntax highlighting:

  ![BASE and REPEAT are enum members, while the other keycodes are defined as macros.](/images/rewrite_neovim_lua/base_layout.png)

  I think that's very nice, but treesitter is more than that. And a great example of that is [nvim-treesitter-textobjects][] where you can operate on treesitter nodes. I have for example:

  - `]f` jump to next function.
  - `]c` jump to next class.
  - `]g` jump to next function or class.
  - `<leader>s` swap next parameter.
  - `if` textobject for inner function. So `cif` would delete the function body and enter insert mode.
  - `ax` textobject for outer comment, to easily delete/change comments.

  The beauty is that these work on treesitter nodes, so they work equally well across languages for functions like `fn myfun() { }`rust, `function myfun() ... end`lua or a `def myfun() do ... end`elixir. (Given that the treesitter implementation supports these options. Markdown doesn't have the concept of a function for instance.)

- [neogit][]: Git management

  [magit][] is widely regarded as the best Git client/integration there is.
  When I used Emacs for work (oh yes, the horrors) I did use [magit][] and yes it was great (although too slow on Windows).

  I don't think I need to say more than that [Neogit][] is [magit][], but for Neovim, and it is also great.
  (It doesn't have feature parity with [magit][] yet, but it's good enough to have replaced [fugitive][] for me.)

- [alpha-nvim][]: Dashboard

  ![My startup screen](/images/rewrite_neovim_lua/alpha.png)

  I'm going to be honest here: I've always thought that a dashboard was unnecessary fluff and people who used them were just pimping Neovim for the sake of pimping.

  And now, I also use a dashboard.

  While it's something I could absolutely live without, by now I'd rather have it than not.

  Okay, saving a keypress by using `f` instead `<leader>f` isn't a big deal.
  And yeah, showing [lazy.nvim][] stats is probably unnecessary.

  But having a list of keymaps that I should internalize is actually really nice.
  It helps remind me of these cool new keymaps I've added, so they don't get forgotten for years until my next config rewrite.

  Yes, the list of keymaps is currently just hardcoded in the dashboard config.
  But I would like to implement keymap tracking and use it for spaced repetition.
  Maybe even have a floating window on startup that you need to clear every day, similar to "flashcards".
  This is a nice idea for a plugin that I may or may not write in the future...

- [flash.nvim][]: Navigational plugin

  Once upon a time there was [vim-sneak][] that added the motion `s` to jump to any location by specifying two characters.
  It was great, and it spawned a slew of other similar plugins that expanded on the idea.
  (I'm not sure this is completely historically accurate, but it felt like a good narrative.)

  The plugin I've landed on is [flash.nvim][].
  What I really like about it is the idea of typing as many characters as possible.
  Even with sneak I didn't always arrive where I wanted, so having the option of typing out more things felt good for me.

  Extra functionality such as doing a "remote yank" (search, yank something, then cursor goes back to where it was) and enhancing `f`/`F`/`t`/`T`/`/` is just gravy.

- [nvim-colorizer][]: Colorizer plugin

  ![What are these colors again? It's the palette of [melange-nvim][] of course!](/images/rewrite_neovim_lua/colorizer.png)

  After [vim-hexokinase][] got archived I had a minor panic.
  It was a neat little plugin that highlighted colors inline, but without altering the highlight of the colors themselves, which is what all the other colorizer plugins seemed to do.

  Luckily I found [nvim-colorizer][] that does the same, and without having to build an external binary like hexokinase that always managed to break for some weird reason.

  I love when things just work.


# Is lua worth it?


[vim-plug]: https://github.com/junegunn/vim-plug
[melange-nvim]: https://github.com/savq/melange-nvim
[kanagawa]: https://github.com/rebelot/kanagawa.nvim
[conform]: https://github.com/stevearc/conform.nvim
[nvim-treesitter-textobjects]: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
[neogit]: https://github.com/NeogitOrg/neogit
[vim-cool]: https://github.com/romainl/vim-cool
[alpha-nvim]: https://github.com/goolord/alpha-nvim
[neoformat]: https://github.com/sbdchd/neoformat
[gruvbox]: https://github.com/morhetz/gruvbox
[gruvbox-material]: https://github.com/sainnhe/gruvbox-material
[gruvbox.nvim]: https://github.com/ellisonleao/gruvbox.nvim
[magit]: https://magit.vc/
[fugitive]: https://github.com/tpope/vim-fugitive
[flash.nvim]: https://github.com/folke/flash.nvim
[vim-sneak]: https://github.com/justinmk/vim-sneak
[nvim-colorizer]: https://github.com/NvChad/nvim-colorizer.lua
[vim-hexokinase]: https://github.com/RRethy/vim-hexokinase
