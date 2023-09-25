---
title: "Rewriting my Neovim config in Lua"
tags: [Tag1, Tag2]
---

I've got tons of things to do; clean the bathrooms, prototype an idea for a SaaS and ponder world peace.
So naturally the procrastination took over and I rewrote my Neovim configuration in Lua.

![](/images/cool_kids_lua.jpg)


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

As I'm writing this I'm up to 78 plugins, with ~30 on my "plugins to check" list, so I guess I'm dying on the "*use all the plugins*" hill.

If that makes you feel like I've betrayed Bram the creator, then my only defense is this:

I tried, but I failed.


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


# Structured configuration

`~/.config/nvim/init.lua`:

```lua
require("config")
```

`~/.config/nvim/lua/config/init.lua`:

```lua
require("config.leader")

if vim.g.neovide then
  require("config.neovide")
end

require("config.lazy")

-- Try to set all things after plugins, as some things may possibly be overwritten.
-- `vim.opt.timout` and `ttimeout` had to be set after lazy for instance.
require("config.options")
require("config.colorscheme")
require("config.autocmds")
require("config.keymaps").init()
require("config.commands")
```


# Favorite new to me plugins

- Lazy.nvim
- melange-nvim
- vim-cool
- neogit
- alpha-nvim
- conform
- nvim-treesitter-textobjects

Maybe...

- lualine
- nvim-colorizer


# Is lua worth it?


