---
title: Uses
---

This page lists the tools I use as a software developer, both in a professional and personal context.

For more lists like this, check out <https://github.com/wesbos/awesome-uses>.


# Smart wearable

While tools are important to get stuff done, something even more important is taking care of yourself.
I've tried various fitness bands, smartwatches, Oura rings and heart rate monitors but the [WHOOP][] has been way more useful than the rest.

![My WHOOP. The strap is surprisingly comfortable.](/images/uses/whoop.jpg)

It helps me in a few different ways:


1. It helps me prioritize sleep

   Honestly, I believe it's the most important thing to get right if you want to perform.
   Be it in sports or as a developer.

1. It helps me find factors that correlates with a good or bad recovery.

   The journal feature is low-key the best thing about the [WHOOP][].
   It asks you to log a set of behaviors and then correlates them with your recovery.

   For example it has given me a large positive recovery for using a sleep mask and a large negative for drinking alcohol.
   Not a big surprise perhaps, but it has also given me a negative score for sauna and a positive score for viewing my phone in bed before I go to sleep.

   Of course, you shouldn't blindly trust this, but I like how it can help you make changes to your routine.
   If you know that drinking alcohol or eating late will reduce your recovery the upcoming day,
   you may not be that willing to go through with it.

1. It helps me not to overreach and other days push me to train harder.

   Especially important if I've been sick or down.

3. I can always wear it, so it's more accurate than any alternative for me.

   I wear it when I sleep, shower, lift weights and with [WHOOP's boxers][boxer] I don't even notice it during grappling training, where you can't have anything on your wrists or your fingers.

Prioritize your health and fitness rather than rewriting your [Neovim][] configuration yet again, it'll be much more worth it.

# Development

![This is from my main monitor with lots of screen space.](/images/uses/neovim_wide.png)

![With the text more readable for the blog.](/images/uses/neovim.png)

I do all my development, writing and note taking inside [Neovim][].
I've been using Vim (and later Neovim when I wanted async code) since more than 15 years ago, and I don't see myself ever switching away.
I have quite a few plugins and tweaks, here's a list of my favorite ones:

- [lazy.nvim] as the plugin manager.
- [telescope.nvim] for fuzzy finding all the things.
- [nvim-cmp] as a completion engine.
- [conform.nvim] to format files on save.
- [nvim-treesitter] for better syntax highlighting and [nvim-treesitter-textobjects][] for jumping between parts of the code.
- [neogit] and [gitsigns.nvim] general git handling.
- [neorg] to organize my personal knowledge base.
- [flash.nvim] for more predictable and faster jumping/searching.
- Basic LSP setup with [nvim-lspconfig], [mason.nvim] and [mason-lspconfig.nvim].

See [my config files][neovim-config] for more.

The colorscheme I use is [melange-nvim] that I try to adapt outside of [Neovim][] as well.
If you find something you love you just have to have it everywhere.

My typeface of choice is a [customized Iosevka][iosevka].
I love the general look of it and the character variants where you can customize individual characters is just gravy.
But the absolutely best feature is how narrow it is while still being legible to me.
This allows me to use four splits on my main monitor instead of three, giving me a huge value add compared to "just" looking nice.

![Four splits on my main monitor. It's nice.](/images/uses/neovim_wide2.png)

I don't use a desktop environment and I rely on [xmonad][] as my tiling window manager to spam [alacritty][] terminals powered by [fish][] or [firefox][] instances all over the place.
I sometimes use [neovide][] as a [Neovim][] GUI, as it has nicer scaling and some cursor effects that makes it a little easier to follow the cursor when it's moving around windows a lot.

As for the OS, I run [Void Linux][] on all my machines at home ([Arch Linux][] on the [Steam Deck][] though).

# Computer hardware

![This is where I work most of the time.  
I may have cleaned it up a bit before taking this picture.](/images/uses/workstation.jpg)

One thing that never ceases to amaze me is the amount of software developers that always work on a laptop.
I get that it's necessary if you travel a lot, or if you work at an office and want to use the same computer when you work at home, but if you don't then a laptop is just so limiting to me.

That's why I've invested in a really good desktop as my main workhorse.

(I also have a laptop that I use from time to time, but when it's time for some serious work there's nothing better than a dedicated desktop.)

## Desktop

The heart of the computer is the Ryzen 9 7950X, a beast of a CPU that's cooled by open loop liquid cooling.
Most of the other things aren't *that* noteworthy. 
There's a big-ass case and a matching PSU, some fast SSDs and an old GPU that I don't have any need to replace.

The next thing I'd like to upgrade are probably my old monitors.
They get the job done buy they're not super great.

Here's a list of the full specs:

- **CPU**: AMD Ryzen 9 7950X
- **GPU**: Radeon RX Vega 56
- **RAM**: Kingston 32GB DDR5 5200MHz CL40 FURY
- **Motherboard**: ASRock B650 PG Lightning
- **Sound card** ASUS XONAR SE (because the motherboard was missing TOSLINK, big fail when purchasing it)
- **Fans**: 8x Noctua NF-A14 PWM 140mm
- **Water cooling**: A 12/16 custom loop with EKWB parts and 140mm radiators
- **Cooling liquid**: Aqua computer Double Protect Ultra
- **PSU**: 750W Seasonic PRIME Platinum, 80 PLUS Platinum
- **SSD**: 2x 500GB Samsung 960 Evo
- **SSD**: Kingston KC3000 M.2 2280 NVMe SSD 2TB
- **Case**: Phanteks Enthoo Primo
- **Monitor**: BenQ EW3270U 31,5"
- **Monitor** 2x DELL U2211H ULTRASHARP 21,5"

## Sound

I've also put more than a reasonable amount of money into the sound system:

- **Speakers**: DALI Opticon 2 MK2
- **Subwoofer**: REL T-Zero MKIII
- **Amplifier**: NAD C368

Yes, they sound great.
But I admit they're overkill for my small office.

## Inputs

![The almighty Ferris](/images/uses/ferris.jpg)

You also need some things to interact with the computer.

The main attraction is the [Ferris][], a minimalistic keyboard with only 34 keys.
Controversial I know, but I don't think you need more keys (except if you're gaming, then a number row is probably more ideal).
All you need is a well-designed layout and a lot of dedication to internalize it.
See my [series about the T-34 keyboard layout](/series/t-34/) for the layout I use.

I use a trackball, the Kensington SlimBlade Trackball, instead of a mouse for the same reason I use a tiny keyboard:
to combat RSI.
It's been working well, no complaints about it.

For voice input I have a Blue Microphone Snowball iCE.
It would be cool to have an awesome podcasting microphone, but that's just overkill.
Maybe if this one dies on me.

# Other tools

- I use [Fastmail][] for emails and my calendar.
- I try to use [Todoist][] to track my tasks and todos, but sometimes I keep them in my [neorg][] personal wiki folder instead.
- For note taking I use [reMarkable 2][].
  I don't use it for anything fancy, it's just nice to sometimes write notes or plan things "on paper". The writing experience really does feel good on it.
- My phone is the [Fairphone 4][].
  It's a great phone and I love being able to super quickly replace the battery.
  But honestly, I should use it less (don't we all?) so I've tried to remove all but the most essential apps.
- A home office.
  Working remotely isn't just good for skipping the commute, but for the ability to focus without disturbance.

[Neovim]: https://neovim.io/
[neovim-config]: https://github.com/treeman/dotfiles/tree/master/.config/nvim
[Ferris]: https://github.com/pierrechevalier83/ferris
[neovide]: https://github.com/neovide/neovide
[alacritty]: https://alacritty.org/
[iosevka]: /iosevka
[fish]: https://fishshell.com/
[xmonad]: https://xmonad.org/
[melange-nvim]: https://github.com/savq/melange-nvim
[firefox]: https://www.mozilla.org/en-US/firefox/new/
[Void Linux]: https://voidlinux.org/
[Arch Linux]: https://archlinux.org/
[Steam Deck]: https://www.steamdeck.com/en/
[WHOOP]: https://join.whoop.com/EBB986
[boxer]: https://shop.whoop.com/en-eu/products/any-wear-boxer/?sku=927-A2-00-0-0
[neorg]: https://github.com/nvim-neorg/neorg
[Fastmail]: https://www.fastmail.com/
[Todoist]: https://todoist.com
[Fairphone 4]: https://www.fairphone.com/en/
[reMarkable 2]: https://remarkable.com/
[lazy.nvim]: https://github.com/folke/lazy.nvim
[conform.nvim]: https://github.com/stevearc/conform.nvim
[nvim-treesitter-textobjects]: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
[nvim-treesitter]: https://github.com/nvim-treesitter/nvim-treesitter 
[nvim-cmp]: https://github.com/hrsh7th/nvim-cmp
[flash.nvim]: https://github.com/folke/flash.nvim
[neogit]: https://github.com/NeogitOrg/neogit
[gitsigns.nvim]: https://github.com/lewis6991/gitsigns.nvim
[telescope.nvim]: https://github.com/nvim-telescope/telescope.nvim
[nvim-lspconfig]: https://github.com/neovim/nvim-lspconfig
[mason.nvim]: https://github.com/williamboman/mason.nvim
[mason-lspconfig.nvim]: https://github.com/williamboman/mason-lspconfig.nvim
