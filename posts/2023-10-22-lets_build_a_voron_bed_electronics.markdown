---
title: "Let's build a VORON: Bed & electronics"
tags: ["3D printing", "VORON"]
series: voron_trident
---

I've made a lot of progress on my VORON.
Electronics and other stuff are installed to the degree that I've begun wiring, but I'm going to separate the wiring into it's own post.

This will be a short post about bed preparation and installing electronics components.

# Feedback from the VORON forum

I've gotten [some feedback][] from the friendly VORON forum on some issues I should take a look at:

1. Verify that the frame is built square (I've tried to do so).
2. The front Z motor mounts were swapped.

   ![Front Z motor mounts now have the VORON logo at the front.](/images/trident/rotated_front_pieces.jpg)
3. I should [check the gantry racking][rack] to get rid of it catching (I haven't done that yet, I want to finish up the wiring first).

[some feedback]: https://forum.vorondesign.com/threads/ldo-trident-250-complete-3d-printer-beginner.1089/
[rack]: https://www.youtube.com/watch?v=cOn6u9kXvy0&embeds_referring_euri=https%3A%2F%2Fforum.vorondesign.com%2F&source_ve_path=OTY3MTQ&feature=emb_imp_woyt

# Misaligned extrusion

When I was going to insert the top cover I noticed that the Z motor didn't fit into the cutout, and I discovered that the center extrusion wasn't right in the middle:

::: Flex
/images/trident/plate_dosent_fit.jpg
/images/trident/screw_is_misaligned.jpg
:::

I have a small memory that I measured it, and the spacing was off by a little, but then maybe I forgot to do something about it?
Just goes to show how my plans of taking the build slowly and trying to be thorough doesn't prevent me from doing silly mistakes.

# The buildplate

Even though the assembly manual says that you should install the buildplate at this point, I decided to skip it.
I got the tip from Nero3D to only install it at the very end, even after wiring, because it's heavy and makes the printer more annoying to flip around during the build.

![These screws were extremely hard to screw down... Maybe I need better tools?](/images/trident/annoying_screws.jpg)

Other than the small screws murdering my fingers, installing the final extrusions was straightforward.

::: Flex
/images/trident/t_extrusions.jpg
/images/trident/buildplate.jpg
:::

![Bed mount installed.](/images/trident/mount_installed.jpg)

# Electronics

And now the part of the build I've been most worried about begins; avoiding blowing up the electronic components.

Well, that requires wiring so maybe I'll blow it up in the next post.

::: Flex
/images/trident/rails.jpg
/images/trident/jumpers.jpg
:::

I couldn't find the [LDO Beefy Raspberry Pi Mount](https://github.com/MotorDynamicsLab/LDOVoron2/blob/main/STLs/beefy_raspberry_bracket.stl) in my printed parts.
But why would I need it, the standard parts work well?

![The big electronic components are laid out.](/images/trident/elecronics_placed.jpg)

Again, laying out the components were no big deal.
Embarrassingly, the most trouble I had was locating the microSD slot for the Raspberry Pi (it's underneath).

Next up is the dreaded wiring.
