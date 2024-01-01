---
title: "Let's build a VORON: Smaller fixes"
tags: ["3D printing", "VORON"]
series: voron_trident
---

# Tuning

## Bulging corners

One significant print issue I've had is bulging in the corners on overhangs:

![Both edges round upwards and apart from being really ugly, they've caused a number of prints to fail when the nozzle has knocked loose pieces from the bed or has caused belts to skip.](/images/trident/bulging_corners.jpg)

![It's really ugly.](/images/trident/bulging_black.jpg)

Maybe it was overheating? But it happened consistently, with long layer times, with max fans, and for PLA/ABS/ASA.
Another idea was over extrusion, but I think that should've shown up in other places?

Then I found the [Bulging][] section in Ellis' Print Tuning Guide.
These were the changes I made:

- Enable `external perimiter first`
- Set `threshold for bridge flow` to 0
- I also experimented with disabling overhangs specific settings completely (set `threshold for bridge flow` to 0 or the disable checkbox), but I'm uncertain how effectual that was compared to the other two settings.

![These are two prints that show the difference between the settings.
The green is without the changes and white is with the changes.
Apart from the overhangs, the green printout looks pretty good, but the overhangs look terrible.
The white still has some defects, but it's **so** much smoother.
](/images/trident/bulging_nerf.jpg)

## Bed mesh offset in one corner

See Discord pin

# Mods

## Display mount

The display mounts I got from the print-it-forward service wasn't compatible with the screen I got.
Now I have a mount that doesn't require tape to hold it together:

TODO

## LED mounts

There were no instructions on how to install the LEDs, so I taped them on top of the included extrusions covers.
Turns out you were supposed to print a bunch of LED mounts to get the LEDs pointing inwards towards the print for better lighting:

TODO image before

TODO image after

36 <https://github.com/VoronDesign/VoronUsers/blob/master/printer_mods/eddie/LED_Bar_Clip/LED_Bar_Clip_Misumi_version2.stl>

## Spool holder


Issue: too short for some spools.

Possible solutions:

- https://mods.vorondesign.com/detail/wWS3pc510oGqxGo0awsFKA just longer (Pick this one)
- https://mods.vorondesign.com/detail/VjlccbeeOuH5iax4AFHA top, but needs more things
- https://mods.vorondesign.com/detail/x2umK6ZcG6l2c5EEM2LQjQ can mount horizontally
- https://www.printables.com/model/369877-better-voron-spool-holder
- https://www.printables.com/model/227260-voron-24-top-mount-single-dual-spool-holder dual

## Handles

I had to have these spacers to mount the handles included in the kit:

TODO image

2 <https://github.com/MotorDynamicsLab/LDOVoron2/blob/main/STLs/handlebar_spacer_x4.stl>

The kit came with some very sturdy handles, but unfortunately they install over the top panel so you can't remove the panel without removing the handles.
This clashes badly with my plan to make a more easily removable top panel, but fortunately I found 3D printed handles that work.

<https://mods.vorondesign.com/detail/EAM1ZiQJCUzXznvOA767w>

## Bowden Tube Guide

<https://mods.vorondesign.com/detail/8CxQeqS1lXhlGphwkyqh7g>

[Bulging]: https://ellis3dp.com/Print-Tuning-Guide/articles/troubleshooting/bulging.html
