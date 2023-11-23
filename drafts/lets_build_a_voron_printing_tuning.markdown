---
title: "Let's build a VORON: Printing & Tuning"
tags: ["3D printing", "VORON"]
series: voron_trident
---

It's up and running, and now it's finally printing time!
Less exciting---but necessary---tuning the printer to make the prints pretty.

# Filament shipment

Even though I've been building the printer for more than a month, I wasn't ready for it being time for printing so soon.
I don't know what filament is good and what I need, so I decided to get a few different brands and colors to try out:

- [3DJAKE niceABS Black]
- [eSUN ABS+ Purple]
- Polymaker PolyTerra PLA [Army Dark Green] and [Army Blue]
- [3DJAKE ecoPLA White & Black]
- [Spectrum PLA Dragon Red]
- [eSUN eSilk PLA Gold]

![I think I went overboard with the filament order.](/images/trident/filament.jpg)

# Secondary printer tuning

At this point the next step in the VORON docs is making a print.
But I had various issues, so I went through the [Secondary printer tuning][] before getting my first successful print.

## Gantry racking & squaring

I've already done the gantry racking, yay!

## Belt tension

I always wondered how tight the belts were supposed to be.
Tuning it by measuring the frequency using an app was pretty nifty, and it was quite painless.

![It's around 110Hz, I think?](/images/trident/belt_frequencies.png)

## Bed mesh

```
[bed_mesh]
speed: 300
mesh_min: 40, 40
mesh_max: 210, 210
fade_start: 0.6
fade_end: 10.0
probe_count: 5,5
algorithm: bicubic
```

And then `BED_MESH_CALIBRATE` will do it's job:

![The bed mesh is situated around 0, seems good.](/images/trident/bed_mesh.png)

I also wanted to use [Klipper Adaptive Meshing & Purging][] (KAMP).

## Input shaping

The guide references input shaping, which is included in the LDO kit.
But [NERO 3D][] recommended to wait with input shaping until you've printed with it a bit, so I've held off on it until I've done some more printing.

# First print

The VORON docs [makes the first print seem so simple][voron-print]---just upload the print and eat a bowl of cereal.
And I was looking forward to just hitting print and watching it go brr...
But of course things wouldn't go so smoothly.

Problems I've had include but isn't limited to:

1. Print stopped with `Hotend not enough`

   This happens because the Tap G-code reduces the hotend temperature to 150° when probing to not damage the bed, but then the temperature is too low for printing.

   This made me implement [a better print_start macro][].

1. Filament didn't load.

   And KlipperScreen complains about `FILAMENT_LOAD` not existing. Sigh.

1. First layer not sticking to the bed.

1. After a few failed attempts, the filament clogs.

   ![The filament clogged somehow, and I had to disassemble the toolhead to fix it.](/images/trident/clogged.jpg)

1. Bunch of "Unknown command" errors in the mainsail log.

   I assume it's because Klipper wasn't selected in the "G-code flavor" in SuperSlicer.

1. The print coming loose from the bed after a while.

   ![Oh FFS.](/images/trident/fail_print.jpg)

   I struggled with this a fair bit.
   I reconfigured `z_offset` which seemed to work, but maybe it's extruding too much filament causing the toolhead to hit the print?

After all that trouble I was running into I was expecting for getting an absolutely shit print...
But it's actually not that terrible?

::: Flex
/images/trident/first_cube_top.jpg
/images/trident/first_cube_x.jpg
:::

It's far from perfect, but the lighting is fairly harsh and it looks better in real-life.
My friends have shown many 3D prints that look a lot worse.

# Functional prints

High on adrenaline I set out to do my first functional print: an [exhaust cover][] for the back of the printer.
I'm going to replace it with a proper filter in the future, but I wanted something to cover the big hole in the back when printing ABS, so I wanted to make a temporary in PLA.

But it things can't go that smoothly:

![I messed with the z-offset during print, and the result was this mess.](/images/trident/fail_print_exhaust.jpg)

The printer made some extremely unpleasant sounds, and I was scared that the nozzle was grinding against the bed.
I tried to tweak the z-offset during print, but I the sound didn't stop and I ended up destroying the print.

The nozzle didn't hit the bed and it was the stepper motors being super loud... But more on that in a future post.

When I re-ran the print and stopped messing with it, the printer spit out a functional print:

![Honestly, pretty good quality.](/images/trident/print_exhaust.jpg)

Other things weren't that good:

![A part needed to mount the Nevermore filter to the extrusion.
It's hard to see in this image, but the top edge is drifting upwards quite a bit.](/images/trident/not_great_nevermore_mount.jpg)

Simpler models seem to print well, but it struggles with more complex geometry.
While I can probably use this Nevermore mount with some sanding, this level of quality isn't high enough to for example print parts for a VORON.

More tuning is needed.

# More print tuning

[Ellis' print tuning guide][] seems like the go-to guide for tuning your prints.
It contains a *lot* of info, so I won't write about it too much lest we'll be here all day.

- Go through and tighten everything again
- Tighten nozzle *while hot* (I didn't do this previously!)

- Extruder calibration

  This contains the images I was missing from the VORON docs.
  I redid the calibration but with the top panel removed to get a more accurate measure.

- Wash the build surface (don't mention it in the blog?)

- First layer squish

- Pressure advance

- Extrusion multiplier

- Stringing? (Need to dry the filament?)

- Stepover

# Print comparison

[3DJAKE niceABS Black]: https://www.3djake.com/3djake/niceabs-black?sai=3802
[eSUN ABS+ Purple]: https://www.3djake.com/esun/abs-purple-2?sai=11812
[PLA Army Blue]: https://www.3djake.com/polymaker/polyterra-pla-army-blue?sai=14897
[PLA Army Dark Green]: https://www.3djake.com/polymaker/polyterra-pla-army-dark-green?sai=11933
[3DJAKE ecoPLA White & Black]: https://www.3djake.com/3djake/ecopla-white-black-economy-set?sai=5063
[Spectrum PLA Dragon Red]: https://www.3djake.com/spectrum/pla-dragon-red?sai=3969
[eSUN eSilk PLA Gold]: https://www.3djake.com/esun/esilk-pla-gold?sai=11857
[NERO 3D]: https://www.youtube.com/@Nero3D
[Klipper Adaptive Meshing & Purging]: https://github.com/kyleisah/Klipper-Adaptive-Meshing-Purging
[Secondary printer tuning]: https://docs.vorondesign.com/tuning/secondary_printer_tuning.html
[voron-print]: https://docs.vorondesign.com/build/slicer/first_print.html
[a better print_start macro]: https://github.com/jontek2/A-better-print_start-macro
[exhaust cover]: https://github.com/MotorDynamicsLab/LDOVoron2/blob/main/STLs/exhaust_cover.stl
[Ellis' print tuning guide]: https://ellis3dp.com/Print-Tuning-Guide/
